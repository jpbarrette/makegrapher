import copy
import fcntl
import getopt
import os
import pdb
import popen2
import pprint
import re
import select
import sys

import StringIO

from functionnal_utils import *



def getCommandOutput(command, verbose = False, distinct = False):
    def makeNonBlocking(fd):
        fl = fcntl.fcntl(fd, fcntl.F_GETFL)
        fcntl.fcntl(fd, fcntl.F_SETFL, fl | os.O_NDELAY)

    if verbose:
        print command

    child = popen2.Popen3(command, 1) # capture stdout and stderr from command
    child.tochild.close()             # don't need to talk to child
    outfile = child.fromchild 
    outfd = outfile.fileno()
    errfile = child.childerr
    errfd = errfile.fileno()
    makeNonBlocking(outfd)            # don't deadlock!
    makeNonBlocking(errfd)
    outdata = errdata = complete = ''
    outeof = erreof = 0

    while 1:
	ready = select.select([outfd,errfd],[],[]) # wait for input
	if outfd in ready[0]:
	    outchunk = outfile.read()
            if verbose:
                print outchunk,
	    if outchunk == '': outeof = 1
            if distinct:
                outdata = outdata + outchunk
            else:
                complete += outchunk
	if errfd in ready[0]:
	    errchunk = errfile.read()
            if verbose:
                print errchunk,
	    if errchunk == '': erreof = 1
            if distinct:
                errdata = errdata + errchunk
            else:
                complete += errchunk
	if outeof and erreof: break
	select.select([],[],[],.1) # give a little time for buffers to fill
    if verbose:
        print ""
    err = child.wait()

    if distinct:
        return err, outdata, errdata
    else:
        return err, complete



class MakeTree:
    def __init__(self, 
		 nodes = None, 
		 makefile = None, 
		 invalidateNotTargets = False,
		 preFilterOut = None,
                 runMake = True):
	if preFilterOut is None:
	    preFilterOut = []
	self.preFilterOut = preFilterOut

	self.phonyRE = re.compile("^.PHONY: ")
	self.rTransform = re.compile("([^%]|^)(\%)($|[^%]).*")
	self.phonies = []
	self.patternedTargets = {}
	self.invalidateNotTargets = invalidateNotTargets
	
	if makefile is not None:
	    lines = self.getMakefile(makefile, runMake)
	    self.genMap(lines)
	elif nodes is not None:
	    self.nodes = nodes
	else:
	    self.nodes = {}




    def graphVizExport(self, filename, size = (8,11)):
	"""
	This function will export a graphviz file
	"""
	nodes = self.nodes

	f = open(filename, 'w')
	f.write("digraph G {\nrankdir=LR;")
	f.write("size = \"" + str(size[0]) + "," + str(size[1]) + "\";")
	f.write("ratio = auto;")

	f.write("node [shape = plaintext];\n")

	for target in nodes.keys():
	    f.write("\"" + target + "\" \n")
	f.write(";\n")

	for target in nodes.keys():
	    for dep in nodes[target]:
		f.write("\"" + target +
			"\" -> \"" +
			dep +
			"\"\n")
	f.write("}\n")
	f.close()


    def registerPhony(self, line):
	for target in line.strip().split(" "):
	    if target not in self.phonies:
		self.phonies.append(target)



    def filterMakefileJunk(self, output):
	reservedComments = ["# automatic\n", "# environment\n", "# default\n", "# makefile"]

	define = re.compile("^define ")
	notTarget = re.compile("^# Not a target:$")
	start = re.compile("^# Files$")
	notAppend = re.compile('^\S+.*:=')
	p = re.compile('^\S+.*:[^=]')
	notVariableAssign = re.compile(' = ')

	preFilterOut = []
	for filter in self.preFilterOut:
	    preFilterOut.append(re.compile(filter))

	lines = []

	previousLine = ""
	gotADefine = False
	skipNext = False
	for line in output:
	    print "Processing line: " + line,
	    if skipNext:
		skipNext = False
		continue

	    if line in reservedComments:
		skipNext = True


	    if define.search(line):
		gotADefine = True
	    if line == "endef\n":
		gotADefine = False
	    if gotADefine:
		continue

	    res = self.phonyRE.search(line)
	    if res is not None:
		self.registerPhony(line[res.end():])
		continue

	    if line[0] == "\t":
		continue

	    if line[0] == "#":
		continue

	    if some(lambda s: s.search(line), preFilterOut):
	    	continue

	    result = p.search(line)
	    if result is not None and \
		    notVariableAssign.search(line) is None and \
		    notAppend.search(line) is None:
	        #notTarget.search(previousLine) is None:
		lines.append(line)
	return lines


    def getMakefile(self, filename, runMake):
        if runMake:
	    command = "make -npr -f %s" % (filename)
	    ret = getCommandOutput(command, distinct = True)
	    if ret[0] != 0:
	        raise RuntimeError, ret[2]
            io = StringIO.StringIO(ret[1])
            lines = io.readlines()
        else:
            lines = open(filename).readlines()
	
	for i in range(len(lines)):
	    if lines[i] == "# Implicit Rules\n":
		lines = lines[i + 1:]
		break
	return self.filterMakefileJunk(lines)


    def isPatternedTargets(self, target):
	res = self.rTransform.search(target)
	return res is not None

    def transformPT(self, target):
	res = self.rTransform.search(target)
	return "^" + re.escape(target[:res.start(2)]) + "(.*)" + re.escape(target[res.end(2):]) + "$"

    def normalize(self, dependency, lemma):
	res = self.rTransform.search(dependency)
	if res is not None:
	    return dependency[:res.start(2)] + lemma + dependency[res.end(2):]
	return dependency

    def genMap(self, lines):
	self.nodes = {}

	i = -1
	for line in lines:
	    i += 1
	    parts = line.split(":")
	    if(len(parts) != 2):
		print "Line is not valid"
		print line
	    targets = filter(lambda s: len(s), parts[0].split(" "))
	    for target in targets:
		if self.isPatternedTargets(target):
		    tMap = self.patternedTargets
		    target = self.transformPT(target)
		else:
		    tMap = self.nodes
		if not tMap.has_key(target):
		    tMap[target] = []
		dependencies = parts[1].strip().split(" ")
		for dependency in dependencies:
		    if dependency not in tMap[target] and dependency != "":
			tMap[target].append(dependency.strip())
	self.matchTargets()

    def matchTargets(self):
        rePatternedTargets = {}
        for pattern in self.patternedTargets.keys():
            rePatternedTargets[pattern] = re.compile(pattern)
		
	for target in self.nodes.keys():
	    for pattern in self.patternedTargets.keys():
		reTarget = rePatternedTargets[pattern]
		res = reTarget.search(target)
		if res is not None:
		    print "Found a target: " + str(target) + ", for pattern: " + str(pattern)
		    for dependency in self.patternedTargets[pattern]:
			dep = self.normalize(dependency, res.group(1))
			if dep not in self.nodes[target]:
                            if dep == "example06.cpp":
                                pdb.set_trace()
			    self.nodes[target].append(dep)
    
    
		    

    def filterNodes(self, seedsIn, seedsOut = None):
	targetsMap = copy.copy(self.nodes)

	reIn = []
	for seedIn in seedsIn:
	    reIn.append(re.compile(seedIn))

	reOut = []
	for seedOut in seedsOut:
	    reOut.append(re.compile(seedOut))

	nodes = {}
	for target in targetsMap.keys():
	    if (len(reIn) ==0 or some(lambda r: r.search(target), reIn)) and not some(lambda r: r.search(target), reOut):
		nodes[target] = []

	paths = map(lambda t: [t], nodes.keys())
	while len(paths) != 0:
	    path = paths.pop()
            print "Processing: " + str(path)
	    lastNode = path[-1]
	    if not targetsMap.has_key(lastNode):
		continue


	    deps = targetsMap[lastNode]
            # We empty it, just to be sure not
            # to process it again.
            targetsMap[lastNode] = []

	    if len(deps) == 0:
		continue

	    for dep in deps:
		if nodes.has_key(dep):
		    if dep not in nodes[path[0]]:
			nodes[path[0]].append(dep)

		else:
		    paths.append(path + [dep])
	return MakeTree(nodes = nodes)



if __name__ == "__main__":


    try:
        opts, args = getopt.getopt(sys.argv[1:], "F:to:fT:s:S:", [])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)

    seedsIn = []
    seedsOut = []
    outputFile = "output.dot"
    invalidateNotTargets = False
    preFilterOut = []
    makefile = None
    runMake = True
    
    for o, a in opts:
        if o == "-o":
            outputFile = a
	if o == "-F":
	    preFilterOut.append(a)
        if o == "-f":
            makefile = a
        if o == "-T":
            makefile = a
            runMake = False
        if o == "-s":
            seedsIn.append(a)
        if o == "-S":
            seedsOut.append(a)
	if 0 == "-t":
	    invalidateNotTargets = True

    if makefile is None and not os.path.exists("Makefile"):
        print "You didn't specified any Makefile, and there's no Makefile in the current directory"
        sys.exit(2)


    tree = MakeTree(makefile = makefile, invalidateNotTargets = invalidateNotTargets, preFilterOut = preFilterOut, runMake = runMake)
    filteredTree = tree.filterNodes(seedsIn, seedsOut)
    filteredTree.graphVizExport(outputFile)

    
