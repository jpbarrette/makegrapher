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

verbose = False

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
                 runMake = True,
                 rebuildingNodes = None):
        
        if rebuildingNodes is None:
            rebuildingNodes = []
        self.rebuildingNodes = rebuildingNodes
        
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




    def graphVizExport(self, filename, size = (8,10), sortDotEntries = False):
	"""
	This function will export a graphviz file
	"""
        print "Exporting to GraphViz format..."
	nodes = self.nodes

	f = open(filename, 'w')
	f.write("digraph G {\nrankdir=LR;")
	f.write("size = \"" + str(size[0]) + "," + str(size[1]) + "\";")
	#f.write("page = \"8.5,11\";")
	f.write("ratio = auto;")
	#f.write("rotate = 90;")

	f.write("node [shape = plaintext];\n")

        targets = nodes.keys()
        if sortDotEntries:
            targets.sort()

	for target in targets:
            if target not in self.rebuildingNodes:
                f.write("\"" + target + "\" \n")
	f.write(";\n")


        # rebuilding targets
        if len(self.rebuildingNodes) > 0:
            f.write("node [shape = plaintext, color = red];\n")
            rebuildingTargets = self.rebuildingNodes
            if sortDotEntries:
                rebuildingTargets.sort()
            for target in rebuildingTargets:
                f.write("\"" + target + "\" [fontcolor=red] \n")
                f.write(";\n")
        

	for target in targets:
            deps = nodes[target]
            if sortDotEntries:
                deps.sort()
	    for dep in deps:
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
        print "Filtering Makefile's junk lines..."
        reservedComments = ["# automatic\n", "# environment\n", "# default\n", "# makefile"]

        define = re.compile("^define ")
        notTarget = re.compile("^# Not a target:$")
        start = re.compile("^# Files$")
        notAppend = re.compile('^\S+.*:=')
        p = re.compile('^\S+.*:[^=]')
        notVariableAssign = re.compile(' = ')
        
        inData = False
        dataStart = re.compile("^# Make data base")
        dataEnd = re.compile("^# Finished Make data base")

        preFilterOut = []
        for filter in self.preFilterOut:
            preFilterOut.append(re.compile(filter))

        lines = []

        previousLine = ""
        gotADefine = False
        skipNext = False
        for line in output:
            if skipNext:
                skipNext = False
                continue

            if not inData and dataStart.search(line):
                inData = True

            if inData and dataEnd.search(line):
                inData = False

            if not inData:
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
            print "Running: " + command
            ret = getCommandOutput(command, distinct = True)
            if ret[0] != 0:
                raise RuntimeError, ret[2]
            io = StringIO.StringIO(ret[1])
            lines = io.readlines()
        else:
            lines = open(filename).readlines()
	
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
        print "Initializing graph..."
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
        print "Matching targets..."
        rePatternedTargets = {}
        for pattern in self.patternedTargets.keys():
            rePatternedTargets[pattern] = re.compile(pattern)
		
        for target in self.nodes.keys():
            for pattern in self.patternedTargets.keys():
                reTarget = rePatternedTargets[pattern]
                res = reTarget.search(target)
                if res is not None:
                    for dependency in self.patternedTargets[pattern]:
                        dep = self.normalize(dependency, res.group(1))
                        if dep not in self.nodes[target]:
                            self.nodes[target].append(dep)
    
    
		    

    def filterNodes(self, seedsIn, seedsOut = None, allInBetween = False, showRebuildingTargets = False):
        print "Filtering nodes..."
        pp = pprint.PrettyPrinter(indent=2)
        targetsMap = copy.copy(self.nodes)

        reIn = []
        for seedIn in seedsIn:
            if verbose:
                print "Compiling seedIn: " + str(seedIn)
            reIn.append(re.compile(seedIn))

        reOut = []
        for seedOut in seedsOut:
            if verbose:
                print "Compiling seedOut: " + str(seedOut)
            reOut.append(re.compile(seedOut))

        nodes = {}
        for target in targetsMap.keys():
            if (len(reIn) ==0 or some(lambda r: r.search(target), reIn)) and not some(lambda r: r.search(target), reOut):
                nodes[target] = []

        dates = {}

        paths = map(lambda t: [t], nodes.keys())
        rebuildingNodes = []

        nbNodes = len(paths)
        nbNodesDone = 0
        currentStartTarget = None
        while len(paths) != 0:
            path = paths.pop()
            if verbose:
                print "Processing path: "
                pp.pprint(path)

            if path[0] != currentStartTarget:
                currentStartTarget = path[0]
                nbNodesDone += 1
                print "Processing node " + currentStartTarget + " " + str(nbNodesDone) + "/" + str(nbNodes);
                
            lastNode = path[-1]
            if not targetsMap.has_key(lastNode):
                continue


            deps = targetsMap[lastNode]
            # We empty it, just to be sure not to process it again.
            # We will fill in the valid target that we are able to
            # reach from this node.
            #
            # It this is already a good node, it will stay empty.
            # 
            targetsMap[lastNode] = []

            if showRebuildingTargets:
                targetExists = os.path.exists(lastNode);
                if not dates.has_key(lastNode) and targetExists:
                    dates[lastNode] = os.path.getmtime(lastNode)

            if len(deps) == 0:
                continue

            for dep in deps:
                if showRebuildingTargets:
                    depExists = os.path.exists(dep)
                    if not dates.has_key(dep) and depExists:
                        dates[dep] = os.path.getmtime(dep)
                    if targetExists and \
                           ((depExists and dates[lastNode] < dates[dep]) \
                            or (not depExists)):
                        rebuildingNodes.append(dep)
                        if not nodes.has_key(dep):
                            nodes[dep] = []
                newpath = path + [dep]
                if nodes.has_key(dep):
                    for node in path[1:-1]:
                        targetsMap[node].append(dep)
                    if dep not in nodes[path[0]]:
                        nodes[path[0]].append(dep)
                else:
                    paths.append(newpath)
        return MakeTree(nodes = nodes, rebuildingNodes = rebuildingNodes)


    def filterExtraDeps(self):
        print "Filtering extra deps..."
        nodes = copy.copy(self.nodes)

        paths = map(lambda t: [t], nodes.keys())
        while len(paths) != 0:
            path = paths.pop()

            lastNode = path[-1]
            if len(path) > 2 and lastNode in nodes[path[0]]:
                nodes[path[0]].remove(lastNode)

            deps = nodes[lastNode]
            if len(deps) == 0:
                continue

            for dep in deps:
                newpath = path + [dep]
                paths.append(newpath)
                
        return MakeTree(nodes = nodes)


def usage():
    print """
MakeGrapher

This program is used to generate a DOT file that represent the dependencies
between makefile's targets. It uses as input a makefile database.

The typical usage is (for viewing the LPEs dependencies within a loadbuild):

  IDILIA_LOADBUILD=yes make T=1 -npr > Makefile.complete
  python ~jpbarrette/Projects/MakeGrapher/make_grapher.py -T Makefile.complete -s "../tmp/build" -o test.dot
  dot -Tps test.dot > test.ps; ps2pdf test.ps; acroread test.pdf

acroread is WAY faster than kghostview. Since the graph might be quite big,
it would make a real difference.

-a, --all-in-between            This will enable the insertion of intermediate
                                targets between chosen targets.
-o, --output-file=FILE          the output file name (the dot file).
-v, --verbose                   toggle verbose output
-T, --database                  makefile output that is used as the database

"""



if __name__ == "__main__":


    try:
        opts, args = getopt.getopt(sys.argv[1:], "aF:to:fT:s:S:RBpv",
                                   ["seed-in=",
                                    "sort-dot-entries",
                                    "remove-extra-deps",
                                    "show-rebuilding-targets",
                                    "print-rebuilding-targets",
                                    "verbose"])
    except getopt.GetoptError:
        # print help information and exit:
        usage()
        sys.exit(2)

    seedsIn = []
    seedInFiles = []
    seedsOut = []
    outputFile = None
    invalidateNotTargets = False
    preFilterOut = []
    makefile = None
    runMake = True
    allInBetween = False
    sortDotEntries = False
    removeExtraDeps = False
    showRebuildingTargets = False
    printRebuildingTargets = False
    
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
        if o == "--seed-in":
            seedInFiles.append(a)
        if o == "-S":
            seedsOut.append(a)
        if o in ("-v", "--verbose"):
            verbose = True
        if o == "-t":
            invalidateNotTargets = True
        if o == "-a":
            allInBetween = True
        if o == "--sort-dot-entries":
            sortDotEntries = True
        if o in ("-R", "--remove-extra-deps"):
            removeExtraDeps = True
        if o in ("-B", "--show-rebuilding-targets"):
            showRebuildingTargets = True
        if o in ("-p", "--print-rebuilding-targets"):
            printRebuildingTargets = True
        

    if makefile is None and not os.path.exists("Makefile"):
        print "You didn't specified any Makefile, and there's no Makefile in the current directory"
        sys.exit(2)
        
    for seedInFile in seedInFiles:
        print "Seeding targets from %s context " % (seedInFile) 
        for line in open(seedInFile).readlines():
            seedsIn.append(line.strip(" \n"))

    tree = MakeTree(makefile = makefile, invalidateNotTargets = invalidateNotTargets, preFilterOut = preFilterOut, runMake = runMake)
    filteredTree = tree.filterNodes(seedsIn, seedsOut, allInBetween = allInBetween, showRebuildingTargets = showRebuildingTargets)
    if removeExtraDeps:
        filteredTree = filteredTree.filterExtraDeps()
    if outputFile is not None:
        filteredTree.graphVizExport(outputFile, sortDotEntries = sortDotEntries)
    if printRebuildingTargets:
        for target in filteredTree.rebuildingNodes:
            print target

    

#                     if allInBetween:
#                         for i in range(len(newpath)):
#                             nodes.setdefault(newpath[i], [])
#                             if i < (len(newpath) - 1):
#                                 if newpath[i + 1] not in nodes[newpath[i]]:
#                                     nodes[newpath[i]].append(newpath[i + 1])
#                     else:
#                         if dep not in nodes[path[0]]:
#                             nodes[path[0]].append(dep)

