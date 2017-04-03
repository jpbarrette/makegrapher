# MakeGrapher

This program is used to generate a DOT file that represent the dependencies
between makefile's targets. It uses as input a makefile database.

The typical usage is (for viewing the LPEs dependencies within a loadbuild):

```
  IDILIA_LOADBUILD=yes make T=1 -npr > Makefile.complete
  python ~jpbarrette/Projects/MakeGrapher/make_grapher.py -T Makefile.complete -s "../tmp/build" -o test.dot
  dot -Tps test.dot > test.ps; ps2pdf test.ps; acroread test.pdf
```

acroread is WAY faster than kghostview. Since the graph might be quite big,
it would make a real difference.

```
-a, --all-in-between            This will enable the insertion of intermediate
                                targets between chosen targets.
-o, --output-file=FILE          the output file name (the dot file).
-v, --verbose                   toggle verbose output
-T, --database                  makefile output that is used as the database
-c, --connected                 produce connected graph to node
```
