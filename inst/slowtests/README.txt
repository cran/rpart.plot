rpart.plot/src/tests/README.txt
-------------------------------

These are files for testing rpart.plot.  The make.bat script
runs test.prp.R and compares the results to
test.prp.Rout.save and test.prp.save.ps.

The tests are in a separate directory so they do not run
automatically.  This is because

(a) the tests are are very slow

(b) different versions of R generate slightly different
    postscript, so the diffs have to be examined manually

(c) the current version of the test script runs only under Windows.

Stephen Milborrow
Berea, Jan 2011
