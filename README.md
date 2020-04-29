# knn-fut
knn in futhark
## versions:
  bf

Simple brute force algorithm
  v1

buffer k-d tree-ish
  v2

v1 but with strategic sorting
## backends:
    c
    opencl
## how to
#### setup
    make setup
#### main commands
    make test version={VERSION} backend={BACKEND}
    make bench version={VERSION} backend={BACKEND}
defaults are v1 and c
#### other commands
    make clean
    make very-clean
very-clean also removes data/

    make manual-bench version={VERSION} backend={BACKEND}

#### make-todos:
it would be nice for the makefile to just automatically detect input and outputs from the data/ folder, so the test-data is defined in just one place
