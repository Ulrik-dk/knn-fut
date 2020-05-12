# knn-fut
knn in futhark
## versions:
#### bf
Simple brute force algorithm
#### v1
buffer k-d tree-ish
#### v2
adds sorting
#### v3
adds
#### v4
adds
#### v5
uses a more precise test
#### v6
stops sorting and uses the indirect array again, is faster in some cases. TODO: should use magic threshholds to determine whether to do brute-force, use sorting, etc.

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
