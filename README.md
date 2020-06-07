# knn-fut
knn in futhark
## versions:
#### bf
brute force algorithm
#### v1
initial kd-tree version
#### v2
sorts query metadata
#### v3
sorts queries as well
#### v4
optimizes radix-sort a bit
#### v5
uses the boundary-test rather than median-test for traversal
#### v6
combines partitioning and sorting in the code
#### v7
uses a treshhold to stop sorting after a certain fraction of active queries to leaves

## how to:
#### benchmark
  To run a benchmark, as defined in a v7-bench.fut file, create a subfolder you wish the results to be put into,inside the 'results/' folder. Then type 'make bench version=v7 test_name={subfolder/}', using the name of the folder you created. To run them all, type 'make benchs'.
#### test
  To run a test, as defined in a v7-test.fut, type 'make setup_tests' followed by  'make test version=v7'. To run them all, type 'make tests'.
## disclaimer:
  dont trust the code comments, they are out of date
