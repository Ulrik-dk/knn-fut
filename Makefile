version = v7
r = 1
backend = opencl
data = data/
test_name = please_specify_manually
TARGETS = bf v1 v2 v3 v4 v5 v6 v7
TESTS = test1 test2 test3 test4 test5 test6

setup:
	@$(MAKE) very-clean --no-print-directory
	@$(MAKE) compile --no-print-directory
	@$(MAKE) ins --no-print-directory
	@$(MAKE) outs --no-print-directory
	@echo "You can now type 'make test' or 'make fut-test' or 'make bench'"
ins:
	@rm -rf data
	@mkdir data &> /dev/null
	@futhark dataset -b --generate=[1][1]f32 --generate=[1][1]f32 > $(data)test1.in
	@futhark dataset -b --generate=[500][200]f32 --generate=[500][200]f32 > $(data)test2.in
	@futhark dataset -b --generate=[5000][11]f32 --generate=[5000][11]f32 > $(data)test3.in
	@futhark dataset -b --generate=[50000][3]f32 --generate=[50000][3]f32 > $(data)test4.in
	@futhark dataset -b --generate=[500000][5]f32 --generate=[500000][5]f32 > $(data)test5.in
	@futhark dataset -b --generate=[5][500000]f32 --generate=[5][500000]f32 > $(data)test6.in
out_%:
	./bf < $(data)$*.in > $(data)$*.out
outs: $(TESTS:%=out_%)

compile_%:
	futhark $(backend) $*.fut -w
	futhark $(backend) $*-bench.fut -w
	futhark $(backend) $*-test.fut -w
compile: $(TARGETS:%=compile_%)

fut_test:
	futhark test $(version)-test.fut --backend=$(backend)
run_fut_test_%:
	@$(MAKE) fut_test version=$* --no-print-directory
fut_tests: $(TARGETS:%=run_fut_test_%)

test:
	futhark bench $(version)-test.fut --backend=$(backend) -r 1
run_test_%:
	@$(MAKE) test version=$* --no-print-directory
tests: $(TARGETS:%=run_test_%)

bench:
	nvidia-smi &> results/$(test_name)/$(version)-results.txt
	futhark bench $(version)-bench.fut --backend=$(backend) -r $(r) &>> results/$(test_name)/$(version)-results.txt
run_bench_%:
	@$(MAKE) bench version=$* --no-print-directory
benchs: $(TARGETS:%=run_bench_%)

clean_thing_%:
	rm -f $* $*.c $*-bench $*-bench.c $*-test $*-test.c
clean: $(TARGETS:%=clean_thing_%)

very-clean:
	rm -rf $(data)
	@$(MAKE) clean --no-print-directory
