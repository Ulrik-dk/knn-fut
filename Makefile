version = v5
backend = opencl
data = data/

setup:
	@$(MAKE) very-clean --no-print-directory
	@$(MAKE) datasets --no-print-directory
	@$(MAKE) compile_all --no-print-directory
	@$(MAKE) outs --no-print-directory
	@$(MAKE) run_builtin_benchmarks --no-print-directory

datasets:
	@mkdir data &> /dev/null
	@futhark dataset -b --generate=[200000][5]f32 --generate=[20000][5]f32 > $(data)test1.in
	@futhark dataset -b --generate=[200000][7]f32 --generate=[20000][7]f32 > $(data)test2.in
	@futhark dataset -b --generate=[200000][9]f32 --generate=[20000][9]f32 > $(data)test3.in
	@futhark dataset -b --generate=[200000][11]f32 --generate=[20000][11]f32 > $(data)test4.in
	@futhark dataset -b --generate=[200000][20]f32 --generate=[20000][20]f32 > $(data)test5.in

outs:
	./bf < $(data)test1.in > $(data)test1.out
	./bf < $(data)test2.in > $(data)test2.out
	./v3 < $(data)test3.in > $(data)test3.out
	./v3 < $(data)test4.in > $(data)test4.out
	./v3 < $(data)test5.in > $(data)test5.out

compile_all:
	futhark $(backend) bf.fut -w
	futhark $(backend) v1.fut -w
	futhark $(backend) v2.fut -w
	futhark $(backend) v3.fut -w
	futhark $(backend) v4.fut -w
	futhark $(backend) v5.fut -w

run_builtin_benchmarks:
	futhark bench bf.fut --backend=$(backend) -r 3 --skip-compilation
	futhark bench v1.fut --backend=$(backend) -r 3 --skip-compilation
	futhark bench v2.fut --backend=$(backend) -r 3 --skip-compilation
	futhark bench v3.fut --backend=$(backend) -r 3 --skip-compilation
	futhark bench v4.fut --backend=$(backend) -r 3 --skip-compilation
	futhark bench v5.fut --backend=$(backend) -r 3 --skip-compilation

run_benchmarks:
	./$(version) -t /dev/stderr -r 3 < $(data)test3.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test4.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test5.in > /dev/null

clean:
	rm -f bf bf.c
	rm -f v1 v1.c
	rm -f v2 v2.c
	rm -f v3 v3.c
	rm -f v4 v4.c
	rm -f v5 v5.c

very-clean:
	rm -rf $(data)
	@$(MAKE) clean --no-print-directory

ctb: #compile test benchmark
	futhark $(backend) $(version).fut
	futhark test $(version).fut
	@$(MAKE) run_benchmarks

bf:
	@$(MAKE) ctb version=bf --no-print-directory
v1:
	@$(MAKE) ctb version=v1 --no-print-directory
v2:
	@$(MAKE) ctb version=v2 --no-print-directory
v3:
	@$(MAKE) ctb version=v3 --no-print-directory
v4:
	@$(MAKE) ctb version=v4 --no-print-directory
v5:
	@$(MAKE) ctb version=v5 --no-print-directory

test_all:
	futhark test v1.fut --backend=$(backend)
	futhark test v2.fut --backend=$(backend)
	futhark test v3.fut --backend=$(backend)
	futhark test v4.fut --backend=$(backend)
	futhark test v5.fut --backend=$(backend)

bench_all:
	@$(MAKE) run_benchmarks version=v1 --no-print-directory
	@$(MAKE) run_benchmarks version=v2 --no-print-directory
	@$(MAKE) run_benchmarks version=v3 --no-print-directory
	@$(MAKE) run_benchmarks version=v4 --no-print-directory
	@$(MAKE) run_benchmarks version=v5 --no-print-directory
