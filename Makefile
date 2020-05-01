version = v6
backend = opencl
data = data/

setup:
	@$(MAKE) very-clean --no-print-directory
	@$(MAKE) datasets --no-print-directory
	@$(MAKE) outs --no-print-directory
	#@$(MAKE) clean --no-print-directory

datasets:
	@mkdir data &> /dev/null
	@futhark dataset -b --generate=[32768][3]f32 --generate=[32768][3]f32 > $(data)test1.in
	@futhark dataset -b --generate=[65536][8]f32 --generate=[65536][8]f32 > $(data)test2.in
	@futhark dataset -b --generate=[262144][4]f32 --generate=[262144][4]f32 > $(data)test3.in
	@futhark dataset -b --generate=[131072][8]f32 --generate=[131072][8]f32 > $(data)test4.in
	@futhark dataset -b --generate=[65536][16]f32 --generate=[65536][16]f32 > $(data)test5.in

outs:
	futhark $(backend) bf.fut
	./bf < $(data)test1.in > $(data)test1.out
	./bf < $(data)test2.in > $(data)test2.out
	futhark $(backend) v3.fut
	./v3 < $(data)test3.in > $(data)test3.out
	./v3 < $(data)test4.in > $(data)test4.out
	./v3 < $(data)test5.in > $(data)test5.out

compile_all:
	futhark $(backend) bf.fut
	futhark $(backend) v1.fut
	futhark $(backend) v2.fut
	futhark $(backend) v3.fut
	futhark $(backend) v4.fut
	futhark $(backend) v5.fut
	futhark $(backend) v6.fut

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
	rm -f v6 v6.c

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
v6:
	@$(MAKE) ctb version=v6 --no-print-directory

test_all:
	futhark test v1.fut --backend=$(backend)
	futhark test v2.fut --backend=$(backend)
	futhark test v3.fut --backend=$(backend)
	futhark test v4.fut --backend=$(backend)
	futhark test v5.fut --backend=$(backend)
	futhark test v6.fut --backend=$(backend)

bench_all:
	@$(MAKE) run_benchmarks version=v1 --no-print-directory
	@$(MAKE) run_benchmarks version=v2 --no-print-directory
	@$(MAKE) run_benchmarks version=v3 --no-print-directory
	@$(MAKE) run_benchmarks version=v4 --no-print-directory
	@$(MAKE) run_benchmarks version=v5 --no-print-directory
	@$(MAKE) run_benchmarks version=v6 --no-print-directory
