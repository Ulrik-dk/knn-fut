version = v2
backend = c
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

outs:
	futhark $(backend) bf.fut
	./bf < $(data)test1.in > $(data)test1.out
	./bf < $(data)test2.in > $(data)test2.out

run_benchmarks:
	./$(version) -t /dev/stderr -r 3 < $(data)test1.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test2.in > /dev/null

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
	futhark test v5.fut --backend=$(backend)
	futhark test v4.fut --backend=$(backend)

bench_all:
	futhark bench v1.fut --backend=$(backend)
	futhark bench v2.fut --backend=$(backend)
	futhark bench v3.fut --backend=$(backend)
	futhark bench v5.fut --backend=$(backend)
	futhark bench v4.fut --backend=$(backend)
