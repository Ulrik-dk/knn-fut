version = v2
backend = opencl
data = data/

setup:
	@$(MAKE) very-clean --no-print-directory
	@$(MAKE) datasets --no-print-directory
	@$(MAKE) outs --no-print-directory
	#@$(MAKE) clean --no-print-directory

datasets:
	@mkdir data &> /dev/null
	@futhark dataset -b --generate=[100][30]f32 --generate=[100][30]f32 > $(data)test1.in
	@futhark dataset -b --generate=[1000][4]f32 --generate=[1000][4]f32 > $(data)test2.in
	@futhark dataset -b --generate=[5000][3]f32 --generate=[5000][3]f32 > $(data)test3.in
	@futhark dataset -b --generate=[10000][4]f32 --generate=[10000][4]f32 > $(data)test4.in
	@futhark dataset -b --generate=[100000][16]f32 --generate=[100000][16]f32 > $(data)test5.in

outs:
	futhark $(backend) bf.fut
	./bf < $(data)test1.in > $(data)test1.out
	./bf < $(data)test2.in > $(data)test2.out
	./bf < $(data)test3.in > $(data)test3.out
	./bf < $(data)test4.in > $(data)test4.out
	./bf < $(data)test5.in > $(data)test5.out

clean:
	rm -f bf bf.c
	rm -f v1 v1.c
	rm -f v2 v2.c

very-clean:
	rm -rf $(data)
	@$(MAKE) clean --no-print-directory

bf:
	futhark $(backend) bf.fut

run_benchmarks: $(version)
	./$(version) -t /dev/stderr -r 3 < $(data)test1.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test2.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test3.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test4.in > /dev/null
	./$(version) -t /dev/stderr -r 3 < $(data)test5.in > /dev/null

v1:
	futhark $(backend) v1.fut
	futhark test v1.fut
	futhark bench v1.fut --backend=$(backend)
	./v1 -t /dev/stderr -r 3 < $(data)test1.in > /dev/null
	./v1 -t /dev/stderr -r 3 < $(data)test2.in > /dev/null
	./v1 -t /dev/stderr -r 3 < $(data)test3.in > /dev/null

v2:
	futhark $(backend) v2.fut
	futhark test v2.fut
	futhark bench v2.fut --backend=$(backend)
	./v2 -t /dev/stderr -r 3 < $(data)test1.in > /dev/null
	./v2 -t /dev/stderr -r 3 < $(data)test2.in > /dev/null
	./v2 -t /dev/stderr -r 3 < $(data)test3.in > /dev/null

test:
	futhark test $(version).fut --backend=$(backend)

bench:
	futhark bench $(version).fut --backend=$(backend)
