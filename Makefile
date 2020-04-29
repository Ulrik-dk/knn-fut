versions = bf v1 v2
backend = c
data = data/

setup:
	@$(MAKE) very-clean --no-print-directory
	@$(MAKE) datasets --no-print-directory
	@$(MAKE) outs --no-print-directory
	@$(MAKE) clean --no-print-directory

datasets:
	@mkdir data &> /dev/null
	@futhark dataset -b --generate=[100][30]f32 --generate=[100][30]f32 > $(data)test1.in
	@futhark dataset -b --generate=[1000][4]f32 --generate=[1000][4]f32 > $(data)test2.in
	@futhark dataset -b --generate=[5000][3]f32 --generate=[5000][3]f32 > $(data)test3.in

outs:
	futhark $(backend) bf.fut
	./bf --entry=just_distances < $(data)test1.in > $(data)test1.out
	./bf --entry=just_distances < $(data)test2.in > $(data)test2.out
	./bf --entry=just_distances < $(data)test3.in > $(data)test3.out

clean:
	rm -f bf bf.c
	rm -f v1 v1.c
	rm -f v2 v2.c

very-clean:
	rm -rf $(data)
	@$(MAKE) clean --no-print-directory

bf:
	futhark $(backend) bf.fut

v1:
	futhark $(backend) v1.fut

v2:
	futhark $(backend) v1.fut

test: $(version)
	futhark test $(version).fut --backend=$(backend)

bench: $(version) $(backend)
	futhark bench $(version).fut --backend=$(backend)

v1-test:
	futhark dataset -b --generate=[134217728]f32 > v1.in
	futhark c v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
	futhark opencl v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
