PAPER = paper
BRUTE = brute
V1 = v1

default:
	@$(MAKE) --no-print-directory $(V1:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6,7,1,2,4,3,2,1] 2i32 1i32" |./$(V1)

brute:
	@$(MAKE) --no-print-directory $(BRUTE:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(BRUTE)

paper:
	@$(MAKE) --no-print-directory $(PAPER:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(PAPER)

	# 16 * 8388608 = 134217728
v1-test:
	futhark dataset -b --generate=[134217728]f32 > v1.in
	futhark c v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
	futhark opencl v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null

compile_c%:
	@futhark c $*.fut

compile_opencl%:
	@futhark opencl $*.fut

clean:
	rm -rf v1.c v1
