PAPER = paper
BRUTE = brute
V1 = v1

default:
	@$(MAKE) --no-print-directory $(V1:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6] 1i32 1i32" |./$(V1)

brute:
	@$(MAKE) --no-print-directory $(BRUTE:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(BRUTE)

paper:
	@$(MAKE) --no-print-directory $(PAPER:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(PAPER)

v1:


compile_c%:
	@futhark c $*.fut

compile_opencl%:
	@futhark opencl $*.fut
