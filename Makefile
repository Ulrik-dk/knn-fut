PAPER = paper
BRUTE = brute
IMPROVED = improved

default:
	@$(MAKE) --no-print-directory $(IMPROVED:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(IMPROVED)

brute:
	@$(MAKE) --no-print-directory $(BRUTE:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(BRUTE)

paper:
	@$(MAKE) --no-print-directory $(PAPER:%=compile_c%)
	@echo "2f32 [0f32,1,2,3,4,5,6]" |./$(PAPER)

improved:


compile_c%:
	@futhark c $*.fut

compile_opencl%:
	@futhark opencl $*.fut
