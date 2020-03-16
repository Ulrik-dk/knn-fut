PAPER = paper
BRUTE = brute
IMPROVED = improved

brute:
	@$(MAKE) --no-print-directory $(BRUTE:%=compile_c%)
	@echo "2i32 [0i32,1,2,3,4,5,6]" |./$(BRUTE)

paper:
	@$(MAKE) --no-print-directory $(PAPER:%=compile_c%)
	@echo "2i32 [0i32,1,2,3,4,5,6]" |./$(PAPER)

improved:
	@$(MAKE) --no-print-directory $(IMPROVED:%=compile_c%)
	@echo "2i32 [0i32,1,2,3,4,5,6]" |./$(IMPROVED)

compile_c%:
	@futhark c $*.fut

compile_opencl%:
	@futhark opencl $*.fut
