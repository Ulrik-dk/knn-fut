backend = opencl
n = 1000000
d = 5
TARGETS = qualitative_bounds_traversal qualitative_median_traversal

prep:
	futhark dataset -g [$(n)][$(d)]f32 -g [$(n)][$(d)]f32 > $(n)_$(d).in

compile_%:
	futhark $(backend) $*.fut -w
compile: $(TARGETS:%=compile_%)

out_%:
	./$* < $(n)_$(d).in > $(n)_$(d)_$*.out
outs: $(TARGETS:%=out_%)

qualitative_test:
	@$(MAKE) prep --no-print-directory
	@$(MAKE) compile --no-print-directory
	@$(MAKE) outs --no-print-directory

clean_thing_%:
	rm -f $* $*.c
clean: $(TARGETS:%=clean_thing_%)
