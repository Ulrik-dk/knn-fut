v1:
	@futhark c v1.fut
	@echo "2f32 [0f32,1,2,3,4,5,6,7,1,2,4,3,2,1] 2i32 1i32" |./v1

v1-test:
	futhark dataset -b --generate=[134217728]f32 > v1.in
	futhark c v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
	futhark opencl v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null

brute:
	@futhark c brute.fut

paper:
	@futhark c paper.fut

tree-trav:
	@futhark c tree-trav.fut
	@echo "3 1024 8024.0f32 3000.0f32" |./tree-trav
