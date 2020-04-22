validation-v1:
	@futhark dataset -b --generate=[1000][6]f32 --generate=[1000][6]f32 > test.in
	@futhark c bf.fut
	./bf --entry=just_distances -t /dev/stderr -r 3 < test.in > test.out
	@futhark c v1.fut
	./v1 --entry=just_distances -t /dev/stderr -r 3 < test.in > /dev/null

v1:
	@futhark c v1.fut
	@echo "2i32 [[0f32,1],[2,3],[4,5],[6,7],[1,2],[4,3],[2,1]] [[0f32, 0]]" |./v1

bf:
	@futhark c bf.fut
	@echo "[[1f32,1],[2,3],[4,5],[6,7],[1,2],[4,3],[2,1]] [[0f32, 0]]" |./bf

clean:
	@rm bf bf.c v1 v1.c tree-trav tree-trav.c &> /dev/null

v1-test:
	futhark dataset -b --generate=[134217728]f32 > v1.in
	futhark c v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
	futhark opencl v1.fut
	./v1 --entry=test -t /dev/stderr -r 3 < v1.in > /dev/null
