open import "lib/github.com/diku-dk/sorts/merge_sort"
--open import "lib/github.com/diku-dk/segmented/segmented"

let dist (u: []i32) (v: []i32) : i32 =
    map (\a -> a.0 - a.1) (zip u v)
    |> map (**2)
    |> reduce (+) 0

let get 't [n] (inds: [n]i32) (src: []t) : [n]t =
    map (\i -> src[i]) inds

let construct_tree [n][d] (P: [n][d]i32) =
        let Pinds = iota n
        let T = []
        let i = 0 -- is also the height-1
        let stop = false
        -- FIXME: take the threshhold as an argument
        let threshhold = 0
        -- initial shape and offset arrays are trivial
        let shp = [n]
        let ofs = [0]
        -- design decision: I want to only have complete iterations,
        -- so shp and ofs will be created in the middle of each iteration
        let (Pinds,T,shp,ofs,_,i) = loop (Pinds,T,shp,ofs,stop,i) while (!stop) do
            let dim = i % d
            let fpairs = zip (map(\ind -> P[ind,dim]) Pinds) Pinds
            let seg_cnt = length shp
            -- sort the value/index-pairs in segments by the value
            -- FIXME: use radix sort instead
            let l_seg = shp[0]
            let (vals,Pinds) = iota (length shp)
                            |> map (\j -> let my_ofs = ofs[j]
                                          let my_len = shp[j]
                                          in (merge_sort_by_key (.0) (<=) fpairs[my_ofs:(my_ofs+my_len)])
                                            ++ (replicate (l_seg-my_len) (0,(-1))))
                            |> flatten
                            |> filter (\(_, p) -> p != -1)
                            |> unzip
            -- map is ... 'a 'b ([]a -> (a -> b) -> []b)
            -- of b == []t, we have..
            -- map is ... 'a 't ([]a -> (a -> []t) -> [][]t)
            -- can we have a mapcat function s.t. irregularities dont matter?
            -- mapcat ... 'a 'b ([]a -> (a -> []b) -> []b)

            -- create shp and ofs for new segmentation, after the sort
            -- each segment semantically gets split in two, with the diff going to the left segment
            -- eg: [5] -> [3,2] -> [2,1,1,1]
            -- eg: [4] -> [2,2] -> [1,1,1,1]
            -- in practice, the given threshhold value restricts this further
            let shp = flatten <| map (\c -> [(c+1)/2, c/2]) shp
            let ofs = [0] ++ (init (scan (+) 0 shp))
            -- for for every other new segment, w.e. one for every old, hence seg_cnt
            -- set of median values given by: offset of every other segment + length of every other segment - 1
            -- this gets the last value in every other segment, starting with 0, then 2 and so on
            -- if a segment of 5 has split into one of 3 and one of 2, as it would,
            -- this guarantees that the middle value gets picked as the median
            let T = T ++ (map (\j -> vals[ofs[j*2] + shp[j*2] - 1]) <| iota seg_cnt)

            -- there should be no furhter iterations after this one,
            -- if the last segment is below a threshhold in size, or if the last segment
            -- is of minimal size - in which case further iterations would break it
            let stop = i >= 5 || last shp <= threshhold || last shp == 1 --FIXME: should condition be expanded?
            in (Pinds,T,shp,ofs,stop,i+1)
        in (Pinds,T,shp,ofs,i+1) --fixme: is this the correct height?

let main [nd] [md] (d: i32) (fP: [nd]i32) (fQ: [md]i32) : []i32 =
        -- Constants
        let M = 2 -- Batch-size for how many indices are taken at a time -- FIXME: parametrize
        let k = 2 -- number of nearest-neighbors to find for each query -- FIXME: parametrize
        let BUFFER_CAPACITY = 4 -- buffers will be processed when there are this many elms -- FIXME: parametrize
        let IT_REG = 1
        let IT_FIRST = 0 --FIXME: check all constants are correct and used
        let STOP_ROOT = -1
        let STOP_LEAF = -2

        -- Inputs
        let n = nd / d
        let m = md / d
        let P = unflatten n d fP
        let Q = unflatten m d fQ
        let mk = m*k

        -- Tree
        let (Pinds,T,L_shp,L_ofs,h) = construct_tree P -- FIXME: call with threshhold leaf-size or height?
        let num_non_leaf_nodes = (1 << (h-1)) - 1
        let Leaf = map (\i -> P[i]) Pinds

        -- Values that will be modified in the loop
        let kNN_inds  = unflatten m k <| (replicate mk (-1i32))
        let kNN_dists = unflatten m k <| (replicate mk (i32.highest))
        let Stack = unflatten m h <| replicate (m*h) 0
        let Tinds = replicate m 0
        let Bf = []
        let in_q = iota m
        let re_q = []
        let stop = false

        -- Main loop
        let (kNN_inds',_,_,_,_,_,_,_) = loop (kNN_inds,kNN_dists,Stack,Tinds,Bf,in_q,re_q,stop) while !stop do
            -- Take M elements from queues
            let taken_from_re = if (length re_q >= M) then M else (length re_q)
            let rest = M - taken_from_re
            let taken_from_in = if (length in_q >= rest) then rest else (length in_q)
            let I = (take taken_from_re re_q) ++ (take taken_from_in in_q)
            let re_q' = drop taken_from_re re_q
            let in_q' = drop taken_from_in in_q

            ------------ FindLeafBatch START
            let Flags_I = replicate M IT_FIRST
            let Depth_I = (replicate taken_from_re (h-1)) ++ (replicate (M - taken_from_re) 0)
            let Tinds_I = get I Tinds
            let Stack_I = get I Stack

            let (_, _, Tinds_I', Stack_I') = loop (Flags_I, Depth_I, Tinds_I, Stack_I) while (or (map (>=0i32) Flags_I)) do
                map5 (\qidx flag depth tind stack ->
                    if (flag == IT_FIRST && depth > 0)
                        then (IT_REG, (depth-1), ((tind-1)>>1), (update (copy stack) depth 0))
                        else if (flag < 0)
                            then (IT_REG, depth, tind, stack)
                            else if (depth == (h-1))
                                then (STOP_LEAF, (depth-1), ((tind-1)>>1), stack)
                                else
                                let status = stack[depth] in
                                if (status == 3) -- if both children have been visited...time to go up!
                                    then ((if (depth <= 0) then STOP_ROOT else IT_REG), (depth-1), ((tind-1)>>1), (update (copy stack) depth 0))
                                    else
                                    let tmp = Q[qidx, depth % d] - T[tind] in
                                    if (status == 0) then -- if first visit
                                        if (tmp >= 0)
                                            then (IT_REG, (depth+1), (2*tind+2), (update (copy stack) depth 1)) -- go right
                                            else (IT_REG, (depth+1), (2*tind+1), (update (copy stack) depth 2)) -- go left
                                        else if (tmp*tmp <= kNN_dists[qidx, k-1])
                                            -- te test instance is nearer to the median than to the nearest neighbors
                                            then (IT_REG, (depth+1), (2*tind+status), (update (copy stack) depth 3))
                                            -- if the median is far away, we can go up (without checking the other side)
                                            else ((if (depth <= 0) then STOP_ROOT else IT_REG), (depth-1), ((tind-1)>>1), (update (copy stack) depth 0))
                ) I Flags_I Depth_I Tinds_I Stack_I |> unzip4

            let Stack' = scatter (copy Stack) I Stack_I'
            let Tinds' = scatter (copy Tinds) I Tinds_I'

            let Query_leaf_inds = map2 (\tind flag -> if (flag == STOP_ROOT) then -1 else tind - num_non_leaf_nodes) Tinds_I Flags_I-- leaf-idx recoverd from t_idx
            -- Result R: the Q leaf-ind batch zipped with their respective query inds
            let Bf' = Bf ++ (filter (\a -> a.1 != -1) <| zip Query_leaf_inds I)
            ------------ FindLeafBatch END

            ------------ ProcessAllBuffers BEGIN
            let Bf_len = length Bf'
            let trigger_process_buffers = Bf_len >= BUFFER_CAPACITY || (length in_q' + length re_q' == 0) -- FIXME: Detect "full buffers"

            let reinsert_in_re_q = []

            let (kNN_inds', kNN_dists', reinsert_in_re_q) = if !trigger_process_buffers then (kNN_inds, kNN_dists, reinsert_in_re_q) else
                let (Bf_vals, Bf_inds) = merge_sort_by_key (.1) (<=) Bf' |> unzip
                let (Bf_ofs, Bf_buff_id) = iota Bf_len
                                           |> map (\i -> (i, if (i==0 || Bf_inds[i-1] != Bf_inds[i]) then Bf_inds[i] else -1))
                                           |> filter (\a -> a.1 != -1)
                                           |> unzip
                let Bf_shp = map (\i -> if(i < Bf_len - 1) then Bf_ofs[i+1] - Bf_ofs[i] else Bf_ofs[i] - Bf_len - 1) <| iota (length Bf_ofs)
                --for every buffer Bf_i
                in loop (kNN_inds, kNN_dists, reinsert_in_re_q) for j < length Bf_shp do
                    let buf_len = Bf_shp[j]
                    let buf_ofs = Bf_ofs[j]
                    let desired_leaf = Bf_buff_id[j]
                    let Qis = Bf_vals[buf_ofs:(buf_ofs+buf_len-1)]
                    let test_len = L_shp[desired_leaf]
                    let test_ofs = L_ofs[desired_leaf]
                    let P_inds = map (+test_ofs) (iota test_len)
                    let Ps = Leaf[test_ofs:(test_ofs+test_len-1)]
                    let (kNN_inds_subset, kNN_dists_subset) = unzip <|
                                  map(\i ->
                                    let single_kNN_inds_set = kNN_inds[i]
                                    let single_kNN_dists_set = kNN_dists[i]
                                    let Q_elm = Q[i]
                                    let dists = map (\i -> dist Q_elm Ps[i]) (iota test_len)
                                    let enlarged = zip (single_kNN_inds_set ++ P_inds) (single_kNN_dists_set ++ dists)
                                    let enlarged = merge_sort_by_key (.1) (<=) enlarged
                                    in unzip (take k enlarged)
                                  ) Qis
                    let kNN_inds = scatter kNN_inds Qis kNN_inds_subset
                    let kNN_dists = scatter kNN_dists Qis kNN_dists_subset
                    let reinsert_in_re_q' = reinsert_in_re_q ++ Qis
                    in (kNN_inds, kNN_dists,reinsert_in_re_q')
            ------------ ProcessAllBuffers END
            let re_q'' = re_q' ++ reinsert_in_re_q
            let stop' = null in_q' && null re_q''
            in (kNN_inds', kNN_dists', Stack',Tinds',Bf',in_q',re_q'',stop')
        in flatten kNN_inds'
        -- TODO: fix this return statement!

         --TODO: return something more clean
