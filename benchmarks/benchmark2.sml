functor Benchmark2(structure List : RANDOM_ACCESS_LIST) : BENCHMARK =
  (* test the speed of cons and tail *)
struct
  open List

  fun sum ns =
      let fun s (ns,total) =
	    if isempty ns then total
	    else s (tail ns,head ns+total)
      in s (ns,0) end

  fun build n =
      let fun b (0,ones) = ones
	    | b (n,ones) = b (n-1,cons 1 ones)
      in b (n,empty) end

  fun run n reps = Time.time (fn () => sum (build n),reps)
end
