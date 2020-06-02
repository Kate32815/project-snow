functor Benchmark4(structure List : RANDOM_ACCESS_LIST) : BENCHMARK =
  (* test the speed of update *)
struct
  open List

  fun build n =
      let fun b (0,ones) = ones
	    | b (n,ones) = b (n-1,cons 1 ones)
      in b (n,empty) end

  fun scan n xs =
      let fun loop (i,xs) =
	        if i = n then xs
		else loop (i+1,update xs i 0)
      in loop (0,xs) end

  fun run n reps =
        let val ones = build n
	in
	    Time.time (fn () => scan n ones,reps)
	end
end
