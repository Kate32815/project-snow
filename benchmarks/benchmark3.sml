functor Benchmark3(structure List : RANDOM_ACCESS_LIST) : BENCHMARK =
  (* test the speed of lookup *)
struct
  open List

  fun build n =
      let fun b (0,ones) = ones
	    | b (n,ones) = b (n-1,cons 1 ones)
      in b (n,empty) end

  fun scan n xs =
      let fun sum (i,total) =
	        if i = n then total
		else sum (i+1,lookup xs i + total)
      in sum (0,0) end

  fun run n reps = 
        let val ones = build n
	in
	    Time.time (fn () => scan n ones,reps)
	end
end
