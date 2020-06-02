functor Benchmark5(structure List : RANDOM_ACCESS_LIST) : BENCHMARK =
  (* histogram *)
struct
  open List

  (* random number generator adapted from Paulson *)
  val a = 16807.0
  val m = 2147483647.0
  fun nextrandom seed =
        let val t = a*seed
        in  t - m * real(floor(t/m))  end
  fun randlist n i =
        let val n' = real n
	    fun norm s = truncate (n' * (s / m))
	    fun r 0 _ = []
	      | r i seed = norm seed :: r (i-1) (nextrandom seed)
	in r i 1.0 end

  fun init 0 = empty
    | init n = cons 0 (init (n-1))

  fun inc (i,a) = update a i (lookup a i + 1)

  fun histo n = fold inc (randlist n (5*n)) (init n)

  fun run n reps = Time.time (fn () => histo n,reps)
end


