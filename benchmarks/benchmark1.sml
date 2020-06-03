functor Benchmark1(structure List : RANDOM_ACCESS_LIST)=
  (* quicksort *)
struct
  open List

  (* random number generator adapted from Paulson *)
  val a = 16807.0
  val m = 2147483647.0
  fun nextrandom seed =
        let val t = a*seed
        in  t - m * real(floor(t/m))  end
  fun randlist n =
        let fun r 0 _ = empty
	      | r n seed = cons seed (r (n-1) (nextrandom seed))
	in r n 1.0 end

  fun quicksort xs : real ralist =
    let fun partition (pivot:real,xs,smalls,bigs) =
          if isempty xs then (smalls,bigs)
          else let val x = head xs
                   val xs = tail xs
               in
                   if x < pivot then partition (pivot,xs,cons x smalls,bigs)
                                else partition (pivot,xs,smalls,cons x bigs)
               end
	fun qsort xs rest =
          if isempty xs then rest
          else let val x = head xs
                   val xs = tail xs
                   val (smalls,bigs) = partition (x,xs,empty,empty)
               in
                   qsort smalls (cons x (qsort bigs rest))
               end
    in qsort xs empty end

  (* for testing *)
  fun convert xs = if isempty xs then [] else head xs :: convert (tail xs)

  (*
  fun run n reps =
        let val r = randlist n
	in
	    Time.time (fn () => quicksort r,reps)
	end
  *)
end
