structure KaplanTarjanList : RANDOM_ACCESS_LIST =
    (* an adaptation of the deques in                               *)
    (*   Haim Kaplan and Robert Tarjan                              *)
    (*   "Persistent Lists with Catenation via Recursive Slow-Down" *)
    (*   STOC '95                                                   *)
struct
    datatype 'a pairs = Elem of 'a | Pair of 'a pairs * 'a pairs

    datatype 'a ralist =
       Even of 'a pairs vector * 'a ralist
     | Odds of 'a pairs vector list * 'a ralist
     | Bottom of 'a pairs vector

    (* Every vector has length 0..4.                                       *)
    (* The first vector contains elements, the second pairs of elements,   *)
    (*   the third pairs of pairs of elements, and so on.                  *)
    (* The "Even" constructor indicates a vector of length 0, 2, or 4.     *)
    (* The "Odds" constructor indicates one or more vectors of len 1 or 3. *)
    (*   (Never have two "Odds" constructors in a row.)                    *)
    (*                                                                     *)
    (* Vectors of length 2 are called "green".                             *)
    (* Vectors of length 1 or 3 are called "yellow".                       *)
    (* Vectors of length 0 or 4 are called "red".                          *) 
    (* The bottom vector is "red" if has length 4, otherwise "green".      *)
    (*                                                                     *)
    (* We guarantee that the first non-yellow vector is green,             *)
    (* and that there is at least one green vector between every two       *)
    (* red vectors.                                                        *)

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = Bottom #[]

(***** Primitve operations on vectors *****)

    fun vcons a #[] = #[a]
      | vcons a #[b] = #[a,b]
      | vcons a #[b,c] = #[a,b,c]
      | vcons a #[b,c,d] = #[a,b,c,d]

    fun vhead #[] = raise Empty
      | vhead v = Vector.sub (v,0)

    fun vtail #[] = raise Empty
      | vtail #[a] = #[]
      | vtail #[a,b] = #[b]
      | vtail #[a,b,c] = #[b,c]
      | vtail #[a,b,c,d] = #[b,c,d]

(***** Primitive operations on lists *****)
(* may leave first non-yellow vector red *)

    fun pcons x (Bottom v) = Bottom (vcons x v)
      | pcons x (Even (v,Odds (vs,rest))) = Odds (vcons x v :: vs,rest)
      | pcons x (Even (v,rest)) = Odds ([vcons x v],rest)
      | pcons x (Odds ([v],rest)) = Even (vcons x v,rest)
      | pcons x (Odds (v :: vs,rest)) = Even (vcons x v,Odds (vs,rest))

    fun phead (Bottom v) = vhead v
      | phead (Even (v,rest)) = vhead v
      | phead (Odds (v :: vs,rest)) = vhead v

    fun ptail (Bottom v) = Bottom (vtail v)
      | ptail (Even (v,Odds (vs,rest))) = Odds (vtail v :: vs,rest)
      | ptail (Even (v,rest)) = Odds ([vtail v],rest)
      | ptail (Odds ([v],rest)) = Even (vtail v,rest)
      | ptail (Odds (v :: vs,rest)) = Even (vtail v,Odds (vs,rest))

(***** Restore invariant if first non-yellow vector is red *****)

    fun fix (Bottom #[a,b,c,d]) = Even (#[a,b],Bottom #[Pair (c,d)])
      | fix (Odds (vs,rest)) = Odds (vs,fix rest)
      | fix (Even (#[],Bottom #[])) = Bottom #[]
      | fix (Even (#[],rest)) = let val (Pair (a,b)) = phead rest
			        in Even (#[a,b],ptail rest) end
      | fix (Even (#[a,b,c,d],rest)) = Even (#[a,b],pcons (Pair (c,d)) rest)
      | fix xs = xs

(***** Top level list operations *****)

    fun cons x xs = fix (pcons (Elem x) xs)
    fun head xs = let val (Elem x) = phead xs
		  in x end
    fun tail xs = fix (ptail xs)

    fun isempty (Bottom #[]) = true
      | isempty _ = false

    fun vlen rank v = Bits.lshift (Vector.length v, rank)    
    fun length xs =
	  let fun len rank (Bottom v) = vlen rank v
		| len rank (Even (v,rest)) = vlen rank v + len (rank+1) rest
		| len rank (Odds ([],rest)) = len rank rest
		| len rank (Odds (v::vs,rest)) =
	            vlen rank v + len (rank+1) (Odds (vs,rest))
	  in
	      len 0 xs
	  end

    fun lookup xs i =
	  let fun ishere rank v i = i < vlen rank v
	      fun find size 0 (Elem x) = x
		| find size i (Elem x) = raise Subscript
		| find size i (Pair (a,b)) =
		    let val size' = Bits.rshift (size,1)
		    in
			if i < size' then find size' i a
			else find size' (i-size') b
		    end
	      fun vlook rank v i =
		    let val size = Bits.lshift (1,rank)
			val j = Bits.rshift (i,rank)
			val offset = i - Bits.lshift (j,rank)
		    in
			find size offset (Vector.sub (v,j))
		    end
	      fun look rank (Bottom v) i =
		    if ishere rank v i then vlook rank v i
		    else raise Subscript
		| look rank (Even (v,rest)) i =
		    if ishere rank v i then vlook rank v i
		    else look (rank+1) rest (i - vlen rank v)
		| look rank (Odds ([],rest)) i =
		    look rank rest i
		| look rank (Odds (v::vs,rest)) i = 
		    if ishere rank v i then vlook rank v i
		    else look (rank+1) (Odds (vs,rest)) (i - vlen rank v)
	  in
	      look 0 xs i
	  end

    fun update xs i x =
	  let fun ishere rank v i = i < vlen rank v
	      fun upd size 0 (Elem _) = Elem x
		| upd size i (Elem _) = raise Subscript
		| upd size i (Pair (a,b)) =
		    let val size' = Bits.rshift (size,1)
		    in
			if i < size' then Pair (upd size' i a,b)
			else Pair (a,upd size' (i-size') b)
		    end
	      fun vupd rank v i =
		    let val size = Bits.lshift (1,rank)
			val j = Bits.rshift (i,rank)
			val offset = i - Bits.lshift (j,rank)
			val p = upd size offset (Vector.sub (v,j))
		    in
			Vector.tabulate (Vector.length v,
                          fn i => if i=j then p else Vector.sub (v,i))
		    end
	      fun search rank (Bottom v) i =
		    if ishere rank v i then Bottom (vupd rank v i)
		    else raise Subscript
		| search rank (Even (v,rest)) i =
		    if ishere rank v i then Even (vupd rank v i,rest)
		    else Even (v,search (rank+1) rest (i - vlen rank v))
		| search rank (Odds ([],rest)) i = 
		    Odds ([],search rank rest i)
		| search rank (Odds (v::vs,rest)) i =
		    if ishere rank v i then Odds (vupd rank v i :: vs,rest)
		    else let val (Odds (vs,rest)) =
			        search (rank+1) (Odds (vs,rest))
				       (i - vlen rank v)
			 in Odds (v::vs,rest) end
	  in
	      search 0 xs i
	  end
end
