structure RandomAccessList : RANDOM_ACCESS_LIST =
    (* as presented in                           *)
    (*   Chris Okasaki                           *)
    (*   "Purely Functional Random-Access Lists" *)
    (*   FPCA '95.                               *)
struct
    datatype 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
    type 'a ralist = (int * 'a tree) list
      (* Invariants for [...,(sizei,ti),...] :                        *)
      (*   sizei is the size of ti                                    *)
      (*   size0 <= size1 and rest of sizes are strictly increasing   *)
      (*   each ti is a complete binary tree                          *)
      (*   indices are ordered as the preorder traversal of the trees *)
      (*       from left to right                                     *)

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = []

    fun cons x (xs as ((size1,t1) :: (size2,t2) :: rest)) =
          if size1 = size2 then (1+size1+size2,Node (x,t1,t2)) :: rest
                           else (1,Leaf x) :: xs
      | cons x xs = (1,Leaf x) :: xs

    fun head [] = raise Empty
      | head ((size,Leaf x) :: rest) = x
      | head ((size,Node (x,t1,t2)) :: rest) = x

    fun tail [] = raise Empty
      | tail ((size,Leaf x) :: rest) = rest
      | tail ((size,Node (x,t1,t2)) :: rest) =
          let val size' = size div 2
	  in
	      (size',t1) :: (size',t2) :: rest
	  end

    fun isempty [] = true
      | isempty ((size,t) :: rest) = false

    fun tree_lookup size (Leaf x) 0 = x
      | tree_lookup size (Leaf x) i = raise Subscript
      | tree_lookup size (Node (x,t1,t2)) 0 = x
      | tree_lookup size (Node (x,t1,t2)) i =
          let val size' = size div 2
	  in
	      if i <= size' then tree_lookup size' t1 (i-1)
	                    else tree_lookup size' t2 (i-1-size')
	  end

    fun tree_update size (Leaf x) 0 y = Leaf y
      | tree_update size (Leaf x) i y = raise Subscript
      | tree_update size (Node (x,t1,t2)) 0 y = Node (y,t1,t2)
      | tree_update size (Node (x,t1,t2)) i y =
          let val size' = size div 2
	  in
	      if i <= size' then Node (x,tree_update size' t1 (i-1) y,t2)
                            else Node (x,t1,tree_update size' t2 (i-1-size') y)
	  end

    fun lookup [] i = raise Subscript
      | lookup ((size,t) :: rest) i =
	  if i < size then tree_lookup size t i
	              else lookup rest (i-size)

    fun update [] i y = raise Subscript
      | update ((size,t) :: rest) i y =
	  if i < size then (size,tree_update size t i y) :: rest
	              else (size,t) :: update rest (i-size) y
  

(* Additional efficient operations not described in paper *)

    fun length [] = 0
      | length ((size,t) :: rest) = size + length rest

    fun create x n =
	  (* make a list of all trees up to size n, then select *)
          (* those trees that form the greedy decomposition     *)
	  let fun make size t rest =
	            if size > n then rest
		    else make (1+size+size) (Node (x,t,t)) ((size,t) :: rest)
	      fun select 0 _ xs = xs
		| select m [] xs = raise Subscript
		| select m (r as (size,t) :: rest) xs = 
		    if m < size then select m rest xs
		    else select (m-size) ((size,t) :: rest) ((size,t) :: xs)
          in
	      select n (make 1 (Leaf x) []) []
	  end

    fun tree_drop size t 0 rest = (size,t) :: rest
      | tree_drop size (Leaf x) 1 rest = rest
      | tree_drop size (Leaf x) i rest = raise Subscript
      | tree_drop size (Node (x,t1,t2)) i rest =
          let val size' = size div 2
	  in
	      if i <= size' then tree_drop size' t1 (i-1) ((size',t2) :: rest)
                            else tree_drop size' t2 (i-1-size') rest
	  end

    fun drop xs 0 = xs
      | drop [] i = raise Subscript
      | drop ((size,t) :: rest) i =
	  if i < size then tree_drop size t i rest
	              else drop rest (i-size)

(* PL research report functions/values *)
    (* returns a random access list given a standard list *)
    fun listToRal list =
      (case list of
         nil => empty
       | (h::t) => cons h (listToRal t))

    (* given a positive int i, generates a list of increasing numbers from 1 to i *)
    fun generateLongList i =
      (case i of
         0 => nil
       | a => generateLongList(i-1)@a::nil)

    (* given a positive int i, generates a list of decreasing numbers from 1 to i *)
    fun generateRaList i =
      (case i of
         0 => empty
       | a => cons a (generateRaList (a-1)))

    (* given a positive int i, generates a list of decreasing real numbers from 1 to i *)
    fun generateRealRaList i =
      (case i of
         0 => empty
       | a => cons (Real.fromInt a) (generateRealRaList (a-1)))

    (* returns the time in nanoseconds required to look up an index n 
       in a given standard list *)
    fun lookup_time_list (lst, n) = 
      let 
        val t = Timer.startCPUTimer() 
      in 
        (List.nth (lst, n)); 
        Time.toNanoseconds(#usr(Timer.checkCPUTimer(t))) 
      end

    (* returns the time in nanoseconds required to look up an index n 
       in a given random access list *)
    fun lookup_time_ral (ralst, n) = 
      let 
        val t = Timer.startCPUTimer() 
      in 
        lookup ralst n; 
        Time.toNanoseconds(#usr(Timer.checkCPUTimer(t))) 
      end
end
