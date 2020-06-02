structure AVLList : RANDOM_ACCESS_LIST =
    (* see                                  *)
    (*   Eugene Myers                       *)
    (*   "Efficient Applicative Data Types" *)
    (*   POPL '84                           *)
struct
    datatype balance = L  (* depth left subtree = depth right subtree + 1 *)
                     | B  (* depth left subtree = depth right subtree     *)
                     | R  (* depth left subtree = depth right subtree - 1 *)

    datatype 'a tree = Nil | Node of balance * int * 'a * 'a tree * 'a tree
    type 'a ralist = 'a tree
      (* Node (_,n,x,a,b) :                                    *)
      (*   n = size a, which also happens to be the index of x *)
      (*                                                       *)
      (* Because we only add/remove elements at the left, only *)
      (* the following rotations are ever required:            *)
      (*   cons:                                               *)
      (*     Node (L,_,y,Node (L,_,z,a1,a2),b)                 *)
      (*       => Node (B,_,z,a1,Node (B,_,y,a2,b))            *)
      (*   tail:                                               *)
      (*     Node (R,_,y,a,Node (R,_,z,b1,b2))                 *)
      (*       => Node (B,_,z,Node (B,_,y,a,b1),b2)            *)
      (*     Node (R,_,y,a,Node (B,_,z,b1,b2))                 *)
      (*       => Node (L,_,z,Node (R,_,y,a,b1),b2)            *)

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = Nil

    fun cons x xs =
          let 
            (* ins a returns (true,a') if a' is deeper than a *)
            fun ins Nil = (true,Node (B,0,x,Nil,Nil))
	      | ins (Node (bal,n,y,a,b)) =
                  case (ins a,bal) of
		    ((false,a'),_) => (false,Node (bal,n+1,y,a',b))
		  | ((true, a'),R) => (false,Node (B,n+1,y,a',b))
		  | ((true, a'),B) => (true, Node (L,n+1,y,a',b))
		  | ((true, a'),L) => (* do rotation *)
                      let (* a' must be left-balanced node *)
                        val (Node (_,m,z,a1,a2)) = a'
                      in
			(false,Node (B,m,z,a1,Node (B,n-m,y,a2,b)))
                      end
	  in
	      #2 (ins xs)
	  end

    fun head Nil = raise Empty
      | head (Node (_,0,x,a,b)) = x
      | head (Node (_,n,x,a,b)) = head a

    fun tail xs =
	  let
	    (* del a returns (true,a') if a' is shallower than a *)
	    fun del Nil = raise Empty
	      | del (Node (_,0,x,a,b)) = (true,b)
	      | del (Node (bal,n,x,a,b)) =
                  case (del a,bal) of
		    ((false,a'),_) => (false,Node (bal,n-1,x,a',b))
		  | ((true, a'),L) => (true, Node (B,n-1,x,a',b))
		  | ((true, a'),B) => (false,Node (R,n-1,x,a',b))
		  | ((true, a'),R) => (* do rotation *)
                      (case b of (* b must have balance R or B *)
		         Node (R,m,y,b1,b2) =>
			   (true, Node (B,n+m,y,Node (B,n-1,x,a',b1),b2))
		       | Node (B,m,y,b1,b2) =>
			   (false,Node (L,n+m,y,Node (R,n-1,x,a',b1),b2)))
	  in
	      #2 (del xs)
	  end

    fun isempty Nil = true
      | isempty (Node (_,n,x,a,b)) = false

    fun length Nil = 0
      | length (Node (_,n,x,a,b)) = 1 + n + length b

    fun lookup Nil i = raise Subscript
      | lookup (Node (_,n,x,a,b)) i =
          if i = n then x
          else if i < n then lookup a i
                        else lookup b (i-n-1)

    fun update Nil i y = raise Subscript
      | update (Node (bal,n,x,a,b)) i y =
          if i = n then Node (bal,n,y,a,b)
          else if i < n then Node (bal,n,x,update a i y,b)
                        else Node (bal,n,x,a,update b (i-n-1) y)
end
