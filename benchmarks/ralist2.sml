structure OptRandomAccessList : RANDOM_ACCESS_LIST =
    (* Optimized version of RandomAccessList                  *)
    (*   -- replaces lists of pairs with custom datastructure *)
    (*   -- replaces division with bit shifts                 *)
struct
    datatype 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
    datatype 'a ralist = Nil | Root of int * 'a tree * 'a ralist

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = Nil

    fun cons x (xs as Root (size1,t1,Root (size2,t2,rest))) =
          if size1 = size2 then Root (1+size1+size2,Node (x,t1,t2),rest)
                           else Root (1,Leaf x,xs)
      | cons x xs = Root (1,Leaf x,xs)

    fun head Nil = raise Empty
      | head (Root (size,Leaf x,rest)) = x
      | head (Root (size,Node (x,t1,t2),rest)) = x

    fun tail Nil = raise Empty
      | tail (Root (size,Leaf x,rest)) = rest
      | tail (Root (size,Node (x,t1,t2),rest)) =
          let val size' = Bits.rshift (size,1)
	  in
	      Root (size',t1,Root (size',t2,rest))
	  end

    fun isempty Nil = true
      | isempty (Root (size,t,rest)) = false

    fun length Nil = 0
      | length (Root (size,t,rest)) = size + length rest

    fun tree_lookup size (Leaf x) 0 = x
      | tree_lookup size (Leaf x) i = raise Subscript
      | tree_lookup size (Node (x,t1,t2)) 0 = x
      | tree_lookup size (Node (x,t1,t2)) i =
          let val size' = Bits.rshift (size,1)
	  in
	      if i <= size' then tree_lookup size' t1 (i-1)
	                    else tree_lookup size' t2 (i-1-size')
	  end

    fun tree_update size (Leaf x) 0 y = Leaf y
      | tree_update size (Leaf x) i y = raise Subscript
      | tree_update size (Node (x,t1,t2)) 0 y = Node (y,t1,t2)
      | tree_update size (Node (x,t1,t2)) i y =
          let val size' = Bits.rshift (size,1)
	  in
	      if i <= size' then Node (x,tree_update size' t1 (i-1) y,t2)
                            else Node (x,t1,tree_update size' t2 (i-1-size') y)
	  end

    fun lookup Nil i = raise Subscript
      | lookup (Root (size,t,rest)) i =
	  if i < size then tree_lookup size t i
	              else lookup rest (i-size)

    fun update Nil i y = raise Subscript
      | update (Root (size,t,rest)) i y =
	  if i < size then Root (size,tree_update size t i y,rest)
	              else Root (size,t,update rest (i-size) y)
end
