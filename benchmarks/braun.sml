structure BraunList : RANDOM_ACCESS_LIST =
    (* See                                                 *)
    (*   Rob Hoogerwoord                                   *)
    (*   "A logarithmic implementation of flexible arrays" *)
    (*   Mathematics of Program Construction '92           *)
struct
    datatype 'a tree = Nil | Node of 'a * 'a tree * 'a tree
    type 'a ralist = 'a tree
      (* Element 0 is stored at the root.               *)
      (* Odd elements are stored in the left subtree.   *)
      (* Even elements are stored in the right subtree. *)
      (*                                                *)
      (* Node that                                      *)
      (*   size odds = size evens or                    *)
      (*   size odds = size evens + 1                   *)
      (* so                                             *)
      (*   odds = Nil => evens = Nil                    *)

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = Nil

    fun cons x Nil = Node (x,Nil,Nil)
      | cons x (Node (y,odds,evens)) = Node (x,cons y evens,odds)

    fun head Nil = raise Empty
      | head (Node (x,odds,evens)) = x

    fun tail Nil = raise Empty
      | tail (Node (x,Nil,evens)) = Nil
      | tail (Node (x,odds,evens)) = Node (head odds,evens,tail odds)

    fun isempty Nil = true
      | isempty (Node (x,odds,evens)) = false

    fun length Nil = 0
      | length (Node (x,odds,evens)) = 1 + length odds + length evens

    fun odd n = Bits.andb (n,1) = 1
    fun half n = Bits.rshift (n,1)

    fun lookup Nil i = raise Subscript
      | lookup (Node (x,odds,evens)) 0 = x
      | lookup (Node (x,odds,evens)) i =
          if odd i then lookup odds (half i)
                   else lookup evens (half i - 1)

    fun update Nil i y = raise Subscript
      | update (Node (x,odds,evens)) 0 y = Node (y,odds,evens)
      | update (Node (x,odds,evens)) i y =
          if odd i then Node (x,update odds (half i) y,evens)
                   else Node (x,odds,update evens (half i - 1) y)

end
