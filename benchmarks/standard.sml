structure StandardList : RANDOM_ACCESS_LIST =
struct
    type 'a ralist = 'a list

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = []

    fun cons x xs = x :: xs

    fun head [] = raise Empty
      | head (x::xs) = x

    fun tail [] = raise Empty
      | tail (x::xs) = xs

    fun isempty [] = true
      | isempty (x::xs) = false

    fun length [] = 0
      | length (x::xs) = 1 + length xs

    fun lookup [] i = raise Subscript
      | lookup (x::xs) 0 = x
      | lookup (x::xs) i = lookup xs (i-1)

    fun update [] i y = raise Subscript
      | update (x::xs) 0 y = y :: xs
      | update (x::xs) i y = x :: update xs (i-1) y
end
