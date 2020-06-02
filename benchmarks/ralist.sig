signature RANDOM_ACCESS_LIST =
sig
    type 'a ralist

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty   : 'a ralist
    val cons    : 'a -> 'a ralist -> 'a ralist
    val head    : 'a ralist -> 'a
    val tail    : 'a ralist -> 'a ralist

    val isempty : 'a ralist -> bool
    val length  : 'a ralist -> int

    val lookup  : 'a ralist -> int -> 'a
    val update  : 'a ralist -> int -> 'a -> 'a ralist

(* ExtendedML axioms
    head empty = raise Empty
    head (cons x xs) = x

    tail empty = raise Empty
    tail (cons x xs) = xs

    isempty empty = true
    isempty (cons x xs) = false

    length empty = 0
    length (cons x xs) = 1 + length xs

    lookup empty i = raise Subscript
    lookup (cons x xs) 0 = x
    lookup (cons x xs) i = lookup xs (i-1)  when i <> 0

    update empty i x = raise Subscript
    update (cons x xs) 0 y = cons y xs
    update (cons x xs) i y = cons x (update xs (i-1) y)  when i <> 0
*)
end
