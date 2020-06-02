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

    val lookup  : 'a ralist -> int -> 'a
    val update  : 'a ralist -> int -> 'a -> 'a ralist

(* Additional efficient operations not described in paper *)
    val length  : 'a ralist -> int
    val create  : 'a -> int -> 'a ralist
    val drop    : 'a ralist -> int -> 'a ralist

(* PL research report functions/values *)
    val listToRal : 'a list -> 'a ralist
    val generateLongList : int -> int list

    val time_lookup_list: unit
    val time_lookup_ral: unit

(* ExtendedML axioms
    head empty = raise Empty
    head (cons x xs) = x

    tail empty = raise Empty
    tail (cons x xs) = xs

    isempty empty = true
    isempty (cons x xs) = false

    lookup empty i = raise Subscript
    lookup (cons x xs) 0 = x
    lookup (cons x xs) i = lookup xs (i-1)  when i <> 0

    update empty i x = raise Subscript
    update (cons x xs) 0 y = cons y xs
    update (cons x xs) i y = cons x (update xs (i-1) y)  when i <> 0

    length empty = 0
    length (cons x xs) = 1 + length xs

    create x 0 = empty
    create x n = cons x (create x (n-1))

    drop xs 0 = xs
    drop xs i = tail (drop xs (i-1))
*)
end
