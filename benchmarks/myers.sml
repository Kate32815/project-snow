structure MyersList : RANDOM_ACCESS_LIST =
    (* See                                    *)
    (*   Eugene W. Myers                      *)
    (*   "An Applicative Random-Access Stack" *)
    (*   IPL 17(5):241-248 (Dec 83)           *)
struct
    datatype 'a ralist = Nil | Cons of 'a * 'a ralist * int * 'a ralist
      (* The four fields of Cons (x,xs,r,xs') are                   *)
      (*   x   -- the head of the list                              *)
      (*   xs  -- the tail of the list                              *)
      (*   r   -- the "rank" of the list                            *)
      (*   xs' -- a "shortcut" pointer that skips r positions ahead *)
      (*                                                            *)
      (* Note that this is slightly optimized from Myers' presen-   *)
      (* tation which had a "length" field instead of a rank.  In   *)
      (* that paper, rank was computed as the difference between    *)
      (* the length of the current list and the length of xs'.      *)

    exception Empty      (* raised by head, tail *)
    exception Subscript  (* raised by lookup, update *)

    val empty = Nil

    fun cons x (xs as Cons (_,_,r,Cons (_,_,r',xs'))) =
          if r = r' then Cons (x,xs,1+r+r',xs')
                    else Cons (x,xs,1,xs)
      | cons x xs = Cons (x,xs,1,xs)

    fun head Nil = raise Empty
      | head (Cons (x,xs,_,_)) = x

    fun tail Nil = raise Empty
      | tail (Cons (x,xs,_,_)) = xs

    fun isempty Nil = true
      | isempty _ = false

    fun length Nil = 0
      | length (Cons (x,xs,r,xs')) = r + length xs'

    fun lookup Nil i = raise Subscript
      | lookup (Cons (x,xs,_,_)) 0 = x
      | lookup (Cons (x,xs,r,xs')) i =
          if i < r then lookup xs (i-1)
                   else lookup xs' (i-r)

    fun update Nil i y = raise Subscript
      | update (Cons (x,xs,r,xs')) 0 y = Cons (y,xs,r,xs')
      | update (Cons (x,xs,_,_)) i y = cons x (update xs (i-1) y)
end
