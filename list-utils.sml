structure ListUtils = struct
(*
- seq 1 10 1;
val it = [1,2,3,4,5,6,7,8,9,10] : int list
- seq 10 1 ~1;
val it = [10,9,8,7,6,5,4,3,2,1] : int list
*)
  fun seq m n s = let
      val cmp = if Int.sign s = 1 then op> else op<
      fun seq_acc x acc =
        if cmp (x, n) then List.rev acc
        else seq_acc (x+s) (x::acc)
  in
      seq_acc m []
  end
  fun sum l = List.foldl op+ 0 l
  fun prod l = List.foldl op* 1 l

  fun isPalindrom l = (List.rev l) = l
end
