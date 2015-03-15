fun print1 a =
  print (a ^ "\n")

fun print2 a b =
  print (a ^ " " ^ b ^ "\n")

fun print3 a b c =
  print (a ^ " " ^ b ^ " " ^ c ^ "\n")

(*
-digits 1234;
val it = [1,2,3,4] : int list
*)
fun digits x = let
    fun digits_acc x acc =
      case (x div 10, x mod 10)
       of (0, r) => r::acc
        | (d, r) => digits_acc d (r::acc)
in
    digits_acc x []
end

(*
- seq 1 10 1;
val it = [1,2,3,4,5,6,7,8,9,10] : int list
- seq 10 1 ~1;
val it = [10,9,8,7,6,5,4,3,2,1] : int list
*)
fun seq m n s = let
    val cmp = if Int.sign s = 1
              then op>
              else op<
    fun seq_acc x acc =
      if cmp (x, n) then List.rev acc
      else seq_acc (x+s) (x::acc)
in
    seq_acc m []
end

fun sum l = List.foldl op+ 0 l
fun prod l = List.foldl op* 1 l

fun squareOfDigits n =
  sum (List.map (fn x => x*x) (digits n))

fun squareDigitsChain n =
  case squareOfDigits n
   of 1 => 1
    | 89 => 89
    | n' => squareDigitsChain n'

fun solve () = let
    val xs = seq 1 9999999 1
    val chains = List.map squareDigitsChain xs
in
    List.length (List.filter (fn x => x=89) chains)
end

val ans = solve ()
val _ = print1 (Int.toString ans)
val _ = OS.Process.exit(OS.Process.success)
