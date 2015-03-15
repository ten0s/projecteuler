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

fun squareOfDigits n =
  ListUtils.sum (List.map (fn x => x*x) (digits n))

fun squareDigitsChain n =
  case squareOfDigits n
   of 1 => 1
    | 89 => 89
    | n' => squareDigitsChain n'

fun solve () = let
    val xs = ListUtils.seq 1 9999999 1
    val chains = List.map squareDigitsChain xs
in
    List.length (List.filter (fn x => x=89) chains)
end

structure Main = struct
  fun main (_, _) = let
      val ans = solve ()
      val _ = PrintUtils.printLn (Int.toString ans)
  in
      0
  end
end
