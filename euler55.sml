(* move to number_utils, refactor euler92 *)

use "list-utils.sml";
use "print-utils.sml";

fun largeIntToDigits (x:LargeInt.int) = let
    fun digits_acc x acc =
      case (x div 10, x mod 10)
       of (0, r) => (Int.fromLarge r)::acc
        | (d, r) => digits_acc d ((Int.fromLarge r)::acc)
in
    digits_acc x []
end;

fun digitsToLargeInt (ds:int list):LargeInt.int = let
    fun to_int_acc [] _ acc = acc
      | to_int_acc (x::xs) p acc = let
          val power = Math.pow (10.0, p)
      in
          to_int_acc xs (p + 1.0) ((Real.fromInt x) * power + acc)
      end
in
    Real.toLargeInt IEEEReal.TO_NEAREST (to_int_acc (List.rev ds) 0.0 0.0)
end;

fun revAndAdd n = let
    val ds = largeIntToDigits n
    val nrev = digitsToLargeInt (List.rev ds)
in
    n + nrev
end;

fun isLychrel n = let
    fun is_lychrel_iter n iter =
      if iter >= 50 then true
      else let
          val n' = (revAndAdd n)
          val ds = largeIntToDigits n'
      in
          (*PrintUtils.printLn (Int.toString n'); *)
          if ListUtils.isPalindrom ds then false
          else is_lychrel_iter n' (iter+1)
      end
in
    is_lychrel_iter (LargeInt.fromInt n) 1
end;

(* TESTS BEGIN *)
use "smlunit.sml";

SMLUnit.assertEqual largeIntToDigits 1234 [1,2,3,4];
SMLUnit.assertEqual digitsToLargeInt [1,2,3,4] 1234;
SMLUnit.assertNot isLychrel 47;
SMLUnit.assertNot isLychrel 349;
SMLUnit.assert isLychrel 196;

fun solve () = let
    val ns = ListUtils.seq 1 10000 1
    val lychrels = List.filter isLychrel ns
in
    List.length lychrels
end

structure Main = struct
  fun main (_, _) = let
      val ans = solve ()
      val _ = PrintUtils.printLn (Int.toString ans)
  in
      0
  end
end
