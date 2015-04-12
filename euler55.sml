(* move to number_utils, refactor euler92 *)

use "list-utils.sml";
use "print-utils.sml";
use "number-utils.sml";

fun revAndAdd n = let
    val ds = NumberUtils.largeIntToDigits n
    val nrev = NumberUtils.digitsToLargeInt (List.rev ds)
in
    n + nrev
end;

fun isLychrel n = let
    fun is_lychrel_iter n iter =
      if iter >= 50 then true
      else let
          val n' = (revAndAdd n)
          val ds = NumberUtils.largeIntToDigits n'
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
