use "list-utils.sml";
use "print-utils.sml";
use "number-utils.sml";

structure Fraction = struct
  type int = LargeInt.int
  type fraction = (int * int)

  fun gcd (m, 0) = abs(m)
    | gcd (m:int, n:int) = gcd(n, m mod n)

  fun mkFraction (n, d):fraction = let
      val g = gcd (n, d)
      val n' = n div g
      val d' = d div g
  in
      (n', d')
  end

  fun fromInt n = (n, LargeInt.fromInt 1)

  fun add ((n, d):fraction) ((n', d'):fraction) = (n * d' + d * n', d * d')
  fun rev (n, d) = (d, n)
end;

fun sqrtOfTwo depth = let
    fun fraction d =
      if d = 0 then Fraction.fromInt 0
      else Fraction.rev (Fraction.add (Fraction.fromInt 2) (fraction (d-1)))
    val (num, denum) = Fraction.add (Fraction.fromInt 1) (fraction depth)
in
    Fraction.mkFraction (num, denum)
end;

fun isLongerNumerator (n, d) = let
    val nlen = List.length (NumberUtils.largeIntToDigits n)
    val dlen = List.length (NumberUtils.largeIntToDigits d)
in
    nlen > dlen
end;

fun solve () = let
    val ds = ListUtils.seq 1 1000 1
    val list = List.filter (isLongerNumerator o sqrtOfTwo) ds
in
    List.length list
end;

structure Main = struct
  fun main (_, _) = let
      val ans = solve ()
      val _ = PrintUtils.printLn (Int.toString ans)
  in
      0
  end
end;

use "smlunit.sml";

SMLUnit.assertEqual Fraction.mkFraction (1, 2) (1, 2);
SMLUnit.assertEqual Fraction.mkFraction (2, 4) (1, 2);
SMLUnit.assertEqual Fraction.fromInt 2 (2, 1);
SMLUnit.assertEqual (fn _ => (Fraction.add (3, 4) (5, 7))) () (41, 28);
SMLUnit.assertEqual sqrtOfTwo 8 (1393, 985);
