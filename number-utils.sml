structure NumberUtils = struct
  fun largeIntToDigits (x:LargeInt.int) = let
      fun digits_acc x acc =
        case (x div 10, x mod 10)
         of (0, r) => (Int.fromLarge r)::acc
          | (d, r) => digits_acc d ((Int.fromLarge r)::acc)
  in
      digits_acc x []
  end

  fun digitsToLargeInt (ds:int list):LargeInt.int = let
      fun to_int_acc [] _ acc = acc
        | to_int_acc (x::xs) p acc = let
            val power = Math.pow (10.0, p)
        in
            to_int_acc xs (p + 1.0) ((Real.fromInt x) * power + acc)
        end
  in
      Real.toLargeInt IEEEReal.TO_NEAREST (to_int_acc (List.rev ds) 0.0 0.0)
  end
end;

use "smlunit.sml";

SMLUnit.assertEqual NumberUtils.largeIntToDigits 1234 [1,2,3,4];
SMLUnit.assertEqual NumberUtils.digitsToLargeInt [1,2,3,4] 1234;
