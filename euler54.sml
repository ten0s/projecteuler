datatype suit = H | D | C | S;
exception IllegalRank
exception InvalidHand

structure Card = struct
  type rank = int
  type card = rank * suit

  fun mkCard (r, s):card =
    if r >= 2 andalso r <= 14 then
        (r, s)
    else
        raise IllegalRank

  fun compare ((r1, _):card, (r2, _):card) = Int.compare (r1, r2)

  val rs = [(#"2", 2), (#"3", 3), (#"4", 4), (#"5", 5),
            (#"6", 6), (#"7", 7), (#"8", 8), (#"9", 9),
            (#"T", 10), (#"J", 11), (#"Q", 12), (#"K", 13),
            (#"A", 14)]

  val ss = [(#"H", H), (#"D", D), (#"C", C), (#"S", S)]

  fun rankByChar (key:char):rank option =
    case List.find (fn (k, _) => k = key) rs
     of SOME (_, r) => SOME r
      | NONE => NONE

  fun charByRank (key:rank):char option =
    case List.find (fn (_, k) => k = key) rs
     of SOME (c, _) => SOME c
      | NONE => NONE

  fun suitByChar (key:char):suit option =
    case List.find (fn (k, _) => k = key) ss
     of SOME (_, s) => SOME s
      | NONE => NONE

  fun charBySuit (key:suit):char option =
    case List.find (fn (_, k) => k = key) ss
     of SOME (c, _) => SOME c
      | NONE => NONE

  fun fromString (str:string):card option =
    if (String.size str) <> 2 then
        NONE
    else let
        val [rc, sc] = String.explode str
    in
        case (rankByChar rc, suitByChar sc)
         of (NONE, _) => NONE
          | (_, NONE) => NONE
          | (SOME r, SOME s)  => SOME (r, s)
    end

  fun toString ((r, s):card):string = let
      val rc = valOf (charByRank r)
      val sc = valOf (charBySuit s)
  in
      String.implode [rc, sc]
  end
end;

(*
    High Card: Highest value card.
    One Pair: Two cards of the same value.
    Two Pairs: Two different pairs.
    Three of a Kind: Three cards of the same value.
    Straight: All cards are consecutive values.
    Flush: All cards of the same suit.
    Full House: Three of a kind and a pair.
    Four of a Kind: Four cards of the same value.
    Straight Flush: All cards are consecutive values of same suit.
    Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
*)

structure Hand = struct
  type card = Card.card
  type hand = card * card * card * card * card

  fun mkHand (c1, c2, c3, c4, c5):hand = let
      fun cmp ((r1, _), (r2, _)) = r1 < r2
      val [c1', c2', c3', c4', c5'] = ListMergeSort.sort cmp [c1, c2, c3, c4, c5]
  in
    (c1', c2', c3', c4', c5')
  end

  fun fromString (str:string):hand option = let
      val strList = String.tokens (fn c => c = #" ") str
      val cards = List.mapPartial Card.fromString strList
  in
      if List.length cards <> 5 then
          NONE
      else let
          val [c1, c2, c3, c4, c5] = cards
      in
          SOME (mkHand (c1, c2, c3, c4, c5))
      end
  end

  fun toString (hand):string = let
      val (c1, c2, c3, c4, c5) = hand
      val cards = [c1, c2, c3, c4, c5]
      val strList = List.map Card.toString cards
  in
    String.concatWith " " strList
  end
end;


(* TESTS *)
use "smlunit.sml";

SMLUnit.assertThrow Card.mkCard (1,H) IllegalRank;
SMLUnit.assertThrow Card.mkCard (15,H) IllegalRank;
SMLUnit.assertEqual Card.fromString "" NONE;
SMLUnit.assertEqual Card.fromString "ABC" NONE;
SMLUnit.assertEqual Card.fromString "1H" NONE;
SMLUnit.assertEqual Card.fromString "2E" NONE;
SMLUnit.assertEqual Card.fromString "BH" NONE;
SMLUnit.assertEqual Card.fromString "2H" (SOME (Card.mkCard (2,H)));
SMLUnit.assertEqual Card.fromString "AH" (SOME (Card.mkCard (14,H)));
SMLUnit.assertEqual Hand.fromString "5D 8C 9S JS AC" (SOME (Card.mkCard (14,C),
                                                            Card.mkCard (11,S),
                                                            Card.mkCard (9,S),
                                                            Card.mkCard (8,C),
                                                            Card.mkCard (5,D)));
SMLUnit.assertEqual Hand.toString (valOf (Hand.fromString "5D 8C 9S JS AC")) "AC JS 9S 8C 5D";

(*
5D 8C 9S JS AC Highest card Ace
5H 5C 6S 7S KD Pair of Fives
5H 5C 6S 7S 7D Two Pairs
2D 9C AS AH AC Three Aces
2H 3D 4C 5S 6H Straight == 2,3,4,5,6
3D 6D 7D TD QD Flush with Diamonds == the same suit
2H 2D 4C 4D 4S Full House With Three Fours == Three and Pair
2D AH AD AC AS Four of Aces
2H 3H 4H 5H 6H Straight Flush == 2,3,4,5,6 of Hearts
TC JC QC KC AC Royal Flush == 10,J,Q,K,A of Clubs
4D 6S 9H QH QC Pair of Queens Highest card Nine
3D 6D 7H QD QS Pair of Queens Highest card Seven
*)

(*
structure Main = struct
  fun main (_, _) = let
      val ans = solve ()
      val _ = PrintUtils.printLn (Int.toString ans)
  in
      0
  end
end
*)
