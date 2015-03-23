type rank = int;
datatype suit = H | D | C | S;

datatype category = RoyalFlush
                  | StraightFlush of rank
                  | Four of rank
                  | FullHouse of (rank * rank)
                  | Flush
                  | Straight of rank
                  | Three of rank
                  | TwoPairs of (rank * rank)
                  | Pair of rank
                  | HighCard;

exception IllegalRank
exception InvalidHand

structure Card = struct
  type card = rank * suit

  fun mkCard (r, s):card =
    if r >= 2 andalso r <= 14 then
        (r, s)
    else
        raise IllegalRank

  fun rank (c:card):rank = let
      val (r, _) = c
  in
      r
  end

  fun suit (c:card):suit = let
      val (_, s) = c
  in
      s
  end

  fun compare (c1, c2) = Int.compare (rank c1, rank c2)

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

  fun toString ((c1, c2, c3, c4, c5):hand):string = let
      val cards = [c1, c2, c3, c4, c5]
      val strList = List.map Card.toString cards
  in
    String.concatWith " " strList
  end

  fun isFlush ((c1, c2, c3, c4, c5):hand):bool = let
      val s = Card.suit c1
  in
      Card.suit c2 = s andalso
      Card.suit c3 = s andalso
      Card.suit c4 = s andalso
      Card.suit c5 = s
  end

  fun isStraight ((c1, c2, c3, c4, c5):hand):rank option = let
      val r1 = Card.rank c1
  in
      if Card.rank c2 = r1 - 1 andalso
         Card.rank c3 = r1 - 2 andalso
         Card.rank c4 = r1 - 3 andalso
         Card.rank c5 = r1 - 4
      then
          SOME (Card.rank c1)
      else
          NONE
  end

  fun isFour ((c1, c2, c3, c4, c5):hand):rank option = let
      val r1 = Card.rank c1
      val r2 = Card.rank c2
      val r3 = Card.rank c3
      val r4 = Card.rank c4
      val r5 = Card.rank c5
  in
      if (r1 = r2 andalso
          r2 = r3 andalso
          r3 = r4) orelse
         (r2 = r3 andalso
          r3 = r4 andalso
          r4 = r5)
      then
          SOME r3
      else
          NONE
  end

  fun isFullHouse ((c1, c2, c3, c4, c5):hand):(rank * rank) option = let
      val r1 = Card.rank c1
      val r2 = Card.rank c2
      val r3 = Card.rank c3
      val r4 = Card.rank c4
      val r5 = Card.rank c5
  in
      if (r1 = r2 andalso
          r2 = r3 andalso
          r4 = r5)
      then
          SOME (r1, r4)
      else
          if (r1 = r2 andalso
              r3 = r4 andalso
              r4 = r5)
          then
              SOME (r3, r1)
          else
              NONE
  end

  fun isThree ((c1, c2, c3, c4, c5):hand):rank option = let
      val r1 = Card.rank c1
      val r2 = Card.rank c2
      val r3 = Card.rank c3
      val r4 = Card.rank c4
      val r5 = Card.rank c5
  in
      if (r1 = r2 andalso
          r2 = r3) orelse
         (r2 = r3 andalso
          r3 = r4) orelse
         (r3 = r4 andalso
          r4 = r4)
      then
          SOME r3
      else
          NONE
  end

  fun isTwoPairs((c1, c2, c3, c4, c5):hand):(rank * rank) option = let
      val r1 = Card.rank c1
      val r2 = Card.rank c2
      val r3 = Card.rank c3
      val r4 = Card.rank c4
      val r5 = Card.rank c5
  in
      if r1 = r2 andalso r3 = r4 then SOME (r1, r3)
      else if r1 = r2 andalso r4 = r5 then SOME (r1, r4)
      else if r2 = r3 andalso r4 = r5 then SOME (r2, r4)
      else NONE
  end

  fun isPair ((c1, c2, c3, c4, c5):hand):rank option = let
      val r1 = Card.rank c1
      val r2 = Card.rank c2
      val r3 = Card.rank c3
      val r4 = Card.rank c4
      val r5 = Card.rank c5
  in
      if r1 = r2 then SOME r1
      else if r2 = r3 then SOME r2
      else if r3 = r4 then SOME r3
      else if r4 = r5 then SOME r4
      else NONE
  end

  fun category (h:hand):category = let
      val flush = isFlush h
  in
      case isStraight h
       of SOME r => if flush then
                        if r = 14 then RoyalFlush
                        else StraightFlush r
                    else Straight r
        | NONE => case isFour h
                   of SOME r => Four r
                    | NONE => (case isFullHouse h
                                of SOME (r, r') => FullHouse (r, r')
                                 | NONE => if flush then Flush
                                           else (case isThree h
                                                  of SOME r => Three r
                                                  | NONE => (case isTwoPairs h
                                                              of SOME (r, r') => TwoPairs (r, r')
                                                              | NONE => (case isPair h
                                                                          of SOME r => Pair r
                                                                           | NONE => HighCard))))
  end
end;

(* TESTS *)
use "smlunit.sml";

(* Card tests *)
SMLUnit.assertThrow Card.mkCard (1,H) IllegalRank;
SMLUnit.assertThrow Card.mkCard (15,H) IllegalRank;
SMLUnit.assertEqual Card.fromString "" NONE;
SMLUnit.assertEqual Card.fromString "ABC" NONE;
SMLUnit.assertEqual Card.fromString "1H" NONE;
SMLUnit.assertEqual Card.fromString "2E" NONE;
SMLUnit.assertEqual Card.fromString "BH" NONE;
SMLUnit.assertEqual Card.fromString "2H" (SOME (Card.mkCard (2,H)));
SMLUnit.assertEqual Card.fromString "AH" (SOME (Card.mkCard (14,H)));

(* Hand tests *)
val hand = (Card.mkCard (14,C),
            Card.mkCard (11,S),
            Card.mkCard (9,S),
            Card.mkCard (8,C),
            Card.mkCard (5,D));
SMLUnit.assertEqual Hand.fromString "5D 8C 9S JS AC" (SOME hand);
SMLUnit.assertEqual Hand.toString hand "AC JS 9S 8C 5D";

(* Hand categories *)
val royalFlush = valOf (Hand.fromString "TC JC QC KC AC");
SMLUnit.assertEqual Hand.category royalFlush RoyalFlush;

val straightFlush = valOf (Hand.fromString "2H 3H 4H 5H 6H");
SMLUnit.assertEqual Hand.category straightFlush (StraightFlush 6);

val straight = valOf (Hand.fromString "9S 8D 7S 6H 5C");
SMLUnit.assertEqual Hand.category straight (Straight 9);

val fourA = valOf (Hand.fromString "2D AH AD AC AS");
SMLUnit.assertEqual Hand.category fourA (Four 14);

val four2 = valOf (Hand.fromString "AD 2H 2D 2C 2S");
SMLUnit.assertEqual Hand.category four2 (Four 2);

val fullHouse = valOf (Hand.fromString "2H 2D 4C 4D 4S");
SMLUnit.assertEqual Hand.category fullHouse (FullHouse (4, 2));

val flush = valOf (Hand.fromString "3D 6D 7D TD QD");
SMLUnit.assertEqual Hand.category flush Flush;

val three = valOf (Hand.fromString "2D 9C AS AH AC");
SMLUnit.assertEqual Hand.category three (Three 14);

val twoPairs = valOf (Hand.fromString "5H 5C 6S 7S 7D");
SMLUnit.assertEqual Hand.category twoPairs (TwoPairs (7, 5));

val pair = valOf (Hand.fromString "5H 5C 6S 7S KD");
SMLUnit.assertEqual Hand.category pair (Pair 5);

val highCard = valOf (Hand.fromString "5D 8C 9S JS AC");
SMLUnit.assertEqual Hand.category highCard HighCard;

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
