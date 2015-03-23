(* http://abstractfactory.blogspot.com/2006/05/sml-hacking-tip-turn-off-polyequal.html *)
Control.polyEqWarn := false;

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

datatype outcome = First
                 | Second
                 | Tie;

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

  fun compare (c1:card, c2:card) = Int.compare (rank c1, rank c2)

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

  fun compare (h1:hand, h2:hand) = let
      val (c11, c12, c13, c14, c15) = h1
      val (c21, c22, c23, c24, c25) = h2
  in
      case Card.compare (c11, c12)
       of EQUAL => (case Card.compare (c12, c22)
                     of EQUAL => (case Card.compare (c13, c23)
                                   of EQUAL => (case Card.compare (c14, c24)
                                                 of EQUAL => Card.compare (c15, c25)
                                                  | res => res)
                                    | res => res)
                      | res => res)
        | res => res
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
          SOME r1
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
          r4 = r5)
      then
          SOME r3
      else
          NONE
  end

  fun isTwoPairs ((c1, c2, c3, c4, c5):hand):(rank * rank) option = let
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

  fun choose ([], []) = Tie
    | choose (l::ls, r::rs) =
      if l > r then First
      else if l < r then Second
      else choose (ls, rs)

  fun highCard (lh, rh) =
    case compare (lh, rh)
     of GREATER => First
      | LESS => Second
      | EQUAL => Tie

  fun chooseOrHighCard (ls, rs) (lh, rh) =
    case choose (ls, rs)
     of Tie => highCard (lh, rh)
      | winner => winner

  fun deal (h1:hand, h2:hand):outcome = let
      val c1 = category h1
      val c2 = category h2
  in
    case (c1, c2)
     of (RoyalFlush, RoyalFlush) => Tie
      | (RoyalFlush, _) => First
      | (_, RoyalFlush) => Second
      | (StraightFlush r1, StraightFlush r2) => choose ([r1], [r2])
      | (StraightFlush _, _) => First
      | (_, StraightFlush _) => Second
      | (Four r1, Four r2) => chooseOrHighCard ([r1], [r2]) (h1, h2)
      | (Four _, _) => First
      | (_, Four _) => Second
      | (FullHouse (r11, r12), FullHouse (r21, r22)) =>
        chooseOrHighCard ([r11, r12], [r21, r22]) (h1, h2)
      | (FullHouse _, _) => First
      | (_, FullHouse _) => Second
      | (Flush, Flush) => highCard (h1, h2)
      | (Flush, _) => First
      | (_, Flush) => Second
      | (Straight r1, Straight r2) => choose ([r1], [r2])
      | (Straight _, _) => First
      | (_, Straight _) => Second
      | (Three r1, Three r2) => chooseOrHighCard ([r1], [r2]) (h1, h2)
      | (Three _, _) => First
      | (_, Three _) => Second
      | (TwoPairs (r11, r12), TwoPairs (r21, r22)) =>
        chooseOrHighCard ([r11, r12], [r21, r22]) (h1, h2)
      | (TwoPairs _, _) => First
      | (_, TwoPairs _) => Second
      | (Pair r1, Pair r2) => chooseOrHighCard ([r1], [r2]) (h1, h2)
      | (Pair _, _) => First
      | (_, Pair _) => Second
      | (HighCard, HighCard) => highCard (h1, h2)
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

(* Hand deals *)
val pairOfFives = valOf (Hand.fromString "5H 5C 6S 7S KD");
val pairOfEights = valOf (Hand.fromString "2C 3S 8S 8D TD");
SMLUnit.assertEqual Hand.deal (pairOfFives, pairOfEights) Second;

val highCardAce = valOf (Hand.fromString "5D 8C 9S JS AC");
val highCardQueen = valOf (Hand.fromString "2C 5C 7D 8S QH");
SMLUnit.assertEqual Hand.deal (highCardAce, highCardQueen) First;

val threeAces = valOf (Hand.fromString "2D 9C AS AH AC");
val flush = valOf (Hand.fromString "3D 6D 7D TD QD");
SMLUnit.assertEqual Hand.deal (threeAces, flush) Second;

val pairOfQueensHighCardNine = valOf (Hand.fromString "4D 6S 9H QH QC");
val pairOfQueensHighCardSeven = valOf (Hand.fromString "3D 6D 7H QD QS");
SMLUnit.assertEqual Hand.deal (pairOfQueensHighCardNine, pairOfQueensHighCardSeven) First;

val fullHouseWithThreeFours = valOf (Hand.fromString "2H 2D 4C 4D 4S");
val fullHouseWithThreeThrees = valOf (Hand.fromString "3C 3D 3S 9S 9D");
SMLUnit.assertEqual Hand.deal (fullHouseWithThreeFours, fullHouseWithThreeThrees) First;

fun readFile () = let
    fun collect_lines_acc fd acc =
      case TextIO.inputLine fd
       of SOME l =>
          collect_lines_acc fd ((String.substring (l, 0, (size l)-1)) :: acc)
        | NONE => List.rev acc
    fun collect_lines (filename) = let
        val fd = TextIO.openIn (filename)
        val lines = collect_lines_acc fd []
    in
        (TextIO.closeIn fd; lines)
    end
in
    collect_lines "files/p054_poker.txt"
end

fun getHands () = let
    fun splitHands s = let
        val s1 = String.substring (s, 0, 14)
        val s2 = String.substring (s, 15, 14)
    in
      (valOf (Hand.fromString s1), valOf (Hand.fromString s2))
    end
    val lines = readFile ()
in
    List.map splitHands lines
end

fun getDeals () = let
    val hands = getHands ()
in
    List.map Hand.deal hands
end

fun solve () = let
    val deals = getDeals ()
in
    List.length (List.filter (fn d => d = First) deals)
end

(*
structure Main = struct
  fun main (_, _) = let
      val res = solve ()
      val _ = PrintUtils.printLn (Int.toString res)
  in
      0
  end
end
*)
