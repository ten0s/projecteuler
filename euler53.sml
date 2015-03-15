fun fac (n:int):LargeInt.int = let
    fun fac_iter 0 acc = acc
      | fac_iter n acc = fac_iter (n-1) (n*acc)
in
    if n < 0 then
        raise Domain
    else
        fac_iter (Int.toLarge n) (Int.toLarge 1)
end

fun selections (n:int) (r:int):LargeInt.int =
    (fac n) div ((fac r) * (fac (n-r)))

(*
if use LargeInt it's possible to reimplement fac
fun fac n = for 1 n 1 op* 1
*)
fun forStep from to step (f:((int * 'a) -> 'a)) (acc_init:'a) = let
    fun for_acc i acc =
      if i > to then acc
      else for_acc (i+step) (f (i, acc))
in
    for_acc from acc_init
end

fun for from to f acc_init = forStep from to 1 f acc_init

fun solve () =
  for 1 100 (fn (i, iacc) =>
                (for 1 i
                     (fn (j, jacc) =>
                         if (selections i j) > 1000000 then jacc + 1
                         else jacc) 0) + iacc) 0

structure Main = struct
  fun main (_, _) = let
      val ans = solve ()
      val _ = PrintUtils.printLn (Int.toString ans)
  in
      0
  end
end
