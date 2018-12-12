object ListMin {
  def min(l: L.List, acc: Int): Int = { l match {
      case L.Nil() => acc
      case L.Cons(h, t) =>
        if (h < acc) { min(t, h) }
        else { min(t, acc) }
  }}


  val l: L.List = L.Cons(5, L.Cons(45, L.Cons(90, L.Cons(4, L.Cons(10, L.Nil())))));
  Std.printString(L.toString(l));
  Std.printString("min = " ++ Std.intToString(min(l, L.head(l))))
}