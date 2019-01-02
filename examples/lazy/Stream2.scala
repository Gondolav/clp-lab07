object Stream2 {
  def powersOfTwo(start: Int): L.List = {
    L.Cons(start, powersOfTwo(start * 2))
  }

  Std.printString(L.toString(L.take(powersOfTwo(1), 5)))
}