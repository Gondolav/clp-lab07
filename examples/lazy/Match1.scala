object Match1 {
  val l: L.List = L.Cons(1, L.Cons(2, L.Cons(error("lazy"), L.Nil()))); // No error is thrown

  l match {
    case L.Nil() => () // At this point, we evaluate l just enough to know it is a Cons
    case L.Cons(h, t) => Std.printInt(h) // Prints 1
  }
}