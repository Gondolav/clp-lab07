object Match {
  val l: L.List = L.Cons(1, L.Cons(2, L.Cons(error("lazy"), L.Nil()))); // No error is thrown
  l match {
    case L.Nil() => () // At this point, we evaluate l just enough to know it is a Cons
    case L.Cons(h, t) => Std.printInt(h) // Prints 1
    case L.Cons(h1, L.Cons(h2, L.Cons(h3, _))) =>
      // Still no error...
      Std.printInt(h3)
    // This forces evaluation of the third list element // and an error is thrown!
  }
}