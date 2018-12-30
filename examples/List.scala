object List {
  val l: L.List = L.Cons(1, L.Cons(2, L.Cons(error("lazy"), L.Nil())));
  1
}