object FunctionCallUsingArgumentCachesCorrectly {
  def f(x: Int): Unit = {
    Std.printInt(x)
  }

  val y: Int = (Std.printString("Hello"); 2);

  f(y); f(y)
}