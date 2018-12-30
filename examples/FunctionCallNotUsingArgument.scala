object FunctionCallNotUsingArgument {
  def f(x: Int): Unit = {
    Std.printInt(10)
  }

  val y: Int = (Std.printString("Hello"); 2);

  f(y)
}