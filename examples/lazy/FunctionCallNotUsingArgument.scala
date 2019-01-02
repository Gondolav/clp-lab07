object FunctionCallNotUsingArgument {
  def f(x: Int): Unit = {
    Std.printString("Ok!")
  }

  val y: Int = (Std.printString("Hello"); 2);

  f(y)
}