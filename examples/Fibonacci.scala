object Fibonacci {
  def fib(i: Int): Int = {
    if (i < 2) { i }
    else { fib(i - 1) + fib(i - 2) }
  }

  Std.printString("Fibonacci(4) = "  ++ Std.intToString(fib(4)));
  Std.printString("Fibonacci(7) = " ++ Std.intToString(fib(7)))
}