object ValNotEvaluated {
  val x: Int = (Std.printInt(42); 0);
  val y: Int = x + 1;
  Std.printString("Ok!")
}