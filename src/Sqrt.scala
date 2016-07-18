/**
  * Created by natalya on 18/07/2016.
  */
object Sqrt {

  def squareNumbers(listNat: List[Int]): List[Int] = {

    def square(x: Int): Int = {
      return x * x;
    }
    return listNat.map(square(_))
  }

  val sqrt: PartialFunction[Int, Double] = {
    case num if num >= 0 => math.sqrt(num.toDouble)
  }

  def fun[x, y, z](f: (y) => x, g: (z) => y): (z) => x = (v) => f(g(v));

  def partAppliedFun[x, y, z](f: (y) => x, g: (z) => y) = f.apply(_)

  def wrapperFuncF[x, y, z](f: (y) => x, g: (z) => y) = f.apply(_) //partial application
  def wrapperFuncG[x, y, z](f: (y) => x, g: (z) => y): (z) => y = g.apply(_)  //partial application

  private def openConsoleWithSqrt(console: Int): Unit = {
    try {
      println(sqrt(console))
      println(squareNumbers(List(console)))
      println(fun[Int, Int, Int](_ *5, _ /1)(console))
      println(fun(wrapperFuncF[Int, Int, Int](_ *5, _ /1), wrapperFuncG[Int, Int, Int](_ *5, _ /1)))
      println(wrapperFuncF[Int, Int, Int](_ *5, _ /1)(console))
      println(wrapperFuncG[Int, Int, Int](_ *5, _ /1)(console))
    } catch {
      case e: RuntimeException => println(e.getMessage)
    }
  }

  def main(args: Array[String]): Unit = {
    var ok: Boolean = false
    do {
      val console = readLine("")

      ok = console != null && !console.isEmpty
      if (ok)
        openConsoleWithSqrt(console.toInt)
    } while (ok)
  }
}