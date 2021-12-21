package test

abstract class Command {
  def run: Int
  // def run2: Int
  // def run3(i: Option[String])(j: Either[Int, String], k: String): Int
  // def run4[T]: Int
  // def ignored: String
}

val commander = new Command {
  def run = 15
  def run2 = 10
  def ignored = "hallo"
  def run3(i: Option[String])(j: Either[Int, String], k: String): Int = 1
  def run4[T]: Int = 1
}
val commander2 = new Command {
  def run = 0
  def run2 = 1
  def ignored = "peter"
  def run3(i: Option[String])(j: Either[Int, String], k: String): Int = 2
  def run4[T]: Int = 2
}

class Bla

@main
def main = {
  val map = MyMacro.router[Command, Int](commander)
  // map("run2")()

  val myCommand = MyMacro.client[Command, Int](() => 12)
  // val x = MyMacro.fun[Int]
  // type X = x.tpe
  // val s: X = "Strin"

}

// @main
// def main = {
//   type Id[X] = X
//   case class Foo[F[_]](val x: F[Int], y: F[Int])

//   val gen = MyMacro.genType[Foo, Id]

//   val foo: gen.Type = Foo(1, 2)
//   val x: Int = obj.x
// }
