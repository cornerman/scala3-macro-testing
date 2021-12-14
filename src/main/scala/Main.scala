package test

trait Command {
  def run: Int
  def run2: Int
  def run3(i: Option[String])(j: Either[Int, String], k: String): Int
  def run4[T]: Int
  // def ignored: String
}

val commander = new Command {
  def run = 15
  def run2 = 10
  def ignored = "hallo"
  def run3(i: Int)(j: String, k: String): String = "1"
}
val commander2 = new Command {
  def run = 0
  def run2 = 1
  def ignored = "peter"
  def run3(i: Int)(j: String, k: String): String = "2"
}

class Bla

@main
def main = {
  println(MyMacro.router[Command, Int](commander)("run")())
  println(MyMacro.client[Bla, Int](() => 12))
}
