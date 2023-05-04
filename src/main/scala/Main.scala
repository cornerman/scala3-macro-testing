package test

import scala.annotation.experimental

trait Command {
  def run(bar: String): Int
  def run2(foo: String): Int
  def run3(bar: String): Int
}

@main
@experimental
def main: Unit = {
  val myCommand: Command = MyMacro.client[Command, String, Int](str => str.toInt + 1)
  println(myCommand.run("11")) // 12
  println(myCommand.run2("2")) // 3
  println(myCommand.run3("1")) // 2

  val myRouter: Map[String, String => Int] = MyMacro.router[Command, String, Int](myCommand)

  println(myRouter("run")("11")) // 12
  println(myRouter("run2")("2")) // 3
  println(myRouter("run3")("1")) // 2
}
