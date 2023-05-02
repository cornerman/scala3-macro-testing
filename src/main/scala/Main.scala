package test

import scala.annotation.experimental

trait Command {
  def run(): Int
  def run2(foo: String): Int
  def run3: Int
}

@main
@experimental
def main: Unit = {
  val myCommand: Command = MyMacro.client[Command, Int](() => 12)
  println(myCommand.run()) // 12
  println(myCommand.run2("test")) // 12
  println(myCommand.run3) // 12
}
