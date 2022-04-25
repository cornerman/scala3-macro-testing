package test

trait Command {
  def run(): Int
  def run2(foo: String): Int
  //TODO: how to work with nullary methods:
  // def run3: Int
}

@main
def main = {
  val myCommand: Command = MyMacro.client[Command, Int](() => 12)
  println(myCommand.run()) // 12
  println(myCommand.run2("test")) // 12
  // println(myCommand.run3) // 12
}
