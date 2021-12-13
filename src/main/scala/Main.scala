trait Command {
  def run: Int
  def run2: Int
  def ignored: String
}

val commander = new Command {
  def run = 15
  def run2 = 10
  def ignored = "hallo"
}
val commander2 = new Command {
  def run = 0
  def run2 = 1
  def ignored = "peter"
}

@main
def main = {
  println(MyMacro.call[MyMacro.Test, Int](null, "run"))
  // println(MyMacro.call[Command, Int](commander, "run"))
  // println(MyMacro.call[Command, Int](commander2, "run"))
  // println(MyMacro.call[Command, Int](commander, "run2"))
  // println(MyMacro.call[Command, Int](commander2, "run2"))
  // println(MyMacro.call[Command, String](commander, "ignored"))
  // println(MyMacro.call[Command, String](commander2, "ignored"))
}
