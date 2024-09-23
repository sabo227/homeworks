object Main {

  def hello(hello: String, name: String) = {
    println(hello + " Scala! This is " + name)
  }

  val hellos = "Hello" :: "Hola" :: "Guten tag" :: Nil
  val names = "Viktor Mikhailov" :: "Viktor Mikhailov".reverse :: Nil

  def main(args: Array[String]): Unit = {
    for (h <- hellos; n <- names) hello(h, n)
  }
}