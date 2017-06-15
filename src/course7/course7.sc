
case class Foo(val x : Int) extends Ordered[Foo]{

  override def compare(that: Foo): Int = this.x compare that.x
}

val a = Foo(2)
val b = Foo(3)
val c = Foo(2)

assert(a < b)
