package course7

object VarianceWithCats extends App {
  object variance_examples {

    class Container[+A](val elems: List[A]) {
      def get(i: Int): A = elems(0)
      def put[B >: A](elem: B) = new Container(elem :: elems)
    }

    class Animal(val name: String, val kind: String)
    class Cat(name: String) extends Animal(name, "Cat")

    val cat1 = new Cat("Miaou")
    val anim1: Animal = new Animal("Booboo", "Baboon")

    // Standard polymorphism
    val anim2: Animal = cat1

    // Making an animal collection
    val animalCollection = new Container[Animal](Nil).put(anim1)

    // Making a cat container
    val catCollection = new Container[Cat](Nil).put(cat1)

    // Polymorphism applied to the members
    catCollection.put(cat1)
    animalCollection.put(cat1)

    // Covariance of the data structure itself
    val animalCollection2: Container[Animal] = catCollection
    
    // Won't work
    // val catCollection2 : Container[Cat] = animalCollection
  }
}