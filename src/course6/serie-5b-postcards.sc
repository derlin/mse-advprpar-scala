val cities = List("Paris", "London", "Berlin", "Lausanne")
val relatives = List("Grandma", "Grandpa", "Aunt Lottie", "Dad")
val travellers = List("Pierre-Andre", "Rachel")


// postcards to all
{
  for (
    t <- travellers;
    r <- relatives;
    c <- cities
  ) yield s"Dear $r, Wish you were here in $c! Love, $t"
}.mkString("\n")

// postcards to relatives starting with G
{
  for (
    t <- travellers;
    r <- relatives if r.startsWith("G");
    c <- cities
  ) yield s"Dear $r, Wish you were here in $c! Love, $t"
}.mkString("\n")

