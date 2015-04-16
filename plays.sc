class Vector(val elements: Array[Double]) {
  def +(other: Vector) = ???

  def /(d: Double) = ???

  def +=(o: Vector) = ???

  def apply(i: Int): Double = ???
}

def average(ps: Iterable[Vector]): Vector = {
  val numVectors = ps.size
  var out = new Vector(ps.head.elements)
  ps foreach (out += _)
  out / numVectors
}

def average2(ps: Iterable[Vector]): Vector = {
  val numVectors = ps.size

  val vSize = ps.head.elements.length

  def element(index: Int): Double = ps.map(_(index)).sum / numVectors

  new Vector(0 until vSize map element toArray)
}