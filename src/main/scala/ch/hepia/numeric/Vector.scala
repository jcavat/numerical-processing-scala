package ch.hepia
package numeric


case class DenseVector(private val ds: Double*) {
  import scala.collection.mutable
  private[numeric] val doubles: mutable.ListBuffer[Double] = mutable.ListBuffer(ds: _*)

  def t(): numeric.Transposed = Transposed( this )
  override def toString: String = {
    "[" + doubles.map(_.toString).mkString("\n") + "]"
  }
  def len(): Int = doubles.length
  def set(idx: Int, value: Double): Unit = doubles.update(idx, value)
  def get(idx: Int): Double = doubles(idx)
  def map(f: Double => Double): DenseVector = DenseVector( doubles.map(f): _* )
  def add(value: Double): Unit = doubles += value
  def mul(by: Double): DenseVector = map( _ * by )


  def slice(from: Int, to: Int): DenseVector = DenseVector( doubles.slice(from, to): _* )
  def sliceFrom(from: Int): DenseVector = slice(from, len())
  def sliceTo(to: Int): DenseVector = slice(0, to)
  def concat(dv: DenseVector): DenseVector = dv match {
    case dv: DenseVector => DenseVector(doubles ++ dv.doubles: _*)
    case _ => throw new IllegalArgumentException
  }

  def removed(idx: Int): DenseVector = sliceTo(idx).concat(sliceFrom(idx+1))
  def less(that: DenseVector): Boolean = compare(that, _ < _)
  def lessOrEqual(that: DenseVector): Boolean = compare(that, _ <= _)
  def compare(that: DenseVector, cmp: (Double, Double) => Boolean): Boolean = {
    for( i <- 0 until len()) {
      if(!cmp(this.get(i), that.get(i))) {
        return false
      }
    }
    true
  }
}

case class Transposed(private[numeric] val v: DenseVector) {
  def t(): numeric.DenseVector = v
  override def toString: String = {
    "[" + v.doubles.map(_.toString).mkString(",") + "]"
  }
  def dot(denseVector: DenseVector): Double = {
    if( this.len() != denseVector.len() ) throw new IllegalArgumentException

    v.doubles.zip(denseVector.doubles).map{ case (d1, d2) => d1*d2}.sum

  }
  def len(): Int = v.doubles.length
  def set(idx: Int, value: Double): Unit = v.set(idx, value)
  def get(idx: Int): Double = v.doubles(idx)
  def map(f: Double => Double): Transposed = Transposed( v.map(f) )
  def add(value: Double): Unit = v.add(value)

  def slice(from: Int, to: Int): Transposed = DenseVector( v.doubles.slice(from, to): _* ).t()
  def concat(dv: Transposed): Transposed = {
    DenseVector(v.doubles ++ dv.v.doubles: _*).t()
  }
  def sliceFrom(from: Int): Transposed = slice(from, len())
  def sliceTo(to: Int): Transposed = slice(0, to)
  def removed(idx: Int): Transposed = sliceTo(idx).concat(sliceFrom(idx+1))
}

object Transposed {
  def apply(ds: Double*): Transposed = DenseVector(ds: _*).t()
}

object Vector {

  case class VectorFill(nb: Int) {
    def withValue(value: Double) = DenseVector(  List.fill(nb)(value): _* )
  }
  case class LineSpaceFrom(from: Double) {
    def to(to: Double) : LineSpaceTo = LineSpaceTo(from, to)
  }
  case class LineSpaceTo(from: Double, to: Double) {
    def repeat(nb: Int): numeric.DenseVector = linspace(from, to, nb)
  }

  def fill(nb: Int, value: Double): DenseVector = DenseVector(  List.fill(nb)(value): _* )

  def fill(nb: Int): VectorFill = VectorFill(nb)

  def linspace(from: Double, to: Double, nb: Int): DenseVector = {
    DenseVector( (0 until nb).map( i => ((to-from)/(nb-1))*i + from): _* )
  }
  def from(from: Double): LineSpaceFrom = LineSpaceFrom(from)

  def zeros(nb: Int): DenseVector = fill(nb, 0.0)

  def ones(nb: Int): DenseVector = fill(nb, 1.0)

  def tabulate(nb: Int, f: Int => Double): DenseVector = DenseVector( (0 until nb).map( f ): _* )

  def from(ls: List[Double]): DenseVector = DenseVector( ls: _* )

  def empty(): DenseVector = ones(0)

  val r = scala.util.Random
  def rand(nb: Int): DenseVector = zeros(nb).map( _ => r.nextDouble() )


}

