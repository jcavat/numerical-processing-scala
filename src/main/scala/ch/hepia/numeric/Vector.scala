package ch.hepia
package numeric

trait Vector {
  def t(): numeric.Vector
  def dot(denseVector: DenseVector): Double
  def len(): Int
  def set(idx: Int, value: Double): Unit
  def get(idx: Int): Double
  def map(f: Double => Double): numeric.Vector
  def add(value: Double): Unit

  def mul(by: Double): Vector = map( _ * by )

  // norm
  // sum

  def slice(from: Int, to: Int): Vector
  def sliceFrom(from: Int): Vector = slice(from, len())
  def sliceTo(to: Int): Vector = slice(0, to)

  def concat(vector: Vector): Vector
  def removed(idx: Int): Vector = sliceTo(idx).concat(sliceFrom(idx+1))
}

case class DenseVector(private val ds: Double*) extends numeric.Vector {
  import scala.collection.mutable
  private[numeric] val doubles: mutable.ListBuffer[Double] = mutable.ListBuffer(ds: _*)

  override def t(): numeric.Transposed = Transposed( this )
  override def toString: String = {
    "[" + doubles.map(_.toString).mkString("\n") + "]"
  }
  override def dot(denseVector: DenseVector): Double = throw new IllegalArgumentException
  override def len(): Int = doubles.length
  override def set(idx: Int, value: Double): Unit = doubles.update(idx, value)
  override def get(idx: Int): Double = doubles(idx)
  override def map(f: Double => Double): DenseVector = DenseVector( doubles.map(f): _* )
  override def add(value: Double): Unit = doubles += value

  def slice(from: Int, to: Int): DenseVector = DenseVector( doubles.slice(from, to): _* )
  def concat(dv: Vector): DenseVector = dv match {
    case dv: DenseVector => DenseVector(doubles ++ dv.doubles: _*)
    case _ => throw new IllegalArgumentException
  }

}

case class Transposed(private[numeric] val v: DenseVector) extends numeric.Vector {
  override def t(): numeric.DenseVector = v
  override def toString: String = {
    "[" + v.doubles.map(_.toString).mkString(",") + "]"
  }
  def dot(denseVector: DenseVector): Double = {
    if( this.len() != denseVector.len() ) throw new IllegalArgumentException

    v.doubles.zip(denseVector.doubles).map{ case (d1, d2) => d1*d2}.sum

  }
  override def len(): Int = v.doubles.length
  override def set(idx: Int, value: Double): Unit = v.set(idx, value)
  override def get(idx: Int): Double = v.doubles(idx)
  override def map(f: Double => Double): Transposed = Transposed( v.map(f) )
  override def add(value: Double): Unit = v.add(value)

  def slice(from: Int, to: Int): Transposed = DenseVector( v.doubles.slice(from, to): _* ).t()
  def concat(dv: Vector): Transposed = dv match {
    case dv: Transposed => DenseVector(v.doubles ++ dv.v.doubles: _*).t()
    case _ => throw new IllegalArgumentException
  }
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
    def repeat(nb: Int): numeric.Vector = linspace(from, to, nb)
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

