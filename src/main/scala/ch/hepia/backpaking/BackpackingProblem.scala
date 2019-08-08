package ch.hepia.backpaking

import ch.hepia.numeric.{DenseVector, Matrix, Transposed, Vector}

import scala.Array;

trait BackpackingProblem {

  import scala.collection.mutable
//  protected val constraints: mutable.ListBuffer[(DenseVector, Double)] = mutable.ListBuffer()
  protected val constraints: Matrix = Matrix.empty()
  protected val bounds: DenseVector = Vector.empty()
  protected var objs: DenseVector = Vector.empty()

  def addObjective(objs: DenseVector): Unit = {
    this.objs = objs
  }
  def addConstraint(v: DenseVector, bound: Double): Unit = { // default v <= borne
    constraints.add(v.t())
    bounds.add(bound)
  }
  def addBound(v: DenseVector): Unit = {
    for( i <- 0 until v.len() ){
      val c: DenseVector = Vector.zeros(v.len())
      c.set(i, 1.0)
      constraints.add(c.t())
      bounds.add(v.get(i))
    }
  }
  protected def isLegal(x: DenseVector): Boolean = {
    constraints.mul( Matrix(x.t()).t() ).getCol(0).lessOrEqual(bounds)
  }
  def solve(): DenseVector
}
