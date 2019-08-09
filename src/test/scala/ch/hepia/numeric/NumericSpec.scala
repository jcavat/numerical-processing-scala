package ch.hepia.numeric

import org.scalatest._

class NumericSpec extends FlatSpec with Matchers {
  "Vectors" should "be comparable" in {
    assert(DenseVector(1.0, 2.0, -3.0) === DenseVector(1.0, 2.0, -3.0))
    assert(Transposed(1.0, 2.0, -3.0) === Transposed(1.0, 2.0, -3.0))
    assert(DenseVector(1.0, 2.0, -3.0) !== Transposed(1.0, 2.0, -3.0))
    assert(DenseVector(1.0, 2.0, -3.0).t() === Transposed(1.0, 2.0, -3.0))
    assert(DenseVector(1.0, 2.0, -3.0) === Transposed(1.0, 2.0, -3.0).t())
    assert( DenseVector(1,2,3).lessOrEqual(DenseVector(1,4,3) ))
    assert( DenseVector(1,2,3).less(DenseVector(2,4,4) ))
    assert( !DenseVector(1,2,3).lessOrEqual(DenseVector(0,4,3) ))
    assert( !DenseVector(1,2,3).less(DenseVector(1,4,4) ))
    assert( !DenseVector(1,2,3).less(DenseVector(0,4,4) ))
  }
  "Matrix" should "be comparable" in {
    assert(Matrix(Transposed(1.0, 2.0), Transposed(1.0, 3.0)) === Matrix(Transposed(1.0, 2.0), Transposed(1.0, 3.0)))
  }
  "Matrix determinant" should "work" in {
    val m1 = Matrix(
      Transposed(1,2,4,8),
      Transposed(3,4,12,3),
      Transposed(1,4,12,5),
      Transposed(3,9,1,6)
    )
    assert(Matrix.det(m1) === 1320.0)

    val m2 = Matrix(
      Transposed(2,1,3),
      Transposed(1,0,2),
      Transposed(2,0,-2),
    )
    assert(Matrix.det(m2) === 6.0)

    val m3 = Matrix(
      Transposed(1,3,1,-1,7),
      Transposed(2,2,2,0,2),
      Transposed(3,1,3,-8, 1),
      Transposed(3,2,4,1,3),
      Transposed(5,2,5,2,2)
    )
    assert(Matrix.det(m3) === -224.0)

  }

  "Matrix" should "transposed correctly" in {
    val m1 = Matrix(
      Transposed(1,2,3),
      Transposed(0,1,1) )

    assert( m1.t() === Matrix(Transposed(1.0, 0.0), Transposed(2.0, 1.0), Transposed(3.0, 1.0)) )
  }

  "Matrix" should "be solved as an equation system" in {

    val m = Matrix(
      Transposed(3,2,7),
      Transposed(0,4,0),
      Transposed(1,-2,1)
    )

    assert( m.solveWith(DenseVector(1,1,-1)) === DenseVector(-1.0, 0.25, 0.5) )
  }
}
