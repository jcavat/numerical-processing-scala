package ch.hepia

import ch.hepia.backpaking.{BackpackingProblem, RandomBackPacking}
import ch.hepia.numeric.Matrix
import ch.hepia.numeric._


// TODO: Ecrire identidy with diag

object Main extends App {

  val m1 = Matrix( Transposed(1,2,3), Transposed(0,1,1) )
  val m2 = Matrix( Transposed(1,1), Transposed(0,2), Transposed(3,-4) )

  val m3 = Matrix(
    Transposed(1,2,4,8),
    Transposed(3,4,12,3),
    Transposed(1,4,12,5),
    Transposed(3,9,1,6)
  ) // 1320
  val m4 = Matrix(
    Transposed(2,1,3),
    Transposed(1,0,2),
    Transposed(2,0,-2),
  ) // 6
  val m5 = Matrix(
    Transposed(1,3,1,-1,7),
    Transposed(2,2,2,0,2),
    Transposed(3,1,3,-8, 1),
    Transposed(3,2,4,1,3),
    Transposed(5,2,5,2,2)
  ) // -224
  val m6 = Matrix(
    Transposed(1,1,2),
    Transposed(1,2,1),
    Transposed(2,1,1)
  )
  val m7 = Matrix(
    Transposed(-1,2,5),
    Transposed(1,2,3),
    Transposed(-2,8,10)
  )
  val m8 = Matrix(
    Transposed(3,2,7),
    Transposed(0,4,0),
    Transposed(1,-2,1)
  ) // inv()

  println( m8 )
  println()
  println( m8.solveWith( DenseVector(1,1,-1) ) )

  println("...")
  println( DenseVector(1,2,3).lessOrEqual(DenseVector(1,4,3) ))

  val prob: BackpackingProblem = new RandomBackPacking
  val objectives = DenseVector(9.0, 5.0, 7.0, 3.0, 1.0)

  prob.addObjective( objectives )
  prob.addConstraint( DenseVector(4.0, 3.0, 5.0, 2.0, 1.0), 10.0 )
  prob.addConstraint( DenseVector(4.0, 2.0, 3.0, 2.0, 1.0), 7.0 )
//  prob.addBound( DenseVector(1.0, 1.0, 1.0, 0.0, 0.0) )

  val sol = prob.solve()
  println( sol )
  println( objectives.t().dot(sol) )


}


