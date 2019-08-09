package ch.hepia

import ch.hepia.backpacking.{BackpackingProblem, RandomBackPacking}
import ch.hepia.numeric.Matrix
import ch.hepia.numeric._


// TODO: Ecrire identidy with diag

object Main extends App {


  val prob: BackpackingProblem = new RandomBackPacking
  val objectives = DenseVector(9.0, 5.0, 7.0, 3.0, 1.0)

  prob.addObjective( objectives )
  prob.addConstraint( DenseVector(4.0, 3.0, 5.0, 2.0, 1.0), 10.0 )
  prob.addConstraint( DenseVector(4.0, 2.0, 3.0, 2.0, 1.0), 7.0 )
//  prob.addBound( DenseVector(1.0, 1.0, 1.0, 0.0, 0.0) )

  val sol = prob.solve()
  println( sol )
  println( objectives.t().dot(sol) )

  val money = new RandomBackPacking
  val values = DenseVector(5.0, 2.0, 1.0, 0.5, 0.2, 0.1, 0.05).mul(-1.0)
  val quantity = DenseVector(3, 2, 2, 1, 1, 1, 1)
  money.addObjective( values )
  money.addBound(quantity)
//  5 <= 4.5
  money.addConstraint( values, -40.50)
  val sol2 = money.solve()
  println( sol2 )
  println( values.t().dot(sol2) )


}


