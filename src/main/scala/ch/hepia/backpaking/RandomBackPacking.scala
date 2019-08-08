package ch.hepia.backpaking
import ch.hepia.numeric.{DenseVector, Transposed, Vector}
;

class RandomBackPacking extends BackpackingProblem {

  override def solve(): DenseVector = {
    val length = objs.len()
    var currentFit = 0.0
    var currentSol: DenseVector = Vector.zeros(objs.len())
    for(_ <- 0 to 10000) {
      val newSol: DenseVector = Vector.rand(length).map(_ * 5.0).map( _.toInt )
      if( isLegal(newSol) && this.objs.t().dot(newSol) >= currentFit ) {
        currentSol = newSol
        currentFit = this.objs.t().dot(newSol)
      }
    }
    currentSol
  }
}
