package ch.hepia.backpacking
import ch.hepia.numeric.{DenseVector, Transposed, Vector}
;

class RandomBackPacking extends BackpackingProblem {

  def solve(): DenseVector = {
    val length = objs.len()
    var currentFit: Option[Double] = None
    var currentSol: DenseVector = Vector.zeros(objs.len())
    for(_ <- 0 to 10000) {
      val newSol: DenseVector = Vector.rand(length).map(_ * 5.0).map( _.toInt )
      if( isLegal(newSol) && currentFit.exists( fit => this.objs.t().dot(newSol) > fit) ) {
        currentSol = newSol
        currentFit = Some(this.objs.t().dot(newSol))
      }else if( isLegal(newSol) && currentFit.isEmpty ) {
        currentSol = newSol
        currentFit = Some(this.objs.t().dot(newSol))
      }
    }
    currentSol
  }
}
