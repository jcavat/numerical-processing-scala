package ch.hepia
package numeric

import scala.util.Try


case class Matrix private(private val dvs: Transposed*) {
  import scala.collection.mutable
  private[numeric] val vectors: mutable.ListBuffer[Transposed] = mutable.ListBuffer( dvs : _* )

  def nbRows: Int = vectors.length
  def nbCols: Int = if (vectors.isEmpty) 0 else vectors.head.len()
  def cols: Seq[DenseVector] = (0 until nbCols).map {
      idx => DenseVector( vectors.map( dv => dv.get(idx) ): _* )
  }
  def set(row: Int, col: Int, value: Double): Unit = vectors(row).set(col, value)
  def get(row: Int, col: Int): Double = vectors(col).get(row)
  def getRow(row: Int): Transposed = vectors(row)
  def getCol(col: Int): DenseVector = cols(col)
  override def toString: String = {
    vectors.map( _.toString() ).mkString("\n")
  }
  def map(f: Double => Double): Matrix = Matrix( vectors.map(_.map(f)): _* )

  def isEmpty: Boolean = nbCols == 0
  def isSquare: Boolean = isEmpty || nbRows == nbCols

  def add(denseVector: DenseVector): Unit = {
    if (!isEmpty && denseVector.len() != nbRows) throw new IllegalArgumentException
    if(isEmpty){
      for( i <- 0 until denseVector.len() ) {
        vectors += Vector.empty().t()
      }
    }
    for( i <- 0 until denseVector.len() ) {
      vectors(i).add(denseVector.get(i))
    }
  }
  def add(transposed: Transposed): Unit = {
    if (!isEmpty && transposed.len() != nbCols) throw new IllegalArgumentException
    vectors += transposed
  }
  def mul(that: Matrix): Matrix = {
    if ( this.nbCols != that.nbRows ) throw new IllegalArgumentException
    Matrix.tabulate(this.nbRows, that.nbCols, (i,j) => {
      this.getRow(i).dot(that.getCol(j))
    })
  }
  def mul(by: Double): Matrix = map( _ * by )
  def t(): Matrix = {
    val res = Matrix.empty()
    for( r <- 0 until nbRows ) {
      res.add( vectors(r).t() )
    }
    res
  }
  def com(): Matrix = {
    Matrix.tabulate(nbRows, nbCols, (i,j) => {
      if( (i+j) % 2 == 0 ){
        Matrix.det( this.removed(i,j) )
      } else {
        -Matrix.det( this.removed(i,j) )
      }
    })
  }
  def solveWith(denseVector: DenseVector): DenseVector = {
    this.inv().mul( numeric.Matrix(denseVector.t()).t() ).getCol(0)
  }

  // TODO: Return Option if det = 0
  def inv(): Matrix = {
    this.com().t().mul( 1.0/Matrix.det(this) )
  }
  def rowRemoved(row: Int): Matrix = {
    Matrix( vectors.slice(0, row) ++ vectors.slice(row+1, vectors.length): _* )
  }
  def colRemoved(col: Int): Matrix = {
    numeric.Matrix( vectors.map(c => c.removed(col)): _* )
  }
  def removed(row: Int, col: Int): Matrix = {
    // Could be (but less efficient:
    // return rowRemoved(row).colRemoved(col)
    val res = Matrix.empty()
    for( i <- 0 until nbRows ) {
      if (i != row){
        val v: Transposed = vectors(i)
        res.add( v.removed(col) )
      }
    }
    res
  }
  override def equals(obj: Any): Boolean = {
    Try(obj.asInstanceOf[Matrix]).toOption.exists( that => that.vectors == this.vectors )
  }

  override def hashCode(): Int = vectors.hashCode()
}

object Matrix {
  def fill(rows: Int, cols: Int, value: Double): Matrix = numeric.Matrix( List.fill(rows)( Vector.fill(cols, value).t() ): _* )
  def zeros(rows: Int, cols: Int): Matrix = Matrix.fill(rows, cols, 0.0)
  def ones(rows: Int, cols: Int): Matrix = Matrix.fill(rows, cols, 1.0)
  def tabulate(rows: Int, cols: Int, f: (Int, Int) => Double): Matrix = {
    val res = Matrix.zeros(rows, cols)
    for( r <- 0 until rows; c <- 0 until cols ){
      res.set(r, c, f(r,c))
    }
    res
  }
  def diag(ds: Double*): Matrix = {
    val res = Matrix.zeros(ds.length, ds.length)
    for( i <- 0 until ds.length ){
      res.set(i, i, ds(i))
    }
    res
  }
  def identity(nb: Int): Matrix = diag( (0 until nb).map( _ => 1.0 ): _* )

  def sign(nb: Int): Matrix = {
    val res = Matrix.zeros(nb, nb)
    for( i <- 0 until nb ){
      res.set(i, i, if( i%2 == 0) 1 else -1 )
    }
    res
  }


  def empty(): Matrix = ones(0,0)
  def det(matrix: Matrix): Double = {

    if (!matrix.isSquare) throw new IllegalArgumentException

    if( matrix.nbCols == 1) {
      matrix.get(0,0)
    } else if( matrix.nbCols == 2 ){
      matrix.get(0,0)*matrix.get(1,1) - matrix.get(1,0)*matrix.get(0,1)
    } else {

      val firstRow = matrix.getRow(0)
      var res: Double = 0.0
      for( i <- 0 until firstRow.len() ){
        if (i % 2 == 0){
          res += firstRow.get(i) * det( matrix.removed(0, i) )
        } else {
          res += -firstRow.get(i) * det( matrix.removed(0, i) )
        }
      }
      res
    }
  }

}
