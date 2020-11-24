package com.github.mutsuhiro6.ml.factorization

import breeze.linalg.Matrix.castUpdateOps
import com.github.mutsuhiro6.linalg._
import breeze.linalg._

object MatrixFactorization {

  private var tolerance = 0.01
  private var maxIteration = 1000

  def generateFilterMat(vec: Vec): Mat = {
    val nonZeroSize = vec.toArray.count(_ != 0)
    val mat = DenseMatrix.zeros[Double](nonZeroSize, vec.length)
    var row = 0
    vec.foreachPair {
      case (i, v) =>
        if (v != 0) {
          mat(row, i) = 1.0
          row += 1
        }
    }
    mat
  }


  def factorize(R: Mat, k: Int, lambda: Double): Mat = {


    val n_u = R(*, ::).map {
      _.toArray.count(_ != 0)
    }
    val n_m = R(::, *).map {
      _.toArray.count(_ != 0)
    }

    val I_u = R(*, ::).map(
      v => generateFilterMat(v)
    )
    val I_m = R(::, *).map(
      v =>
        generateFilterMat(v)
    )

    val rows = R.rows
    val cols = R.cols

    val U = DenseMatrix.rand[Double](rows, k)
    val M = DenseMatrix.rand[Double](cols, k)


    def calculateLoss: Double = {
      var loss: Double = 0.0
      R.foreachPair {
        case ((i, j), r) =>
          if (r != 0.0) {
            loss += r - U(i, ::).inner.t * M(j, ::).inner
          }
      }
      for (i <- 0 until rows) {
        loss += lambda * n_u(i) * norm(U(i, ::).inner)
      }
      for (j <- 0 until cols) {
        loss += lambda * n_m(j) * norm(M(j, ::).inner)
      }
      loss
    }

    var loss = 0.0
    var diff = Double.PositiveInfinity
    var itr = 0
    while (true) {
      itr += 1
      println(s"Iteration: $itr")
      // update U
      println("Updating U...")
      for (i <- 0 until rows) {
        val MIm = (I_u(i) * M).t
        val Am = MIm * MIm.t + lambda * n_u(i) * DenseMatrix.eye[Double](MIm.rows)
        U(i, ::) := (inv(Am) * (MIm * (I_u(i) * R(i, ::).inner))).toDenseVector.t
      }
      // update M
      println("Updating M...")
      for (i <- 0 until cols) {
        val UIu = (I_m(i) * U).t
        val Au = UIu * UIu.t + lambda * n_m(i) * DenseMatrix.eye[Double](UIu.rows)
        M(i, ::) := (inv(Au) * (UIu * (I_m(i) * R(::, i)))).toDenseVector.t
      }
      diff = math.abs(loss - calculateLoss)
      println(s"Diff: $diff")
      if (itr != 1 && (diff < tolerance || itr >= maxIteration)) {
        val R_hat = U * M.t
        return R_hat
      }
    }
    val R_hat = U * M.t
    R_hat
  }

  def setTolerance(tol: Double): Unit = {
    tolerance = tol
  }

  def setMaxIteration(itr: Int): Unit = {
    maxIteration = itr
  }

}
