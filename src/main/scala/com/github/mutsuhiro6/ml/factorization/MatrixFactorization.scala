package com.github.mutsuhiro6.ml.factorization

import com.github.mutsuhiro6.linalg._
import breeze.linalg._

object MatrixFactorization {

  private var tolerance = 0.01

  def generateFilterMat(vec: Vec): Mat = {
    val mat = DenseMatrix.zeros[Double](vec.activeSize, vec.length)
    var row = 0
    vec.foreachPair {
      case (i, v) =>
        if (v != 0) {
          mat(row, i) = 1.0
          row+=1
        }
    }
    mat
  }

  def loss(R: Mat, U: Mat, M: Mat, lambda: Double): Double = {


1.0
  }

  def factorize(R: Mat, k: Int, lambda: Double): Mat = {

    val n_u = R(*, ::).map {
        _.toArray.count(_ != 0)
    }
    val n_m = R(::, *).map {
      _.toArray.count(_ != 0)
    }

    val I_u = R(*, ::).map(generateFilterMat)
    val I_m = R(::, *).map(generateFilterMat)

    val U = DenseMatrix.rand[Double](R.rows, k)
    val M = DenseMatrix.rand[Double](R.cols, k)


    var loss = 0.0
    while (loss < tolerance) {}


U
  }

}
