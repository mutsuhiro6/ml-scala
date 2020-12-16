package com.github.mutsuhiro6

package object util {

  def elapsedTime[R](message: String)(run: => R): Double = {
    val tic = System.nanoTime()
    run
    val toc = System.nanoTime()
    val giga = 1000.0 * 1000.0 * 1000.0
    val time: Double = (toc - tic) / giga
    message match {
      case null => println(s"Elapsed time: $time [sec].")
      case _ => println(s"Elapsed time in $message: $time [sec].")
    }
    time
  }

  def elapsedTime[R](run: => R): Double = {
    elapsedTime(null)(run)
  }

}
