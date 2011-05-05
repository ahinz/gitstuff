package org.hinz.gis
import java.util.Date

/**
 * An interval represents the time it takes to go from one distance point
 * to another distance point
 *
 * Intervals also contain the date they were recorded
 */
case class Interval(start:Double, end:Double, time: Double, recordedAt: Date) {

  /**
   * Determine if this interval contains the given point
   *
   * @param d point to check
   * @return true if d is in [start,end]
   */
  def contains(d: Double):Boolean =
    d >= start && d <= end

  /**
   * Split an interval at the given point
   * The ratio of distance to time remains the same
   *
   * If d is not in the interval an exception is thrown
   */
  def split(d: Double):(Interval,Interval) =
    if (contains(d)) 
      (Interval(start, d, ((d - start)/(end - start))*time, recordedAt),
       Interval(d, end, ((end -d)/(end - start))*time, recordedAt))
    else
      throw new Exception("Cannot split " + toString + " on " + d)

  override def toString = "[" + start + ", " + end + "]"
}


object main {
  def main(args: Array[String]) = {
    println("Hello world!")
  }
}
