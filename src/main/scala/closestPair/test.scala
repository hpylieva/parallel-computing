package ua.edu.ucu.cs.parallel

import java.io.{BufferedWriter, File, FileWriter}

import org.scalameter.{Key, MeasureBuilder, Warmer, config}

import scala.util.Random

object test {
  case class Point(x: Int, y: Int)

  def distance(point1: Point, point2: Point): Double = {
    math.sqrt(math.pow(point1.x - point2.x, 2) + math.pow(point1.y - point2.y, 2))
  }

  private var threshold: Int = 0
  private def setMaxPointsOnThread(pointsLength: Int, cores: Int): Unit = threshold = pointsLength / cores

  def findDistanceOfClosestPair(points: Vector[Point], threads: Int = 1): (Double, Option[Point], Option[Point]) = {
    setMaxPointsOnThread(points.length, threads)
    val (_, distance, p0, p1) = closestPairMain(points)
    (distance, p0, p1)
  }

  def closestPairMain(points: Vector[Point]): (Vector[Point], Double, Option[Point], Option[Point]) = {
    if (points.length < 2)
      (points, Double.MaxValue, Option(points(0)), None)
    else {
      val (leftInputPoints, rightInputPoints, splitPoint) = splitByX(points)
      val ((leftResultPoints, leftDistance, p0_left, p1_left), (rightResultPoints, rightDistance, p0_right, p1_right)) =
        if (leftInputPoints.length < threshold)
          (closestPairMain(leftInputPoints), closestPairMain(rightInputPoints))
        else
          parallel(closestPairMain(leftInputPoints), closestPairMain(rightInputPoints))
      val pointsMerged = sortByY(leftResultPoints ++ rightResultPoints)
      var minDistance = Double.MaxValue
      var p0_min = Option(points(0))
      var p1_min = Option(points(1))
      if (leftDistance<rightDistance){
         minDistance = leftDistance
         p0_min = p0_left
         p1_min = p1_left
      }
      else {
        minDistance = leftDistance
        val p0_min = p0_right
        val p1_min = p1_right
      }

      val (dist, p0, p1) = boundaryMerge(pointsMerged, minDistance, p0_min, p1_min, splitPoint)
      (pointsMerged, dist, p0, p1)
    }
  }

  private def processLeftRightParts(leftPoints: Vector[Point], rightPoints: Vector[Point]) = {
    if (leftPoints.length < threshold)
      (closestPairMain(leftPoints), closestPairMain(rightPoints))
    else
      parallel(closestPairMain(leftPoints), closestPairMain(rightPoints))
  }

  def splitByX(points: Vector[Point]): (Vector[Point], Vector[Point], Point) = {
    val sortedX = sortByX(points)
    val medianId = sortedX.length / 2
    val medianX = sortedX(medianId)
    val (left, right) = sortedX.splitAt(medianId)
    (left, right, medianX)
  }

  def boundaryMerge(points: Vector[Point], minDistance: Double, p0: Option[Point], p1: Option[Point], middlePoint: Point): (Double, Option[Point], Option[Point]) = {

    val xl = middlePoint.x - minDistance
    val xr = middlePoint.x + minDistance

    val pointsInRange = points.filter(p => p.x >= xl && p.x <= xr)
    val (minBoundaryDistance, p0_on_boundary, p1_on_boundary) = getMinPointsDistance(pointsInRange)
    if (minDistance<minBoundaryDistance) {
      (minDistance, p0, p1)
    }
    else   {
      (minBoundaryDistance, p0_on_boundary, p1_on_boundary)
    }

  }

  def getMinPointsDistance(points: Vector[Point]): (Double, Option[Point], Option[Point]) = {
    if (points.length < 2)
      (Double.MaxValue, Option(points(0)), None)
    else {
      var minDistance = Double.MaxValue
      var p0 = points(0)
      var p1 = points(1)
      for (i <- 0 until points.length - 1) {
        for (j <- i + 1 until math.min(i+8, points.length)) {
          val stepDistance = distance(points(i), points(j))
          if (stepDistance < minDistance) {
            minDistance = stepDistance
            p0 = points(i)
            p1 = points(j)
          }
        }
      }
      print(minDistance, Option(p0), Option(p1),"\n")
      (minDistance, Option(p0), Option(p1))
    }
  }

  def sortByY(points: Vector[Point]): Vector[Point] = points.sortBy(point => point.y)
  def sortByX(points: Vector[Point]): Vector[Point] = points.sortBy(point => point.x)

  def generatePoints(length: Int, max: Int = 100, seed: Int = 123): Vector[Point] = {
    val min = - max
    val range = max * 2
    val rnd = new Random(seed = seed)
    (0 until length).map(_ => Point(rnd.nextInt(range)+min, rnd.nextInt(range)+min)).toVector.distinct
  }

  def writeToFile(input: Vector[Point], filename: String ):Unit = {
    val file = new File("src/main/scala/closestPair/"+filename)
    val input_txt = input.map(p => f"${p.x}, ${p.y}").mkString("\n")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(input_txt)
    bw.close()
  }

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 5,//20,
    Key.exec.maxWarmupRuns -> 20,//60,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

  def main(args: Array[String]): Unit = {
    val length = 16
    val points = generatePoints(length)
    writeToFile(points, "points.txt")
    val cores = Runtime.getRuntime.availableProcessors()

    println("closest_pair sequential result: ", findDistanceOfClosestPair(points))
    println("closest_pair parallel result:   ", findDistanceOfClosestPair(points, threads = cores))

//    val seqtime = standardConfig measure {
//      findDistanceOfClosestPair(points)
//    }
//    val partime = standardConfig measure {
//      findDistanceOfClosestPair(points, threads = cores)
//    }
//
//    println(s"closest_pair sequential time:  $seqtime")
//    println(s"closest_pair parallel time:  $partime")
//    println(s"speedup when used parallel:  ${seqtime.value / partime.value}")
  }
}

