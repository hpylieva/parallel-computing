package ua.edu.ucu.cs.parallel
import java.io.{BufferedWriter, File, FileWriter}

import org.scalameter.{Key, MeasureBuilder, Warmer, config}

import scala.util.Random

object ClosestPair {
  case class Point(x: Int, y: Int)
  case class PointsPair(point1: Point, point2: Point) {
    def distance: Double = ClosestPair.distance(point1, point2)
  }

  def distance(point1: Point, point2: Point): Double = {
    math.sqrt(math.pow(point1.x - point2.x, 2) + math.pow(point1.y - point2.y, 2))
  }

  private var threshold: Int = 0
  private def setMaxPointsOnThread(pointsLength: Int, cores: Int): Unit = threshold = pointsLength / cores

  def findDistanceOfClosestPair(points: Vector[Point], threads: Int = 1): Double = {
    setMaxPointsOnThread(points.length, threads)
    val (_, distance) = closestPairMain(points)
    distance
  }

  def closestPairMain(points: Vector[Point]): (Vector[Point], Double) = {
    if (points.length < 2)
      (points, Double.MaxValue)
    else {
      val (left, right, splitPoint) = splitByX(points)
      val ((leftPoints, leftDistance), (rightPoints, rightDistance)) = processLeftRightParts(left, right)
      val pointsMerged = sortByY(leftPoints ++ rightPoints)
      val dist = boundaryMerge(pointsMerged, leftDistance, rightDistance, splitPoint)
      (pointsMerged, dist)
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

  def boundaryMerge(points: Vector[Point], leftDistance: Double, rightDistance: Double, middlePoint: Point): Double = {
    val minDistance = Math.min(leftDistance, rightDistance)
    val xl = middlePoint.x - minDistance
    val xr = middlePoint.x + minDistance

    val pointsInRange = points.filter(p => p.x >= xl && p.x <= xr)
    val minBoundaryDistance = getMinPointsDistance(pointsInRange)
    scala.math.min(minDistance, minBoundaryDistance)
  }

  def getMinPointsDistance(points: Vector[Point]): Double = {
    if (points.length < 2)
      Double.MaxValue
    else {
      var minDistance = Double.MaxValue
      for (i <- 0 until points.length - 1) {
        var j = i + 1
        while (j < i + 8 && j < points.length) {
          val stepDistance = distance(points(i), points(j))
          minDistance = if (stepDistance < minDistance) stepDistance else minDistance
          j += 1
        }
      }
      minDistance
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
    Key.exec.maxWarmupRuns -> 50,//60,
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

    val seqtime = standardConfig measure {
      findDistanceOfClosestPair(points)
    }
    val partime = standardConfig measure {
      findDistanceOfClosestPair(points, threads = cores)
    }

    println(s"closest_pair sequential time:  $seqtime")
    println(s"closest_pair parallel time:  $partime")
    println(s"speedup when used parallel:  ${seqtime.value / partime.value}")
  }
}
