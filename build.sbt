import sbt.Keys.libraryDependencies

val deps = Seq("com.storm-enroute" % "scalameter_2.11" % "0.8.2")

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.11.8",
      version := "0.1.0-SNAPSHOT",
      sparkVersion := "2.2.0",
      sparkComponents ++= Seq("sql")
    )),
    name := "Parallel Tasks",
    libraryDependencies ++= deps,
    libraryDependencies ++= "org.apache.spark" %% "spark-core" % "1.6.2",
    libraryDependencies ++= "org.apache.spark" %% "spark-sql" % "1.6.2"
  )
