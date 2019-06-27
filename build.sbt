import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "breeze",
    libraryDependencies ++= Seq( 
      "org.scalanlp" %% "breeze" % "0.13.2",
      "org.scalanlp" %% "breeze-viz" % "0.13.2",
      "org.scalanlp" %% "breeze-natives" % "0.13.2",
      scalaTest % Test)
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
