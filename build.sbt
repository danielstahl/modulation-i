name := "modulation-i"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.5"

libraryDependencies += "com.illposed.osc" % "javaosc-core" % "0.2"

libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.11" % "1.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"

libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.20-R6"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.0.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "net.soundmining" %% "soundmining-tools" % "1.0-SNAPSHOT"

org.scalastyle.sbt.ScalastylePlugin.Settings
