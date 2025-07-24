name := "aoc"
scalaVersion := "3.3.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

// Increase heap size for running tests
Test / fork := true

javaOptions ++= Seq(
  "-Xmx4G",       // max heap
  "-XX:+UseG1GC"  // better GC for large heaps
)

// Show time taken for tests
Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD")
