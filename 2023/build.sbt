scalaVersion := "3.3.3"
scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror")
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M11" % Test
