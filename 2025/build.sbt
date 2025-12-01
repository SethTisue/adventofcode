scalaVersion := "3.8.0-RC2"
scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror")
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
libraryDependencies += "org.scalameta" %% "munit" % "1.2.1" % Test
