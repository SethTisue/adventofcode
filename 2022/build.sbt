scalaVersion := "3.2.1"
scalacOptions ++= Seq("-deprecation", "-source:future", "-Werror")
libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M7" % Test