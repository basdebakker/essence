scalaVersion := "3.9.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-java-output-version:25",
  "-source:future",
)

lazy val root = rootProject
  .settings(
    name := "essence",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.13.0",
      "junit" % "junit" % "4.13.2" % "test",
    )
  )
