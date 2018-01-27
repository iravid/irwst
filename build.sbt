
inThisBuild(List(
  organization := "com.iravid",
  scalaVersion := "2.12.4",
  version := "0.1.0-SNAPSHOT",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  scalacOptions ++= Seq(
    "-Ypartial-unification"
  )
))

lazy val root = (project in file("."))
  .settings(
    publish := {}
  )
  .aggregate(irwst, benchmarks)

lazy val irwst = (project in file("irwst"))
  .settings(
    name := "irwst"
  )

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.0.1",
      "org.scalaz" %% "scalaz-core" % "7.3.0-M19"
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(irwst)
