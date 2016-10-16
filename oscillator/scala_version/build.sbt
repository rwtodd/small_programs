
lazy val commonSettings = Seq(
  organization := "com.waywardcode",
  version := "1.0",
  scalaVersion := "2.12.0-RC1"
)


lazy val oscillator = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "oscillator"
  )
