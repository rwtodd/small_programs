
lazy val commonSettings = Seq(
  organization := "com.waywardcode",
  version := "1.0",
  scalaVersion := "2.12.0",
  scalacOptions += "-opt:l:classpath" 
)


lazy val fastFib = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "fastfib"
  )
