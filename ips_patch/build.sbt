
lazy val commonSettings = Seq(
  organization := "rwt.ips",
  version := "1.0",
  scalaVersion := "2.12.0-RC2",
  scalacOptions += "-opt:l:classpath" 
)


lazy val ipsApply = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "ips_apply"
  )
