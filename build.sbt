name := "fp-in-scala"

scalaVersion := "2.10.1"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-language:postfixOps")

initialCommands in console := "import the4e6.fpinscala._"
