import com.typesafe.sbt.SbtStartScript

name := "JubaQL Client"

version := "1.3.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  // logging
  "org.slf4j"                  %  "slf4j-api"      % "1.7.7",
  "org.slf4j"                  %  "slf4j-log4j12"  % "1.7.7",
  // JSON serialization
  "org.json4s"                 %% "json4s-native"  % "3.2.10",
  // parsing of program arguments
  "com.github.scopt"           %% "scopt"          % "3.2.0",
  // for making HTTP requests
  "net.databinder.dispatch"    %% "dispatch-core"  % "0.11.2",
  // for comfortable JubaQL input
  "jline"                      % "jline"           % "2.12",
  // for testing
  "org.scalatest"     %% "scalatest"               % "2.2.1"   % "test",
  "net.databinder"    %% "unfiltered-filter"       % "0.8.2"   % "test",
  "net.databinder"    %% "unfiltered-netty-server" % "0.8.2"   % "test"
)

// disable parallel test execution to avoid BindException when mocking
// HTTP servers
parallelExecution in Test := false

// add the "start-script" task as per
// <https://github.com/sbt/sbt-start-script#details>
seq(SbtStartScript.startScriptForClassesSettings: _*)
