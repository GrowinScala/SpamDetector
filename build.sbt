name := "SpamDetect"

version := "0.1"

scalaVersion := "2.11.6"
val akkaV = "2.5.13"
val sprayV = "1.3.1"

libraryDependencies ++= Seq(
  //Breeze
  "org.scalanlp" %% "breeze" % "0.13.2",
  "org.scalanlp" %% "breeze-natives" % "0.13.2",


  //Akka HTTP
  "com.typesafe.akka" %% "akka-actor" % akkaV,
  "com.typesafe.akka" %% "akka-stream" % akkaV,
  "com.typesafe.akka"   %%  "akka-remote"    % akkaV,
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.0",

  //Spray
  "io.spray"            %%  "spray-can"     % sprayV,
  "io.spray"            %%  "spray-routing" % sprayV,
  "io.spray"		  %%  "spray-client"  % sprayV,
  "io.spray"		  %%  "spray-json"  % "1.3.1"
)

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
resolvers += "spray repo" at "http://repo.spray.io"
