name := "the Collective, tester"


libraryDependencies ++= {
  val akkaV = "2.1.4"
  Seq("net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.2",
      "org.apache.commons" % "commons-io" % "1.3.2",
      "org.apache.httpcomponents" % "httpclient" % "4.3",
      "com.typesafe.akka"   %%  "akka-actor"    % akkaV)
}
