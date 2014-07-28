import com.banno._

BannoSettings.settings

name := "the Collective, tester"

addBannoDependencies(
  "banno-utils",
  // "hbase-persistence-0.94",
  "sentry-client",
  "postgres-persistence",
  "redis-utils",
  // "banno-health",
  "banno-pagerduty"
)

libraryDependencies ++= Seq(
   "net.sourceforge.htmlcleaner" % "htmlcleaner" % "2.2",
   "org.apache.commons" % "commons-io" % "1.3.2",
   "org.apache.httpcomponents" % "httpclient" % "4.1.2"
)
