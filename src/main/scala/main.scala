import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.protocol._
import org.apache.commons.io._
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import scala.util.Random
import scala.util.matching.Regex

trait GenerateExampleData {
  private[this] var continueRunning = false
  private[this] val home = "http://kernel-example.com:8080"

  case class Profile(id: String, userAgent: Option[String], ip: Option[String])
  def randomUserAgent = randomElement(UserAgents)
  def randomIp() = if (random.nextDouble <= ipProbability) Some(randomElement(ips)) else None
  val profiles = List.fill(profileCount)(Profile(newUuid, Some(randomUserAgent), randomIp()))
  def randomProfile() = randomElement(profiles)

  // Possible Options: 
  //    Change to use gattling / scripts
  //    use akka scheduler instead of thread.sleep
  //    delays aren't long enough to be realistic, sped up for testing
  //
  // Unsure if these scenarios technically trigger the data we need
  private[this] def pageVisit(client: HttpClient, url: String): String = {
    println("pageVisit: " + url)
    val httpGetOne = new HttpGet(url)
    val context: HttpContext  = new BasicHttpContext
    httpGetOne.setHeader("If-Modified-Since","11/26/2012");
    IOUtils.toString((client.execute(httpGetOne, context).getEntity.getContent))
  }

  private[this] def populateExtensions(response: String): List[String] = {
    val pattern = """<a href(.*?)>""".r
    val rawList: List[String] = (pattern findAllIn response).toList
    val refinedList = rawList.map(_.drop(9).takeWhile(_ != '"'))
    refinedList.filter(_.contains(".html")).distinct
  }

  def login: String = {
    val httpclient: HttpClient   = new DefaultHttpClient()
    val context:    HttpContext  = new BasicHttpContext

    val httpGetOne = new HttpGet(home)
    val randomUserRoute = home + "logged-in.html?user_id=" + generateUserId + "&password=" + generatePassword
    val httpGetTwo = new HttpGet(randomUserRoute)
    randomUserRoute
  }

  def toggleOn = if (!continueRunning) {
    continueRunning = true
    runRandomScenarios
  }
  def toggleOff = continueRunning = false

  private[this] def runAScenario(client: HttpClient, url: String, response: String): Unit = {
    Thread.sleep(Random.nextInt(500) + 100)
    val extenstions = populateExtensions(response)

      Random.nextInt(7) match {
        case 0 | 1=> Nil
        case _ => 
          val randomRoute = home + "/" + extenstions(Random.nextInt(extenstions.length))
          runAScenario(client, randomRoute, pageVisit(client, randomRoute))
    }
  }

  // need to implement different users
  private[this] def runRandomScenarios: Unit = {
    while (continueRunning) {
      val httpclient: HttpClient = new DefaultHttpClient() // maybe makes different users
      println("new client")
      Thread.sleep(Random.nextInt(700) + 500)
      runAScenario(httpclient, home, pageVisit(httpclient, home))
    }  
  }

  private[this] def generateUserId = Random.nextString(7) 
  private[this] def generatePassword = Random.nextString(10)

  // double check these are correect,
  //  taken from banno/kernel/serve/src/main/scala/GenerateFakeDataRoute.scala
  private[this] lazy val WindowsDesktop = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-GB; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6"
  private[this] lazy val MacDesktop = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
  private[this] lazy val LinuxDesktop = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:24.0) Gecko/20100101 Firefox/24.0"

  private[this] lazy val IosTablet = "Mozilla/5.0(iPad; U; CPU iPhone OS 3_2 like Mac OS X; en-us) AppleWebKit/531.21.10 (KHTML, like Gecko) Version/4.0.4 Mobile/7B314 Safari/531.21.10"
  //TODO Android & Windows tables may not be detected correctly
  private[this] lazy val AndroidTablet = "Mozilla/5.0 (Linux; U; Android 3.0; xx-xx; GT-P7100 Build/HRI83) AppleWebkit/534.13 (KHTML, like Gecko) Version/4.0 Safari/534.13"
  private[this] lazy val WindowsTablet = "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; ARM; Trident/6.0)"

  private[this] lazy val IosMobile = "Mozilla/5.0 (iPhone; CPU iPhone OS 5_0 like Mac OS X) AppleWebKit/534.46 (KHTML, like Gecko) Version/5.1 Mobile/9A334 Safari/7534.48.3"
  private[this] lazy val AndroidMobile = "Mozilla/5.0 (Linux; Android 4.2.2; en-us; SAMSUNG GT-I9195 Build/JDQ39) AppleWebKit/535.19 (KHTML, like Gecko) Version/1.0 Chrome/18.0.1025.308 Mobile Safari/535.19"
  private[this] lazy val WindowsMobile = "Mozilla/5.0 (compatible; MSIE 10.0; Windows Phone 8.0; Trident/6.0; IEMobile/10.0; ARM; Touch; NOKIA; Lumia 920)"

  private[this] lazy val UserAgents = Seq(WindowsDesktop, MacDesktop, LinuxDesktop, IosTablet, AndroidTablet, WindowsTablet, IosMobile, AndroidMobile, WindowsMobile)

  private[this] lazy val ips = Seq("69.170.149.10", "4.2.144.24", "4.2.144.240", "4.2.144.248", "4.2.176.192", "4.2.188.96", "4.2.192.16", "4.2.192.64", "4.2.226.128", "4.3.2.255", "4.17.3.64")
  private[this] lazy val ipProbability = 0.9d
              
}

object main extends GenerateExampleData {
  def main(args: Array[String]): Unit = {
    toggleOn
    Thread.sleep(500)
    toggleOff
  }
}