package main

import org.apache.http.impl.client.{BasicCookieStore, DefaultHttpClient}
import org.apache.http.impl.cookie.BasicClientCookie
import org.apache.http.protocol._
import org.apache.commons.io._
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import scala.util.Random
import scala.util.matching.Regex
import akka.actor._
import scala.concurrent.duration.Duration
import org.apache.http.cookie.Cookie
import org.apache.http.util.EntityUtils

trait GenerateExampleData extends Scheduling {
  private[this] val home = "http://kernel-example.com:8080"
  val profileCount = 5

  val random = new scala.util.Random(System.currentTimeMillis / 10000)
  def randomElement[A](seq: Seq[A]) = seq(random.nextInt(seq.size))
  def randomElements[A](n: Int)(seq: Seq[A]): Seq[A] =
    if (n <= 0) Seq.empty[A] else if (seq.size <= n) seq else { val a = randomElement(seq); a +: randomElements(n-1)(seq.filterNot(_ == a)) }

  def randomUserAgent = randomElement(UserAgents)
  def randomIp() = if (random.nextDouble <= ipProbability) Some(randomElement(ips)) else None

  private[this] def getResponse(client: HttpClient, url: String): String = {
    println("getResponse: " + url)
    val httpGetOne = new HttpGet(url)
    IOUtils.toString((client.execute(httpGetOne).getEntity.getContent))
  }

  private[this] def populateExtensions(response: String): List[String] = {
    val pattern = """<a href(.*?)>""".r
    val rawList: List[String] = (pattern findAllIn response).toList
    val refinedList = rawList.map(_.drop(9).takeWhile(_ != '"'))
    refinedList.filter(_.contains(".html")).distinct
  }

  def generateUserID: String = {
    val httpClient = new DefaultHttpClient()
    val url = "http://localhost:9091/kernel.js"
    val response = httpClient.execute(new HttpGet(url)).toString
    val findETagStart = response.indexOf("ETag: ") + 6
    val findETagEnd = response.indexOf(",",79)
    response.substring(findETagStart,findETagEnd)
  }


  def pageVisit(id: String, keywords: String) {
    val client = new DefaultHttpClient()
    val url = "http://kernel-serve.com:9091/institutions/8eac4943-acd6-40d6-b9a0-ecba52bc35ef/profiles/" + id + "/visit?" + keywords
    val get = new HttpGet(url)
    val response = client.execute(get).toString
    println(url)
    println(response)
  }

  runRandomScenarious(id: String) = Random.nextInt(3) match {
      case 0 => println("stop")
                Nil
      case 1 => pageVisit(id, "")
      case _ => pageVisit(id, "keywords=business+checking,business")

  }

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

trait Scheduling {

  protected[this] val actorSystem = ActorSystem()
  private[this] val scheduler = actorSystem.scheduler
  implicit val executor = actorSystem.dispatcher

  def runOnSchedule(delay: Int, frequency: Int)(task: => Unit): Cancellable = {
    val cancellable = scheduler.schedule(
      Duration(delay, "seconds"),
      Duration(frequency, "seconds"))(task)

    cancellable
  }

  def runWithWait(waitTime: Int)(task: => Unit) {
    scheduler.scheduleOnce(Duration(waitTime, "seconds"))(task)
  }
}

object main extends GenerateExampleData with Scheduling {
  def main(args: Array[String]): Unit = {
    val cancellable = {
      val delay = 3
      val frequency = 5 //+ Random.nextInt(???)
      runOnSchedule(delay, frequency)(runRandomScenarious(generateUserID)) 
      //^^ This sets `runRandomScenarious` to run every `frequency` seconds until .cancel() is called
    }
    val runLength = 30
    runWithWait(runLength)(cancellable.cancel())
    runWithWait(runLength + 1)(actorSystem.shutdown)
  }
}