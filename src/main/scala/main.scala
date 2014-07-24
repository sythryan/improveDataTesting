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
    Thread.sleep(Random.nextInt(5000) + 1000)
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
      Thread.sleep(Random.nextInt(7000) + 500)
      runAScenario(httpclient, home, pageVisit(httpclient, home))
    }  
  }

  private[this] def generateUserId = Random.nextString(7) 
  private[this] def generatePassword = Random.nextString(10)
              
}

object main extends GenerateExampleData {
  def main(args: Array[String]): Unit = {
    toggleOn
    Thread.sleep(10000)
    toggleOff
  }
}