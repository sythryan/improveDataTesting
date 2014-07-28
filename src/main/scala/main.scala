import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.protocol._
import org.apache.commons.io._
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import scala.util.Random
import scala.util.matching.Regex
import akka.actor._
import scala.concurrent.duration.Duration

trait GenerateExampleData extends Scheduling {
  private[this] val home = "http://kernel-example.com:8080"

  // Possible Options: 
  //    Change to use gattling / scripts
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

  private[this] def runAScenario(client: HttpClient, url: String, response: String) {
      val extenstions = populateExtensions(response)

      Random.nextInt(7) match {
        case 0 | 1 => println("stop"); Nil
        case _ => 
          val randomRoute = home + "/" + extenstions(Random.nextInt(extenstions.length)) // n must be positive error from Random.nextInt(0)
          val waitSeconds = 0 //+ Random.nextInt(???)
          runWithWait(waitSeconds)(runAScenario(client, randomRoute, pageVisit(client, randomRoute)))
      }
  }

  // need to implement different users
  protected[this] def runRandomScenarios {
        val httpclient: HttpClient = new DefaultHttpClient() // maybe makes different users
        println("new client")

        runAScenario(httpclient, home, pageVisit(httpclient, home))
  }

  private[this] def generateUserId = Random.nextString(7) 
  private[this] def generatePassword = Random.nextString(10)
              
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
      runOnSchedule(delay, frequency)(runRandomScenarios) 
      //^^ This sets `runRandomScenarious` to run every `frequency` seconds until .cancel() is called
    }
    val runLength = 30
    runWithWait(runLength)(cancellable.cancel())
    runWithWait(runLength + 1)(actorSystem.shutdown)
  }
}