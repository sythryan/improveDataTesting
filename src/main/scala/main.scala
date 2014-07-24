import org.apache.http.impl.client.DefaultHttpClient
import java.net.{URLConnection, URL}
import java.util.ArrayList
import java.util.Scanner
import org.apache.http.protocol._
import org.apache.commons.io._
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.HttpEntity
import org.apache.http.util.EntityUtils
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{HttpGet, HttpPost}
import java.io.{BufferedReader, InputStreamReader, InputStream}
import scala.xml._
import java.io._
import scala.util.Random

import scala.util.matching.Regex

trait GenerateExampleData {
  private[this] var continueRunning = false
  private[this] val home = "http://kernel-example.com:8080"

  // Possible Option: Change to use gattling / scripts
  // Unsure if these scenarios technically trigger the data we need
  private[this] def pageVisit(url: String): Unit = {
    println("pageVisit: " + url)
    val httpclient: HttpClient   = new DefaultHttpClient()
    val httpGetOne = new HttpGet(url)
  }

  private[this] def populateExtensions(url: String): List[String] = {
    val httpclient: HttpClient   = new DefaultHttpClient()
    val context:    HttpContext  = new BasicHttpContext

    val httpGetOne = new HttpGet(url)
    val response =  IOUtils.toString((httpclient.execute(httpGetOne, context).getEntity.getContent))
    val pattern = """<a href(.*?)>""".r

    val test: List[String] = (pattern findAllIn response).toList
    val dropped = test.map(_.drop(9).takeWhile(_ != '"'))
    dropped.filter(_.contains(".html")).distinct
  }

  def login = {
    val httpclient: HttpClient   = new DefaultHttpClient()
    val context:    HttpContext  = new BasicHttpContext

    val httpGetOne = new HttpGet(home)
    val httpGetTwo = new HttpGet(home + "logged-in.html?user_id=" + generateUserId + "&password=" + generatePassword)
  }

  def toggleOn = 
    if (!continueRunning)
      println("toggleOn")
      continueRunning = true
      runRandomScenarios
  def toggleOff = continueRunning = false

  private[this] def runAScenario(url: String): Unit = {

    val extenstions = populateExtensions(url)

    Random.nextInt(4) match {
      case 0 => Nil
      case 1 => pageVisit(home + "/" + extenstions(Random.nextInt(extenstions.length)))
      case 2 => 
        val randomRoute = home + "/" + extenstions(Random.nextInt(extenstions.length)) 
        pageVisit(randomRoute)
        runAScenario(randomRoute)
      case 3 => 
        pageVisit(home)
        runAScenario(home)
    }
  }

  // need to implement different users
  private[this] def runRandomScenarios: Unit = {
    while (continueRunning) {
        runAScenario(home)
    }  
  }

  private[this] def generateUserId = Random.nextString(7) 
  private[this] def generatePassword = Random.nextString(10)
              
}

object main extends GenerateExampleData {
  def main(args: Array[String]): Unit = {
    toggleOn
    toggleOn
  }
}