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
// General Notes:
//   1. We may want to generate some saved usernames and passwords to make them more realistic
//        they are currently random strings
// 

trait GenerateExampleData {
  
  // I'm cringing at making this non functional
  private[this] var continueRunning = false

  // TODO: Generalize scenarios
  //          maybe pass the httpclient as a param

  // Possible Option: Change to use gattling / scripts
  // Unsure if these scenarios technically trigger the data we need
  private[this] def pageVisit = {
    val httpclient: HttpClient   = new DefaultHttpClient()
    val httpGetOne = new HttpGet("http://kernel-example.com:8080/")
  }

  // Is there a cookie that we need to be passing?
  def login = {
    val httpclient: HttpClient   = new DefaultHttpClient()
    val context:    HttpContext  = new BasicHttpContext

    val httpGetOne = new HttpGet("http://kernel-example.com:8080/")
    val response =  IOUtils.toString((httpclient.execute(httpGetOne, context).getEntity.getContent))
    
    val pattern = """<a href(.*?)>""".r

    val test: List[String] = (pattern findAllIn response).toList
    val dropped = test.map(_.drop(9).takeWhile(_ != '"'))
    val filtered = dropped.filter(_.contains(".html")).distinct
    println(filtered)
    // println(().mkString(","))
    // could not find a post that I was expecting (for when clicking login)
    // val httpGetTwo = new HttpGet("http://kernel-example.com:8080/logged-in.html?user_id=" + generateUserId + "&password=" + generatePassword)
  }

  def toggleOn = 
    if (!continueRunning)
      continueRunning = true
      runRandomScenarios
  def toggleOff = continueRunning = false

  private[this] def runRandomScenarios =
    while (continueRunning) {
      // add random time duraions
      // currently biased towards pageVisit
      Random.nextInt(3) match {
        case 0 => login
        case _ => pageVisit
      }  
    }

  // these could use improvement
  private[this] def generateUserId = Random.nextString(7) 
  private[this] def generatePassword = Random.nextString(10)
              
}

object main extends GenerateExampleData {
  def main(args: Array[String]): Unit = {
    login 
  }
}