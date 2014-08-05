import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.protocol._
import org.apache.commons.io._
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import scala.util.Random
import scala.util.matching.Regex
import akka.actor._
import scala.concurrent.duration.Duration
import org.apache.http.util.EntityUtils


object responseTest {

  def main(args: Array[String]): Unit = {
    val client = HttpClientBuilder.create().build()
    val get = new HttpGet("http://kernel-example.com:8080/business-checking.html")
    val response = IOUtils.toString((client.execute(get).getEntity.getContent))
    println(response)
  }
}