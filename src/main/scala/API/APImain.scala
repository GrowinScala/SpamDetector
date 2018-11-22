import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import spray.httpx.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsValue}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

import scala.concurrent.ExecutionContext

case class Message(content: String)
object MessageJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val PortofolioFormats = jsonFormat1(Message)
}

object Server extends App {

  override def main(args: Array[String]) {
    val host = "0.0.0.0"
    val port = 8080

    implicit val system: ActorSystem = ActorSystem("SpamOrHam")
    implicit val executor: ExecutionContext = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val route =
      post {
        entity(as[JsValue]) { json =>
          complete(new Main(json.asJsObject.getFields("content").head.toString()).convertToResponse )
        }
      }

    Http().bindAndHandle(route, host, port)
  }
}
