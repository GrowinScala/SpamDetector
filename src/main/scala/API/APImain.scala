import DefinedStrings.APIStrings
import DefinedValues.{APIValues, ThresholdValues}
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import spray.httpx.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsValue}

import scala.concurrent.ExecutionContext

/**
 * JSON schema
 * @param content
 */
case class Message(content: String)
object MessageJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val PortofolioFormats = jsonFormat1(Message)
}

/**
 * Running server
 */
object Server extends App {
  override def main(args: Array[String]) {

    val threshold = new ThresholdValues()
    val APIValues = new APIValues()
    val APIStrings = new APIStrings()

    /**
     * Setting host and port
     */
    val host = APIStrings.APIHost
    val port = APIValues.APIPort

    implicit val system: ActorSystem = ActorSystem(APIStrings.systemName)
    implicit val executor: ExecutionContext = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    /**
     * Getting the SMS content from the client and running the algorithm with it
     */
    val route =
      post {
        entity(as[JsValue]) { json =>
          if (json.asJsObject.getFields(APIStrings.JSONFieldContent).head.toString().length < threshold.maxCharNumber)
            complete(new Main(json.asJsObject.getFields(APIStrings.JSONFieldContent).head.toString()).convertToResponse)
          else complete(HttpResponse(BadRequest, entity = APIStrings.maxCharNumberEntity))
        }
      }

    /**
     * Response to client
     */
    Http().bindAndHandle(route, host, port)

  }
}
