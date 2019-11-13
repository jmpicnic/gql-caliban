package jmpicnic.gql

import akka.NotUsed
import akka.actor.ActorSystem
import akka.http.scaladsl.common.EntityStreamingSupport
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, Materializer, OverflowStrategy}
import akka.util.ByteString
import caliban.ResponseValue.StreamValue
import caliban.{GraphQL, ResponseValue}
import zio.DefaultRuntime
//import com.cruxsystems.common.protocol.http.server.EndpointService
//import com.cruxsystems.common.util.LogEnabled
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Json}
import zio.{Fiber, IO, Ref}

object GQLEndpoint {
  val runtime =  new DefaultRuntime {}
  val newline = ByteString("\n")
  implicit val streamSupport = EntityStreamingSupport.json()
    .withContentType(ContentTypes.`application/json`)
    .withParallelMarshalling(parallelism = 8, unordered = true)
    .withFramingRenderer(Flow[ByteString].map(bs => bs ++ newline))

  import com.typesafe.scalalogging.Logger


  def makeWebSocketService[Q, M, S](interpreter: GraphQL[Any, Q, M, S], subscribers: Ref[Map[Int, Fiber[Throwable, Unit]]], log: Logger)(implicit fm: Materializer): Flow[Message, Message, Any] = {
    val (akkaQueue, internalAkkaSource) = Source.queue[String](0, OverflowStrategy.fail).preMaterialize
    val incomingMessages: Sink[Message, NotUsed] = Flow[Message].map {
        case TextMessage.Strict(msg) =>
          log.info(s"Processing Message $msg")
          io.circe.parser.parse(msg).flatMap(subscriptionDecoder.decodeJson(_)).map {
            case GQLSubscriptionControl(None, "connection_init", _) =>
              log.info(s"Connection init: $msg")
              akkaQueue.offer("""{"type":"connection_ack"}""")
            case GQLSubscriptionControl(None, "connection_terminate", _) =>
              log.info(s"Terminating Connection $msg")
            case GQLSubscriptionControl(Some(id), "start", Some(GraphQLQuery(query, opName, variables))) =>
              log.info(s"Subscription Start $msg")
              val qExecutor = interpreter.execute(query).flatMap {
                rv: ResponseValue =>
                  log.info(s"Message processed by interpreter with result $rv")
                  rv match {
                    case ResponseValue.ObjectValue((fieldName, StreamValue(zioStream)) :: Nil) =>
                      log.info(s"Message is a Subscription with operation: $fieldName and stream: $zioStream")
                      zioStream.foreach{
                        case chunk: ResponseValue.ObjectValue =>
                          IO.succeed(akkaQueue.offer(s"""{"type":"data","id":$id,"payload":${chunk.toString}}\n"""))
                      }.fork.flatMap(fiber => subscribers.update(_.updated(id, fiber)))
                  }
              }
              runtime.unsafeRunAsync_(qExecutor)
              akkaQueue.offer(s"""{"type":"data","id":$id,"payload":{"msg":"Setup up subscription", "subs":"${query.replaceAllLiterally("\n", " ")}"}}""")
            case GQLSubscriptionControl(Some(id), "stop", _) =>
              log.info(s"Subscription End $msg")
              akkaQueue.offer(s"""{"type":"complete","id":$id,"payload":"Cancelled Subscription"}""")
              subscribers.get.flatMap(map => IO.whenCase(map.get(id)) { case Some(fiber) => fiber.interrupt })
          }
        case ts: TextMessage.Streamed =>
          ts.textStream.runWith(Sink.ignore)
        case br: BinaryMessage =>
          br.dataStream.runWith(Sink.ignore)
      }.to(Sink.ignore)
    Flow.fromSinkAndSource(incomingMessages, internalAkkaSource.log("AkkaStream outbound>>").map(TextMessage(_)))
  }

  case class GraphQLQuery(query: String, operationName: Option[String], variables: Option[Json] = None)
  implicit val queryDecoder: Decoder[GraphQLQuery] = deriveDecoder[GraphQLQuery]

  //{"type":"connection_init","payload":{}}
  //{"id":"1","type":"start","payload":{"query":"subscription {\n  thingSubscribe(il: [23345,2345]) {\n    i, s\n  }\n}\n","variables":{}}}
  // {"id":"1","type":"stop"}
  // {"type":"connection_terminate","payload":null}
  case class GQLSubscriptionControl(id: Option[Int], `type`: String, payload: Option[GraphQLQuery])
  implicit val subscriptionDecoder: Decoder[GQLSubscriptionControl] = deriveDecoder[GQLSubscriptionControl]

  def apply[Q, M, S](interpreter: GraphQL[Any, Q, M, S])(implicit as: ActorSystem, fm: ActorMaterializer) =
    runtime.unsafeRun(for {
      subscribers <- Ref.make(Map.empty[Int, Fiber[Throwable, Unit]])
    } yield new GQLEndpoint(interpreter, subscribers))


}

class GQLEndpoint[-Q, -M, -S](interpreter: GraphQL[Any, Q, M, S], subscribers: Ref[Map[Int, Fiber[Throwable, Unit]]])(implicit actorSystem: ActorSystem, materializer: Materializer)
  extends EndpointService.Endpoint with LogEnabled {
  import GQLEndpoint._

  override val route: Route = path("api") {
    post {
      entity(as[GraphQLQuery]) { q =>
        log.info(s"Received Query as: $q")
        runtime.unsafeRun {
          interpreter.execute(q.query).map {
            case o: ResponseValue.ObjectValue => complete(o.toString)
            case other =>
              import akka.http.scaladsl.model._
              complete(HttpEntity(contentType = ContentTypes.`text/plain(UTF-8)`, s"Internal Error to request: $q => $other"))
          }
        }
      } ~ entity(as[String]) {
        e =>
          log.error(s"FOUND UNEXPECTED CONTENT: $e")
          complete(StatusCodes.BadRequest -> HttpEntity(contentType = ContentTypes.`text/plain(UTF-8)`, s"Request not interpreted: $e"))
      }
    } ~
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h4>This is the URL for GraphQL Server api, please use POST with Query/Mutation/Subscription to access its contents</h4>"))
      }
  } ~ path("ws") {
    get {
      log.info("Received get request at aivdm/ws")
      val wsHandler = handleWebSocketMessagesForProtocol(makeWebSocketService(interpreter, subscribers, log), "graphql-ws")
      log.info(s"Processed get request at aivdm/ws: wsHandler $wsHandler")
      wsHandler
    }
  }
}
