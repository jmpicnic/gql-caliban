package jmpicnic.gql


import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.stream.ActorMaterializer
import jmpicnic.gql.EndpointService.Endpoint

import scala.concurrent.ExecutionContext
import scala.io.StdIn

object MicroServer {

}

trait MicroServer
	extends App with Configuration with LogEnabled {

	protected val endpoints: List[Endpoint]

	// $COVERAGE-OFF$Main Application Wrapper
	implicit val actorSystem = ActorSystem()
	implicit val executor: ExecutionContext = actorSystem.dispatcher
	implicit val loger: LoggingAdapter = Logging(actorSystem, getClass.toString)
	implicit val actorMaterializer: ActorMaterializer = ActorMaterializer()

	def boot() {

		//implicit val authService = new AuthenticationService(new AutoValidate)
		//implicit val authService = new AuthService(new AWSCognitoValidation(authCognito, log)) Use this Service for AWS ;-)


		//val systemServices = SystemServices(authService)


		val httpService = new EndpointService(endpoints)

		val bindingFuture = Http().bindAndHandle(httpService.routes, httpHost, httpPort)

		log.info(s"Bound server to $httpHost:$httpPort")
		log.info(s"Server online at http://localhost:${httpPort}/\n<Assigned Host: $httpHost>\nPress RETURN to stop...")
		StdIn.readLine() // let it run until user presses return
		bindingFuture
			.flatMap(_.unbind()) // trigger unbinding from the port
			.onComplete(_ => actorSystem.terminate()) // and shutdown when done
	}
	// $COVERAGE-ON$
}
