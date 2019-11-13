package jmpicnic.gql

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.server.Route
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.concurrent.ExecutionContext

class HttpService(route: Route)
                 (implicit executionContext: ExecutionContext,
                  actorSystem: ActorSystem)
	extends Configuration {

	val corsSettings = CorsSettings.defaultSettings.withAllowedMethods(List(GET, POST, PUT, HEAD, OPTIONS, DELETE))//.copy(allowedMethods = List(GET, POST, PUT, HEAD, OPTIONS, DELETE))

	val routes: Route =
		cors(corsSettings) {
			route
		}
}
