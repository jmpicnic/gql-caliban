package jmpicnic.gql


import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route

import scala.concurrent.ExecutionContext

object EndpointService {
	trait Endpoint {
		val route: Route
	}
	def apply(ep: Endpoint*)(implicit executionContext: ExecutionContext,
	                         actorSystem: ActorSystem): EndpointService = new EndpointService(ep)

}

class EndpointService(endpoints: Seq[EndpointService.Endpoint])
                     (implicit executionContext: ExecutionContext, actorSystem: ActorSystem)
	extends HttpService(endpoints.map(_.route).tail.fold(endpoints.head.route)((acc, r) => acc ~ r))