package jmpicnic.gql



object ThingMicroServer extends MicroServer {

  lazy val endpoints: List[EndpointService.Endpoint] = GQLEndpoint.runtime.unsafeRun(ThingGQLEndpoint.buildEndpoint(Thing.store)) :: Nil

  // Start the server
  boot()

}
