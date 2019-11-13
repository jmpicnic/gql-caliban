
package jmpicnic.gql

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import caliban.CalibanError.ExecutionError
import caliban.schema.{Schema, SubscriptionSchema}
import caliban.{GraphQL, RootResolver}
import zio.stream.ZStream
import zio.{IO, Queue, Ref, UIO}

object ThingGQLEndpoint {

	implicit def functionSubscriptionSchema[R, E, A, ARG]: SubscriptionSchema[ARG => ZStream[R, E, A]] =
		new SubscriptionSchema[ARG => ZStream[R, E, A]] {}

	case class ByS(s: String)
	case class ByI(i: Long)


	case class ThingQuery(findAll: List[Thing], findByS: ByS => IO[Throwable, List[Thing]], findByI: ByI => IO[Throwable, List[Thing]])
	//case class ThingQuery(findByS: ByS => IO[Throwable, List[Thing]], findByI: ByI => IO[Throwable, List[Thing]])

	implicit lazy val querySchema = Schema.gen[ThingQuery]

	case class CreateThing(i: Long, s: String, d: Double)

	case class ThingMutation(createThing: CreateThing => IO[Throwable, Thing])

	case class ByIList(il: List[Long])
	case class ThingSubscription(thingSubscribe: ByIList => ZStream[Any, ExecutionError, Thing])

	implicit lazy val mutationSchema = Schema.gen[ThingMutation]
	implicit lazy val subscriptionSchema = Schema.gen[ThingSubscription]

	case class Subscription(iVals: List[Long], queue: Queue[Thing])

	def buildEndpoint(universe: Thing.ThingStore)(implicit as: ActorSystem, fm: ActorMaterializer) =
		for {
			subscriptions <- Ref.make(List.empty[Subscription])
		} yield GQLEndpoint(new ThingGQLEndpoint(universe, subscriptions).interpreter)
}

class ThingGQLEndpoint (universe: Thing.ThingStore, subscriptions: Ref[List[ThingGQLEndpoint.Subscription]]) {

	import ThingGQLEndpoint._

	private val queryService = ThingQuery(findAll, findByS, findByI)
	private val mutationService = ThingMutation(createThing)
	private val subscriptionService = ThingSubscription(subscribeByIs)
	private val interpreter: GraphQL[Any, ThingQuery, ThingMutation, ThingSubscription] =
		GraphQL.graphQL(RootResolver(queryService, mutationService, subscriptionService))

	private def createThing(cr: CreateThing): IO[Throwable, Thing] = {
		println(s"Creating thing: $cr")
		val th = Thing(cr.i,cr.s,cr.d)
		val addingEffect = subscriptions.get.flatMap {
			subsList => {
				println(s"Execution notification Effect with subscriptions: $subsList")
				UIO.foreach(subsList) {
					subscription: Subscription => {
						println(s"Iterating on Subscription: $subscription")
						UIO.when(subscription.iVals.contains(th.i)) {
							println(s"Satisfied Condition for subscription: $subscription with $th")
							subscription.queue.offer(th)
								.onInterrupt(subscriptions.update(_.filterNot(_ == subscription))) // Remove subscription if fiber interrupted
						}
					}
				}
			}
		}.map(dummy => universe.addThing(th))
		addingEffect
	}

	private def findAll: List[Thing]  = universe.all
	private def findByS(byS: ByS): IO[Throwable, List[Thing]] = zio.ZIO.succeed {universe.findByS(byS.s)}
	private def findByI(byI: ByI): IO[Throwable, List[Thing]] = zio.ZIO.succeed {universe.findByI(byI.i)}


	private def subscribeByIs(arg: ByIList): ZStream[Any, Nothing, Thing] = {
		println(s"Subscribing this list of i's $arg")
		ZStream.unwrap {
			for {
				queue <- Queue.unbounded[Thing]
				_     <- subscriptions.update(current => Subscription(arg.il, queue) :: current)
			} yield ZStream.fromQueue(queue)
		}
	}

}