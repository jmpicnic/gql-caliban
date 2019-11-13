package jmpicnic.gql

import caliban.schema.ArgBuilder.Typeclass
import caliban.schema.{ArgBuilder, Schema}
import caliban.{CalibanError, schema}
import zio.IO

import scala.reflect.runtime.universe._

// Sample
class ThingFilter(override val repo: ThingRepo) extends Filter[Thing](repo) {
	sealed trait ThingReference extends Reference
	override type REFERENCE = ThingReference


	object IR extends ThingReference {
		override val ev: IO[CalibanError.ExecutionError, repo.Reference[_]] = IO.succeed(repo.IR)
		override val ofType: Type = typeOf[Long]
	}
	object DR extends ThingReference {
		override val ev: IO[CalibanError.ExecutionError, repo.Reference[_]] = IO.succeed(repo.DR)
		override val ofType: Type = typeOf[Double]
	}
	object SR extends ThingReference {
		override val ev: IO[CalibanError.ExecutionError, repo.Reference[_]] = IO.succeed(repo.SR)
		override val ofType: Type = typeOf[String]
	}

	override implicit lazy val reifiedReferenceSchema: schema.Schema.Typeclass[ThingReference] = Schema.gen[ThingReference]
	override implicit lazy val reifiedReferenceBuilder: Typeclass[ThingReference] = ArgBuilder.gen[ThingReference]

	override protected implicit lazy val objSchema: schema.Schema.Typeclass[Thing] = Schema.gen[Thing]
}
