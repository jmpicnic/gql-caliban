package jmpicnic.gql

import scala.collection.mutable

object Thing {


	trait ThingStore {
		type Rep[A] = Thing => A

		val i: Rep[Long] = _.i
		val s: Rep[String] = _.s
		val d: Rep[Double] = _.d

		def all: List[Thing]
		def addThing(th: Thing): Thing
		def findByI(i: Long): List[Thing]
		def findByS(s: String): List[Thing]
	}

	object store extends ThingStore {
		implicit val stringLifter: String => Rep[String] = v => t => v
		implicit val doubleLifter: Double => Rep[Double] = v => t => v
		implicit val longLifter: Long => Rep[Long] = v => t => v
		implicit val booleanLifter: Boolean => Rep[Boolean] = v => t => v

		val thingStore = mutable.Set[Thing](
			Thing(1, "asdf", 22.0),
			Thing(2, "qwer", 33.3),
			Thing(3, "zaxcv", 44.4)
		)

		def all = {

			thingStore.toList
		}
		def filter(v: Rep[Boolean]): List[Thing]  = thingStore.filter(v(_)).toList

		override def addThing(th: Thing): Thing = {
			thingStore += th
			println(s"After adding the thing: $thingStore")
			th
		}

		override def findByI(i: Long): List[Thing] = filter(t => t.i == i)

		override def findByS(s: String): List[Thing] = filter(t => t.s == s)
	}





}

case class Thing(i: Long, s: String, d: Double)
