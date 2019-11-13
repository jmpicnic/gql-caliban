package jmpicnic.gql

import scala.concurrent.{ExecutionContext, Future}

object ThingRepo {
	val store = List(
		Thing(1, "asdf", 22.0),
		Thing(2, "qwer", 33.3),
		Thing(3, "zaxcv", 44.4)
	)

}

class ThingRepo(implicit ec: ExecutionContext) extends Filter.Repo[Thing] {

	override type Lifted[T] = Thing => T
	override type CTX = Nothing
	override implicit val stringOperable: Operable[String] = new Operable[String] {
		override def lift(ctx: Option[CTX] = None): String => Lifted[String] = s => th => s
		override def eql(l: Lifted[String], r: Lifted[String]): Lifted[Boolean] = th => l(th) == r(th)
		override def gt(l: Lifted[String], r: Lifted[String]): Lifted[Boolean] = th => l(th) > r(th)
		override def lt(l: Lifted[String], r: Lifted[String]): Lifted[Boolean] = th => l(th) < r(th)
		override def ge(l: Lifted[String], r: Lifted[String]): Lifted[Boolean] = th => l(th) >= r(th)
		override def le(l: Lifted[String], r: Lifted[String]): Lifted[Boolean] = th => l(th) <= r(th)
	}
	override implicit val doubleOperable: Operable[Double] = new Operable[Double] {
		override def lift(ctx: Option[CTX] = None): Double => Lifted[Double] = s => th => s
		override def eql(l: Lifted[Double], r: Lifted[Double]): Lifted[Boolean] = th => l(th) == r(th)
		override def gt(l: Lifted[Double], r: Lifted[Double]): Lifted[Boolean] = th => l(th) > r(th)
		override def lt(l: Lifted[Double], r: Lifted[Double]): Lifted[Boolean] = th => l(th) < r(th)
		override def ge(l: Lifted[Double], r: Lifted[Double]): Lifted[Boolean] = th => l(th) >= r(th)
		override def le(l: Lifted[Double], r: Lifted[Double]): Lifted[Boolean] = th => l(th) <= r(th)
	}
	override implicit val longOperable: Operable[Long] = new Operable[Long] {
		override def lift(ctx: Option[CTX] = None): Long => Lifted[Long] = s => th => s
		override def eql(l: Lifted[Long], r: Lifted[Long]): Lifted[Boolean] = th => l(th) == r(th)
		override def gt(l: Lifted[Long], r: Lifted[Long]): Lifted[Boolean] = th => l(th) > r(th)
		override def lt(l: Lifted[Long], r: Lifted[Long]): Lifted[Boolean] = th => l(th) < r(th)
		override def ge(l: Lifted[Long], r: Lifted[Long]): Lifted[Boolean] = th => l(th) >= r(th)
		override def le(l: Lifted[Long], r: Lifted[Long]): Lifted[Boolean] = th => l(th) <= r(th)
	}
	override implicit val booleanOperable: Operable[Boolean] = new Operable[Boolean] {
		override def lift(ctx: Option[CTX] = None): Boolean => Lifted[Boolean] = s => th => s
		override def eql(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) == r(th)
		override def gt(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) > r(th)
		override def lt(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) < r(th)
		override def ge(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) >= r(th)
		override def le(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) <= r(th)
	}


	//override def find(p: Proposition): Future[Seq[Thing]] =
	override def and(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) && r(th)
	override def or(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean] = th => l(th) || r(th)
	override def not(s: Lifted[Boolean]): Lifted[Boolean] = th => !s(th)
	override def find(p: Proposition): Future[Seq[Thing]] = Future{ThingRepo.store.filter(p.ev())}

	object SR extends Reference[String] {
		override def ev(ctx: Option[CTX] = None): Lifted[String] = _.s
	}
	object IR extends Reference[Long] {
		override def ev(ctx: Option[CTX] = None): Lifted[Long] = _.i
	}
	object DR extends Reference[Double] {
		override def ev(ctx: Option[CTX] = None): Lifted[Double] = _.d
	}

}
