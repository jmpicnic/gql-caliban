package jmpicnic.gql

import java.time.OffsetDateTime

import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.Value
import caliban.schema.{ArgBuilder, Schema, Step}
import zio.{IO, ZIO}

import scala.concurrent.Future

object Filter {

	trait Repo[OBJ] {
		type Lifted[T]
		type CTX

		trait Operable[T] {
			def lift(ctx: Option[CTX] = None): T => Lifted[T]
			def eql(l: Lifted[T], r: Lifted[T]): Lifted[Boolean]
			def gt(l: Lifted[T], r: Lifted[T]): Lifted[Boolean]
			def lt(l: Lifted[T], r: Lifted[T]): Lifted[Boolean]
			def ge(l: Lifted[T], r: Lifted[T]): Lifted[Boolean]
			def le(l: Lifted[T], r: Lifted[T]): Lifted[Boolean]
		}
		implicit val stringOperable: Operable[String]
		implicit val doubleOperable: Operable[Double]
		implicit val longOperable: Operable[Long]
		implicit val booleanOperable: Operable[Boolean]

		def and(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean]
		def or(l: Lifted[Boolean], r: Lifted[Boolean]): Lifted[Boolean]
		def not(s: Lifted[Boolean]): Lifted[Boolean]

		def find(p: Proposition): Future[Seq[OBJ]]

		trait Term[T] {
			def ev(ctx: Option[CTX] = None): Lifted[T]
		}

		case class Literal[T](v: T)(implicit op: Operable[T]) extends Term[T] {
			override def ev(ctx: Option[CTX] = None): Lifted[T] = op.lift(ctx)(v)
		}
		abstract class Reference[T] extends Term[T]

		sealed trait Proposition { def ev(ctx: Option[CTX] = None): Lifted[Boolean] }

		sealed class Expression[T](f: (Lifted[T], Lifted[T]) => Lifted[Boolean])(l: Reference[T], r: Term[T])
			extends Proposition { def ev(ctx: Option[CTX] = None) = f(l.ev(ctx), r.ev(ctx))}
		case class Eq[T](l: Reference[T], r: Term[T])(implicit op: Operable[T]) extends Expression[T](op.eql)(l, r)
		case class Lt[T](l: Reference[T], r: Term[T])(implicit op: Operable[T]) extends Expression[T](op.lt)(l, r)
		case class Gt[T](l: Reference[T], r: Term[T])(implicit op: Operable[T]) extends Expression[T](op.gt)(l, r)
		case class Le[T](l: Reference[T], r: Term[T])(implicit op: Operable[T]) extends Expression[T](op.le)(l, r)
		case class Ge[T](l: Reference[T], r: Term[T])(implicit op: Operable[T]) extends Expression[T](op.ge)(l, r)
		case class And(clauses: Proposition*) extends Proposition {def ev(ctx: Option[CTX] = None): Lifted[Boolean] = clauses.map(_.ev(ctx)).reduce[Lifted[Boolean]]((acc, e) => and(acc, e))}
		case class Or(clauses: Proposition*) extends Proposition {def ev(ctx: Option[CTX] = None): Lifted[Boolean] = clauses.map(_.ev(ctx)).reduce[Lifted[Boolean]]((acc, e) => or(acc, e))}
		case class Not(clause: Proposition) extends Proposition {def ev(ctx: Option[CTX] = None): Lifted[Boolean] = not(clause.ev(ctx))}
	}

}

abstract class Filter[OBJ](val repo: Filter.Repo[OBJ]) {
	import repo._

	import scala.reflect.runtime.universe._

	trait Term {
		val ofType: Type
		val ev: IO[ExecutionError, repo.Term[_]]
	}

	sealed trait Literal[T] extends Term

	case class S(v: String) extends Literal[String] {
		override val ofType = implicitly[TypeTag[String]].tpe
		override lazy val ev: IO[ExecutionError, repo.Term[String]] = IO.succeed(repo.Literal(v))}
	case class D(v: Double) extends Literal[Double] {
		override val ofType = implicitly[TypeTag[Double]].tpe
		override val ev: IO[ExecutionError, repo.Term[Double]] = IO.succeed(repo.Literal(v))}
	case class L(v: Long) extends Literal[Long] {
		override val ofType = implicitly[TypeTag[Long]].tpe
		override val ev: IO[ExecutionError, repo.Term[Long]] = IO.succeed(repo.Literal(v))}
	case class B(v: Boolean) extends Literal[Boolean]{
		override val ofType = implicitly[TypeTag[Boolean]].tpe
		override val ev: IO[ExecutionError, repo.Term[Boolean]] = IO.succeed(repo.Literal(v))}
	implicit lazy val literalSchema = Schema.gen[Literal[_]]
	implicit lazy val literalArgBuilder = ArgBuilder.gen[Literal[_]]


	trait Reference extends Term {
		override val ev: IO[ExecutionError, repo.Reference[_]]
	}

	type REFERENCE <: Reference
	implicit val reifiedReferenceSchema: Schema.Typeclass[REFERENCE]
	implicit val reifiedReferenceBuilder: ArgBuilder.Typeclass[REFERENCE]

	import caliban.introspection.adt.{__InputValue, __Type, __TypeKind}

	implicit lazy val referenceSchema = new Schema.Typeclass[Reference] {
		override def toType(isInput: Boolean): __Type = {
			__Type(__TypeKind.UNION, Some("Reference"), Some("A Reference to a field in the entity"), interfaces = None,
				possibleTypes = reifiedReferenceSchema.toType(isInput).possibleTypes,
				inputFields = None,
				ofType = None
			)
		}

		override def resolve(value: Reference): Step[Any]= value match {
			case r: REFERENCE => reifiedReferenceSchema.resolve(r)
			case other => Step.NullStep//IO.fail(ExecutionError(s"Cannot find the reference: $other"))
		}
	}

	implicit lazy val referenceBuilder = new ArgBuilder[Reference] {
		override def build(input: Value): IO[ExecutionError, Reference] = reifiedReferenceBuilder.build(input)
	}

	implicit def customTermSchema[T]: Schema.Typeclass[Term] = new Schema.Typeclass[Term] {
		override def toType(isInput: Boolean): __Type = {
			__Type(__TypeKind.UNION, Some("Term"), Some("Atomic Term (reference or literal) that can participate in a logical expression"), interfaces = None,
				possibleTypes = for {
					litTypes <- literalSchema.toType(isInput).possibleTypes
					refTypes <- referenceSchema.toType(isInput).possibleTypes
					r <- Some(litTypes ++ refTypes)
				} yield r,
				inputFields = None, ofType = None)
		}
		override def resolve(value: Term): Step[Any] = value match {
			case lit: Literal[_] => literalSchema.resolve(lit)
			case ref: Reference => referenceSchema.resolve(ref)
		}
	}
	implicit lazy val customTermArgBuilder = new ArgBuilder.Typeclass[Term] {
		override def build(input: Value): IO[ExecutionError, Term] = input match {
			case v: Value.FloatValue => IO.succeed(D(v.value))
			case v: Value.BooleanValue => IO.succeed(B(v.value))
			case v: Value.StringValue => IO.succeed(S(v.value))
			case v: Value.IntValue => IO.succeed(L(v.value))
			case v: Value.EnumValue => referenceBuilder.build(v)
			case other => IO.fail(ExecutionError(s"Unhandled Input Value $other"))
		}
	}

	sealed trait Clause {
		protected[Filter] def ev: IO[ExecutionError, repo.Proposition]
	}
	sealed trait SingleTermClause extends Clause {
		val l: Reference
		val r: Term
		def ofType: Type = l.ofType
	}
	sealed trait CompositeClause extends Clause

	private def toReference[T](in: Term): IO[ExecutionError, repo.Reference[T]] = in.ev.flatMap {
		case r: repo.Reference[T] => IO.succeed(r)
		case other => IO.fail(ExecutionError(s"The term provided is not a Reference: $other"))
	}

	private def expressionParametersBuilder[T](l: Reference, r: Term): ZIO[Any, ExecutionError, (repo.Reference[T], repo.Term[T])] =
		if(!(l.ofType =:= r.ofType)) IO.fail(ExecutionError(s"Incompatible types of arguments for expression: ${l.ofType} <> ${r.ofType}"))
		else for {
			lr <- toReference[T](l)
			rt <- r.ev
		} yield (lr, rt.asInstanceOf[repo.Term[T]])


	protected[Filter] def evaluateClause(c: Clause) = c match {
		case cc: CompositeClause => cc.ev
		case st: SingleTermClause => st.ev
		case other => IO.fail(ExecutionError(s"Unknown type of Clause: $other"))
	}

	case class Eq(l: Reference, r: Term) extends SingleTermClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = l.ofType match {
			case tt if tt =:= typeOf[String] => expressionParametersBuilder[String](l, r).map(t => repo.Eq[String](t._1, t._2))
			case tt if tt =:= typeOf[Double] => expressionParametersBuilder[Double](l, r).map(t => repo.Eq[Double](t._1, t._2))
			case tt if tt =:= typeOf[Boolean] => expressionParametersBuilder[Boolean](l, r).map(t => repo.Eq[Boolean](t._1, t._2))
			case tt if tt =:= typeOf[Long] => expressionParametersBuilder[Long](l, r).map(t => repo.Eq[Long](t._1, t._2))
			case other => IO.fail(ExecutionError(s"Unsupported Term Type: $other"))
		}
	}
	case class Lt(l: Reference, r: Term) extends SingleTermClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = l.ofType match {
			case tt if tt =:= typeOf[String] => expressionParametersBuilder[String](l, r).map(t => repo.Lt[String](t._1, t._2))
			case tt if tt =:= typeOf[Double] => expressionParametersBuilder[Double](l, r).map(t => repo.Lt[Double](t._1, t._2))
			case tt if tt =:= typeOf[Boolean] => expressionParametersBuilder[Boolean](l, r).map(t => repo.Lt[Boolean](t._1, t._2))
			case tt if tt =:= typeOf[Long] => expressionParametersBuilder[Long](l, r).map(t => repo.Lt[Long](t._1, t._2))
			case other => IO.fail(ExecutionError(s"Unsupported Term Type: $other"))
		}
	}
	case class Gt(l: Reference, r: Term) extends SingleTermClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = l.ofType match {
			case tt if tt =:= typeOf[String] => expressionParametersBuilder[String](l, r).map(t => repo.Gt[String](t._1, t._2))
			case tt if tt =:= typeOf[Double] => expressionParametersBuilder[Double](l, r).map(t => repo.Gt[Double](t._1, t._2))
			case tt if tt =:= typeOf[Boolean] => expressionParametersBuilder[Boolean](l, r).map(t => repo.Gt[Boolean](t._1, t._2))
			case tt if tt =:= typeOf[Long] => expressionParametersBuilder[Long](l, r).map(t => repo.Gt[Long](t._1, t._2))
			case other => IO.fail(ExecutionError(s"Unsupported Term Type: $other"))
		}
	}
	case class Le(l: Reference, r: Term) extends SingleTermClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = l.ofType match {
			case tt if tt =:= typeOf[String] => expressionParametersBuilder[String](l, r).map(t => repo.Le[String](t._1, t._2))
			case tt if tt =:= typeOf[Double] => expressionParametersBuilder[Double](l, r).map(t => repo.Le[Double](t._1, t._2))
			case tt if tt =:= typeOf[Boolean] => expressionParametersBuilder[Boolean](l, r).map(t => repo.Le[Boolean](t._1, t._2))
			case tt if tt =:= typeOf[Long] => expressionParametersBuilder[Long](l, r).map(t => repo.Le[Long](t._1, t._2))
			case other => IO.fail(ExecutionError(s"Unsupported Term Type: $other"))
		}
	}
	case class Ge(l: Reference, r: Term) extends SingleTermClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = l.ofType match {
			case tt if tt =:= typeOf[String] => expressionParametersBuilder[String](l, r).map(t => repo.Ge[String](t._1, t._2))
			case tt if tt =:= typeOf[Double] => expressionParametersBuilder[Double](l, r).map(t => repo.Ge[Double](t._1, t._2))
			case tt if tt =:= typeOf[Boolean] => expressionParametersBuilder[Boolean](l, r).map(t => repo.Ge[Boolean](t._1, t._2))
			case tt if tt =:= typeOf[Long] => expressionParametersBuilder[Long](l, r).map(t => repo.Ge[Long](t._1, t._2))
			case other => IO.fail(ExecutionError(s"Unsupported Term Type: $other"))
		}
	}

	case class Not(s: Clause) extends CompositeClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = s match {
			case c: CompositeClause => c.ev.map(repo.Not)
			case st: SingleTermClause => (st.ofType match {
				case s if s =:= typeOf[String] => st.ev
				case d if d =:= typeOf[Double] => st.ev
				case l if l =:= typeOf[Long] => st.ev
				case b if b =:= typeOf[Boolean] => st.ev
				case t if t =:= typeOf[OffsetDateTime] => st.ev
				case other => IO.fail(ExecutionError(s"Unsupported Clause Type: $other"))
			}).map(repo.Not)
		}
	}
	case class And(s: List[Clause]) extends CompositeClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = ZIO.collectAll(s.map(evaluateClause)).map(repo.And(_: _*))
	}
	case class Or(s: List[Clause])extends CompositeClause {
		override protected[Filter] val ev: IO[ExecutionError, repo.Proposition] = ZIO.collectAll(s.map(evaluateClause)).map(repo.Or(_: _*))
	}

	implicit lazy val eqSchema = Schema.gen[Eq]
	implicit lazy val ltSchema = Schema.gen[Lt]
	implicit lazy val leSchema = Schema.gen[Le]
	implicit lazy val gtSchema = Schema.gen[Gt]
	implicit lazy val geSchema = Schema.gen[Ge]

	implicit lazy val customClauseSchema = new Schema.Typeclass[Clause] {
		override def toType(isInput: Boolean): __Type =
			__Type(__TypeKind.OBJECT, Some("Clause"), Some("A Filter Clause to select entities"), interfaces = None,
				possibleTypes = None, //Some(clauseSchemata.map(_.toType(isInput))),
				inputFields = Some(List(
					//				__InputValue("exp", Some("A logical Expression"), () => logicalExpressionSchema.toType(isInput), None),
					__InputValue("eq", Some("An Equality Expression"), () => eqSchema.toType(isInput), None),
					__InputValue("lt", Some("A less than Expression"), () => ltSchema.toType(isInput), None),
					__InputValue("lt", Some("A greater than Expression"), () => gtSchema.toType(isInput), None),
					__InputValue("lt", Some("A less or equal Expression"), () => leSchema.toType(isInput), None),
					__InputValue("lt", Some("A less or equal Expression"), () => geSchema.toType(isInput), None),
					__InputValue("not", Some("A Not Clause"), () => notSchema.toType(isInput), None),
					__InputValue("and", Some("An And Clause"), () => andSchema.toType(isInput), None),
					__InputValue("or", Some("An Or Clause"), () => orSchema.toType(isInput), None),
				)),
				ofType = None
			)

		override def resolve(value: Clause): Step[Any] = value match {
			case eql: Eq => eqSchema.resolve(eql)
			case lt: Lt => ltSchema.resolve(lt)
			case le: Le => leSchema.resolve(le)
			case gt: Gt => gtSchema.resolve(gt)
			case ge: Ge => geSchema.resolve(ge)
			case not: Not => notSchema.resolve(not)
			case and: And => andSchema.resolve(and)
			case or: Or => orSchema.resolve(or)
		}
	}

	//noinspection ConvertExpressionToSAM
	implicit lazy val customClauseArgBuilder: ArgBuilder[Clause] = new ArgBuilder.Typeclass[Clause] {
		override def build(input: Value): IO[ExecutionError, Clause] =
			input match {
				case Value.ObjectValue(fields) =>
					if (fields.size != 1) IO.fail(ExecutionError(s"Invalid Number of fields for a clause, expecting only on1: ${fields.keySet}"))
					else try fields.map(t => clauseArgBuilders(t._1).build(t._2).map(_.asInstanceOf[Clause])).headOption.getOrElse(IO.fail(ExecutionError(s"Cannot identify the clause type from the provided input: ${fields.keySet}")))
					catch {
						case e: Throwable => IO.fail(ExecutionError(s"Unexpected Exception: $e.msg"))
					}
				case other => IO.fail(ExecutionError(s"Unhandled Input Value $other"))
			}
	}

	implicit lazy val notSchema = Schema.gen[Not]
	implicit lazy val andSchema = Schema.gen[And]
	implicit lazy val orSchema = Schema.gen[Or]

	implicit lazy val eqArgBuilder = ArgBuilder.gen[Eq]
	implicit lazy val ltArgBuilder = ArgBuilder.gen[Lt]
	implicit lazy val leArgBuilder = ArgBuilder.gen[Le]
	implicit lazy val gtArgBuilder = ArgBuilder.gen[Gt]
	implicit lazy val geArgBuilder = ArgBuilder.gen[Ge]


	implicit lazy val customNotArgBuilder = new ArgBuilder.Typeclass[Not] {
		override def build(input: Value): IO[ExecutionError, Not] = input match {
			case o: Value.ObjectValue => customClauseArgBuilder.build(o).map(Not)
			case other => IO.fail(ExecutionError(s"Unhandled Input Value $other"))
		}
	}
	implicit lazy val customAndArgBuilder = new ArgBuilder.Typeclass[And] {
		override def build(input: Value): IO[ExecutionError, And] = input match {
			case Value.ListValue(values) => ZIO.collectAll(values.map(customClauseArgBuilder.build)).map(And)
			case other => IO.fail(ExecutionError(s"Unhandled Input Value $other"))
		}
	}
	implicit lazy val customOrArgBuilder = new ArgBuilder.Typeclass[Or] {
		override def build(input: Value): IO[ExecutionError, Or] = input match {
			case Value.ListValue(values) => ZIO.collectAll(values.map(customClauseArgBuilder.build)).map(Or)
			case other => IO.fail(ExecutionError(s"Unhandled Input Value $other"))
		}
	}
	private lazy val clauseArgBuilders = Map[String, ArgBuilder[_]] (
		"eq" -> eqArgBuilder,
		"lt" -> ltArgBuilder,
		"gt" -> gtArgBuilder,
		"le" -> leArgBuilder,
		"ge" -> geArgBuilder,
		"not" -> customNotArgBuilder,
		"and" -> customAndArgBuilder,
		"or" -> customOrArgBuilder
	)

	protected def find(by: Clause): IO[Throwable, List[OBJ]] = by.ev.flatMap { p => zio.ZIO.fromFuture(ec => repo.find(p)).map(_.toList) }
	case class Query(by: Clause => IO[Throwable, List[OBJ]])
	protected implicit val objSchema: Schema.Typeclass[OBJ]
	private implicit val querySchema = Schema.gen[Query]

	import caliban.{GraphQL, RootResolver}
	val interpreter: GraphQL[Any, Query, Unit, Unit] = GraphQL.graphQL(RootResolver(Query(find)))
}