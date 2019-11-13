package jmpicnic.gql

import org.scalactic.{AbstractStringUniformity, Uniformity}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import zio.DefaultRuntime

import scala.concurrent.ExecutionContext

object FilterSpec {
	val whiteSpaceNormalized: Uniformity[String] =
		new AbstractStringUniformity {

			/**
			 * @return the passed string with all whitespaces, tabs and \n converted to single whitespace.
			 */
			def normalized(s: String): String = s.replaceAll("\\s+", " ")

			override def toString: String = "whitespaceNormalized"
		}
}

class FilterSpec extends WordSpecLike
	with Matchers
	with BeforeAndAfterAll {
	import FilterSpec._

	implicit val ec: ExecutionContext = ExecutionContext.global
	val repo = new ThingRepo
	val underTest = new ThingFilter(repo)

	"The Filter GraphQL Schema" must {
		"00. Have a Query Defined" in {

			val queryInterpreter = underTest.interpreter
			val schema = queryInterpreter.render
			println(s"Schema:\n${schema}")
			schema should equal (
				"""
					|type Thing {
					|  i: Long!
					|  s: String!
					|  d: Double!
					|}
					|
					|input LtInput {
					|  l: Reference!
					|  r: Term!
					|}
					|
					|input LeInput {
					|  l: Reference!
					|  r: Term!
					|}
					|
					|input GeInput {
					|  l: Reference!
					|  r: Term!
					|}
					|
					|input NotInput {
					|  s: Clause!
					|}
					|
					|type Query {
					|  by(eq: EqInput, lt: LtInput, lt: GtInput, lt: LeInput, lt: GeInput, not: NotInput, and: AndInput, or: OrInput): [Thing!]!
					|}
					|
					|input EqInput {
					|  l: Reference!
					|  r: Term!
					|}
					|
					|input OrInput {
					|  s: [Clause!]!
					|}
					|
					|input AndInput {
					|  s: [Clause!]!
					|}
					|
					|input GtInput {
					|  l: Reference!
					|  r: Term!
					|}""".stripMargin) (after being whiteSpaceNormalized)
		}
		"01. Support a simple term" in {
			val queryInterpreter = underTest.interpreter
			val q =
				"""
					|{
					|  by(eq: {l: SR, r: "asdf"}) {
					|    i, s, d
					|	 }
					|}
					|""".stripMargin
			val runtime = new DefaultRuntime {}
			val check = runtime.unsafeRun(queryInterpreter.check(q).fold(_.msg, s => "success"))
			check should be ("success")
			val retrieved= runtime.unsafeRun(queryInterpreter.execute(q).fold(_.msg ,  s => s.toString))
			retrieved should be ("{\"by\":[{\"i\":1,\"s\":\"asdf\",\"d\":22.0}]}")
		}
		"02. Suppot a composite proposition" in {
			val queryInterpreter = underTest.interpreter
			val q =
				"""
					|{
					|  by(or: [
					|           {eq: {l: SR, r: "asdf"}},
					|           {and: [
					|                  {eq: {l: IR, r: 2}},
					|                  {lt: {l: DR, r: 35.0}}
					|                  ]}
					|          ]) {
					|					     i, s, d
					|            }
					|}
					|""".stripMargin

			val runtime = new DefaultRuntime {}
			val check = runtime.unsafeRun(queryInterpreter.check(q).fold(_.msg, s => "success"))
			check should be ("success")
			val result= runtime.unsafeRun(queryInterpreter.execute(q).fold(_.msg ,  s => s.toString))
			result should be ("{\"by\":[{\"i\":1,\"s\":\"asdf\",\"d\":22.0},{\"i\":2,\"s\":\"qwer\",\"d\":33.3}]}")
		}
	}
}
