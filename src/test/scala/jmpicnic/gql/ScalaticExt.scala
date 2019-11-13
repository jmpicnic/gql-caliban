package jmpicnic.gql

import org.scalactic.{AbstractStringUniformity, Uniformity}

object ScalaticExt {
	val whiteSpaceNormalized: Uniformity[String] =
		new AbstractStringUniformity {

			/**
			 * @return the passed string with all whitespaces, tabs and \n converted to single whitespace.
			 */
			def normalized(s: String): String = s.replaceAll("\\s+", " ")

			override def toString: String = "whitespaceNormalized"
		}
}
