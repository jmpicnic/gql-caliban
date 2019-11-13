package jmpicnic.gql

import com.typesafe.scalalogging.Logger


trait LogEnabled {
	protected val log: Logger = Logger(this.getClass)
}
