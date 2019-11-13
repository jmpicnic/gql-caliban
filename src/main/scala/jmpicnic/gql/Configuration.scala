package jmpicnic.gql


import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}
object Configuration {

}

trait Configuration {
	val config : Config = ConfigFactory.load()
	protected val httpConfig = config.getConfig("http")



	val httpHost = httpConfig.getString("interface")
	val httpPort = httpConfig.getInt("port")
	val httpSelfTimeout = httpConfig.getDuration("self-timeout")


	private def getDuration(key: String) = FiniteDuration(config.getDuration(key, MILLISECONDS), MILLISECONDS)

}
