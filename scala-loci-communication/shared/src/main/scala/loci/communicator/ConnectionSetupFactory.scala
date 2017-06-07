package loci
package communicator

object ConnectionSetupFactory {
  type Properties = Map[String, List[String]]
}

trait ConnectionSetupFactory[+P <: ProtocolCommon] {
  val schemes: Seq[String]

  type Properties

  final def listener(url: String, props: ConnectionSetupFactory.Properties):
      Option[Listener[P]] =
    setup(url, props, listener)

  final def connector(url: String, props: ConnectionSetupFactory.Properties):
      Option[Connector[P]] =
    setup(url, props, connector)

  private def setup[S <: ConnectionSetup[_]](
      url: String, props: ConnectionSetupFactory.Properties,
      setup: (String, String, String, Properties) => Option[S]): Option[S] =
    (schemes
      collectFirst (Function unlift { scheme =>
        val prefix = scheme + "://"
        val prefixLength = prefix.length
        if ((url substring (0, prefixLength) compareToIgnoreCase prefix) == 0)
          Some((scheme, url substring prefixLength))
        else
          None
      })
      flatMap { case (scheme, location) =>
        setup(url, scheme, location, properties(props))
      })

  protected def properties(
  	implicit props: ConnectionSetupFactory.Properties): Properties

  protected def listener(
      url: String, scheme: String, location: String, properties: Properties):
    Option[Listener[P]]

  protected def connector(
      url: String, scheme: String, location: String, properties: Properties):
    Option[Connector[P]]
}
