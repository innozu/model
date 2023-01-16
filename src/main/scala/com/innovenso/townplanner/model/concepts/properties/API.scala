package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.{Key, SortKey}

case class API(
    authentication: AuthenticationType = NoAuthentication(),
    style: ApiStyle,
    scope: ApiScope,
    ddoSProtection: DDoSProtection = NoDDosProtection(),
    rateLimiting: RateLimiting = NoRateLimiting()
) extends Property {
  val key: Key = Key("api")
  val sortKey: SortKey = SortKey.next
  val canBePlural: Boolean = false
}

trait HasAPI extends HasProperties {
  def api: Option[API] =
    props(classOf[API]).headOption
  def withAPI(api: API): HasProperties =
    withProperty(api)
}

trait CanConfigureAPI[ModelComponentType <: HasAPI] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(api: API): ModelComponentType =
    propertyAdder.withProperty(modelComponent, api)
}

trait AuthenticationType {
  def title: String
  def description: Option[String] = None
}

object AuthenticationType {
  def fromString(
      title: String,
      description: Option[String] = None
  ): AuthenticationType =
    Option(title).filter(_.toLowerCase.trim.nonEmpty) match {
      case Some("api key")              => ApiKey(description)
      case Some("basic authentication") => BasicAuth(description)
      case Some("oauth")                => OAuth(description)
      case Some(t)                      => Authentication(t, description)
      case None                         => NoAuthentication(description)
    }
}

case class ApiKey(override val description: Option[String] = None)
    extends AuthenticationType {
  val title: String = "API Key"
}

case class BasicAuth(override val description: Option[String] = None)
    extends AuthenticationType {
  val title: String = "Basic Authentication"
}

case class OAuth(override val description: Option[String] = None)
    extends AuthenticationType {
  val title: String = "OAuth"
}

case class NoAuthentication(override val description: Option[String] = None)
    extends AuthenticationType {
  val title: String = "none"
}

case class Authentication(
    override val title: String,
    override val description: Option[String] = None
) extends AuthenticationType

trait ApiStyle {
  def title: String
  def description: Option[String] = None
}

object ApiStyle {
  def fromString(value: String, description: Option[String] = None): ApiStyle =
    Option(value).filter(_.toLowerCase.trim.nonEmpty) match {
      case Some("rest") => RestAPI(description)
      case Some("soap") => SoapAPI(description)
      case Some("grpc") => GrpcAPI(description)
      case Some(t)      => OtherAPI(t, description)
      case None         => NoAPI(description)
    }
}
case class RestAPI(override val description: Option[String] = None)
    extends ApiStyle {
  val title: String = "REST"
}
case class SoapAPI(override val description: Option[String] = None)
    extends ApiStyle {
  val title: String = "SOAP"
}
case class GrpcAPI(override val description: Option[String] = None)
    extends ApiStyle {
  val title: String = "GRPC"
}

case class NoAPI(override val description: Option[String] = None)
    extends ApiStyle {
  val title: String = "None"
}

case class OtherAPI(
    override val title: String,
    override val description: Option[String] = None
) extends ApiStyle

trait ApiScope {
  def title: String
  def description: Option[String] = None
}

object ApiScope {
  def fromString(value: String, description: Option[String] = None): ApiScope =
    Option(value).filter(_.toLowerCase.trim.nonEmpty) match {
      case Some("public")         => PublicScope(description)
      case Some("private")        => PrivateScope(description)
      case Some("vpn")            => VpnScope(description)
      case Some("ip whitelisted") => WhitelistingScope(description)
      case Some(t)                => OtherScope(t, description)
      case None                   => PublicScope(description)
    }
}

case class PublicScope(override val description: Option[String] = None)
    extends ApiScope {
  val title: String = "public"
}

case class PrivateScope(override val description: Option[String] = None)
    extends ApiScope {
  val title: String = "private"
}

case class VpnScope(override val description: Option[String] = None)
    extends ApiScope {
  val title: String = "vpn"
}

case class WhitelistingScope(override val description: Option[String] = None)
    extends ApiScope {
  val title: String = "ip whitelisted"
}

case class OtherScope(
    override val title: String,
    override val description: Option[String] = None
) extends ApiScope

trait DDoSProtection {
  def title: String
  def description: Option[String] = None
}

object DDoSProtection {
  def fromString(
      value: String,
      description: Option[String] = None
  ): DDoSProtection =
    Option(value).filter(_.toLowerCase.trim.nonEmpty) match {
      case Some("none") => NoDDosProtection(description)
      case Some(t)      => OtherDDoSProtection(t, description)
      case None         => NoDDosProtection(description)
    }
}

case class NoDDosProtection(override val description: Option[String] = None)
    extends DDoSProtection {
  val title: String = "none"
}

case class OtherDDoSProtection(
    override val title: String,
    override val description: Option[String] = None
) extends DDoSProtection

trait RateLimiting {
  def title: String
  def description: Option[String] = None
}

object RateLimiting {
  def fromString(
      value: String,
      description: Option[String] = None
  ): RateLimiting =
    Option(value).filter(_.toLowerCase.trim.nonEmpty) match {
      case Some("none")                => NoRateLimiting(description)
      case Some("through api gateway") => ApiGatewayRateLimiting(description)
      case Some(t)                     => OtherRateLimiting(t, description)
      case None                        => NoRateLimiting(description)
    }
}

case class NoRateLimiting(override val description: Option[String] = None)
    extends RateLimiting {
  val title: String = "none"
}

case class ApiGatewayRateLimiting(
    override val description: Option[String] = None
) extends RateLimiting {
  val title: String = "through API Gateway"
}

case class OtherRateLimiting(
    override val title: String,
    override val description: Option[String] = None
) extends RateLimiting
