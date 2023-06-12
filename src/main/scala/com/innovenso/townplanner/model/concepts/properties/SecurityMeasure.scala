package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey
import com.innovenso.townplanner.model.samples
import com.innovenso.townplanner.model.samples.randomInt

sealed trait SecurityMeasure extends Property {
  val key: Key = Key("security-measure")
  val canBePlural: Boolean = true
  val sortKey: SortKey = SortKey.next
  def name: String
  def description: String
  def descriptionOption: Option[String] = Option(description).filter(!_.isBlank)
}

case class EncryptionAtRest(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Encryption at rest"
}

case class EncryptionInTransit(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Encryption in transit"
}

case class StrictAccessControls(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Strict Access Controls"
}

case class PrivilegedAccessMonitoring(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Privileged Access Monitoring"
}

case class NetworkSegmentation(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Network segmentation"
}

case class NetworkSegregation(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Network segregation"
}

case class PhysicalSecurity(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Physical Security"
}

case class Antivirus(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "Antivirus"
}

case class StaticApplicationSecurityTesting(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "SAST"
}

case class DynamicApplicationSecurityTesting(
    description: String = ""
) extends SecurityMeasure {
  val name: String = "DAST"
}

case class OtherSecurityMeasure(name: String, description: String = "")
    extends SecurityMeasure

object SecurityMeasure {
  def fromString(
      value: String,
      description: Option[String] = None
  ): SecurityMeasure = Option(value).map(_.toLowerCase).getOrElse("") match {
    case "sast" => StaticApplicationSecurityTesting(description.getOrElse(""))
    case "dast" => DynamicApplicationSecurityTesting(description.getOrElse(""))
    case "antivirus" => Antivirus(description.getOrElse(""))
    case "physical" | "physical security" =>
      PhysicalSecurity(description.getOrElse(""))
    case "segregation" | "network segregation" =>
      NetworkSegregation(description.getOrElse(""))
    case "segmentation" | "network segmentation" =>
      NetworkSegmentation(description.getOrElse(""))
    case "monitoring" | "privileged Access monitoring" =>
      PrivilegedAccessMonitoring(description.getOrElse(""))
    case "access" | "strict access controls" =>
      StrictAccessControls(description.getOrElse(""))
    case "encryptionintransit" | "encryption in transit" =>
      EncryptionInTransit(description.getOrElse(""))
    case "encryptionatrest" | "encryption at rest" =>
      EncryptionAtRest(description.getOrElse(""))
    case name: String => OtherSecurityMeasure(name, description.getOrElse(""))
  }

  def random: SecurityMeasure = randomInt(11) match {
    case 1 => StaticApplicationSecurityTesting(samples.description)
    case 2 => DynamicApplicationSecurityTesting(samples.description)
    case 3 => Antivirus(samples.description)
    case 4 => PhysicalSecurity(samples.description)
    case 5 => NetworkSegregation(samples.description)
    case 6 => NetworkSegmentation(samples.description)
    case 7 => PrivilegedAccessMonitoring(samples.description)
    case 8 => StrictAccessControls(samples.description)
    case 9 =>
      EncryptionInTransit(samples.description)
    case 10 =>
      EncryptionAtRest(samples.description)
    case 11 => OtherSecurityMeasure(samples.title, samples.description)
  }
}

trait HasSecurityMeasures extends HasProperties {
  def securityMeasures: List[SecurityMeasure] = props(
    classOf[SecurityMeasure]
  )

  def securityMeasure(key: Key): Option[SecurityMeasure] =
    prop(key, classOf[SecurityMeasure])

  def withSecurityMeasure(
      securityMeasure: SecurityMeasure
  ): HasProperties =
    withProperty(securityMeasure)

}

trait CanConfigureSecurityMeasures[
    ModelComponentType <: HasSecurityMeasures
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def provides(securityMeasure: SecurityMeasure): ModelComponentType =
    propertyAdder.withProperty(modelComponent, securityMeasure)
}
