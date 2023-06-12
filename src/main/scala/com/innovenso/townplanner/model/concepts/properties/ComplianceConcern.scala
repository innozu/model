package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.meta.SortKey

abstract class ComplianceConcern extends Property {
  val key: Key = Key("compliance")
  val canBePlural: Boolean = true
  val sortKey: SortKey = SortKey.next
  def name: String
  def description: String
}

case class PrivacyCompliance(description: String) extends ComplianceConcern {
  val name = "Privacy"
}

case class PCICompliance(description: String) extends ComplianceConcern {
  val name = "PCI"
}

case class HealthDataCompliance(description: String) extends ComplianceConcern {
  val name = "Health records"
}

case class Compliance(name: String, description: String)
    extends ComplianceConcern

trait HasDataProtectionConcerns extends HasProperties {
  def privacyComplianceConcerns: List[PrivacyCompliance] =
    props(classOf[PrivacyCompliance])
  def pciComplianceConcerns: List[PCICompliance] =
    props(classOf[PCICompliance])
  def healthDataComplianceConcerns: List[HealthDataCompliance] =
    props(classOf[HealthDataCompliance])
  def otherComplianceConcerns: List[Compliance] = props(classOf[Compliance])
  def complianceConcerns: List[ComplianceConcern] = props(
    classOf[ComplianceConcern]
  )
}

trait CanConfigureDataProtectionConcerns[
    ModelComponentType <: HasDataProtectionConcerns
] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def dealsWith(concern: ComplianceConcern): ModelComponentType =
    propertyAdder.withProperty(modelComponent, concern)
}
