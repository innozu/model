package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._
import com.innovenso.townplanner.model.samples

case class Risk(
    key: Key = Key("risk"),
    sortKey: SortKey = SortKey.next,
    typeOfRisk: TypeOfRisk,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Element
    with HasDescription
    with HasLinks
    with HasCriticality
    with HasContext
    with CanBeAssociated {

  val layer: Layer = StrategyLayer
  val aspect: Aspect = NoStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Risk",
    classOf[Risk]
  )

  def withProperty(property: Property): Risk =
    copy(properties = this.properties + (property.key -> property))
}

trait TypeOfRisk {
  def name: String
}

case object TechnicalDebt extends TypeOfRisk {
  val name: String = "Technical Debt"
}

case object SecurityVulnerability extends TypeOfRisk {
  val name: String = "Security Vulnerability"
}

case object ComplianceRisk extends TypeOfRisk {
  val name: String = "Compliance Risk"
}

object TypeOfRisk {
  def random: TypeOfRisk = samples.randomInt(3) match {
    case 1 => TechnicalDebt
    case 2 => SecurityVulnerability
    case 3 => ComplianceRisk
  }
}

trait HasRisks extends HasModelComponents with HasRelationships {
  def risks: List[Risk] = components(classOf[Risk])
  def risk(key: Key): Option[Risk] =
    component(key, classOf[Risk])
}

case class RiskConfigurer(
    modelComponent: Risk,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[Risk]
    with CanConfigureDescription[Risk]
    with CanConfigureLinks[Risk]
    with CanConfigureAssociations[Risk]
    with CanConfigureContext[Risk]
    with CanConfigureCriticality[Risk] {
  def as(
      body: RiskConfigurer => Any
  ): Risk = {
    body.apply(this)
    propertyAdder.townPlan
      .component(modelComponent.key, modelComponent.getClass)
      .get
  }
}

trait CanAddRisks extends CanAddProperties with CanAddRelationships {
  def describes(
      risk: Risk
  ): RiskConfigurer = RiskConfigurer(has(risk), this, this)

  def hasRandom(risk: Risk): Risk = describesRandom(risk) as { _ => }
  def describesRandom(risk: Risk): RiskConfigurer = {
    val configurer = RiskConfigurer(has(risk), this, this)
    val body = { it: RiskConfigurer =>
      it has Title.random
      Description.randoms.foreach(it.has)
      Link.randoms.foreach(it.has)
      it ratesImpactAs Criticality.random
      Context.randoms.foreach(it.has)
    }
    body.apply(configurer)
    configurer
  }

}
