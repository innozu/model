package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.concepts.relationships.{
  Association,
  CanAddRelationships,
  HasRelationships
}
import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

case class RiskRegister(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    title: String = "Risk Register",
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends TimelessView {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Risk Register",
    classOf[RiskRegister]
  )
  val layer: Layer = ApplicationLayer

  def withProperty(property: Property): RiskRegister =
    copy(properties = this.properties + (property.key -> property))

}

trait HasRiskRegister extends HasViews with HasRisks with HasRelationships {
  def riskRegister: Option[CompiledRiskRegister] =
    components(classOf[RiskRegister]).headOption.map(
      RiskRegisterCompiler(_, this).compile
    )
}

trait CanAddRiskRegister extends CanAddProperties with CanAddRelationships {
  def needs(riskRegister: RiskRegister): RiskRegister =
    has(riskRegister)
}

case class CompiledRiskRegister(
    view: RiskRegister,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[RiskRegister]
    with HasRisks
    with HasRelationships
    with HasItSystems
    with HasItContainers
    with HasItPlatforms
    with HasTechnologies
    with HasItSystemIntegrations
    with HasArchitectureBuildingBlocks
    with HasBusinessCapabilities {}

case class RiskRegisterCompiler(
    view: RiskRegister,
    source: HasRiskRegister
) extends ViewCompiler[
      RiskRegister,
      CompiledRiskRegister,
      HasRiskRegister
    ] {
  override def compile: CompiledRiskRegister = CompiledRiskRegister(
    view,
    viewTitle,
    "Data Model",
    viewComponents(risks ::: riskRelationships ::: elementsWithRisk)
  )

  private val risks: List[Risk] =
    source.risks

  private def riskRelationships: List[Association] = risks
    .flatMap(source.relationships(_, classOf[Association]))
    .map(_.asInstanceOf[Association])

  private def elementsWithRisk: List[Element] =
    risks.flatMap(source.directDependencies(_, classOf[Association]))

}
