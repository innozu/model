package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._

case class ItPlatform(
    key: Key = Key("it platform"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Element
    with HasDescription
    with HasLinks
    with HasArchitectureVerdict
    with HasCriticality
    with HasExternalIds
    with HasFatherTime
    with HasSWOT
    with HasResilienceMeasures
    with CanBeFlowSource
    with CanBeFlowTarget
    with CanTrigger
    with CanBeTriggered
    with CanRealize
    with CanBeAssociated
    with CanBeComposedOf
    with CanBeImpacted
    with CanBeImplemented
    with CanBeKnown
    with CanInteractWithDataObjects {
  val layer: Layer = ApplicationLayer
  val aspect: Aspect = ActiveStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "IT Platform",
    classOf[ItPlatform]
  )

  def withProperty(property: Property): ItPlatform =
    copy(properties = this.properties + (property.key -> property))
}

trait HasItPlatforms extends HasModelComponents with HasRelationships {
  def platforms: List[ItPlatform] = components(classOf[ItPlatform])
  def platform(key: Key): Option[ItPlatform] =
    component(key, classOf[ItPlatform])
  def realizedArchitectureBuildingBlocks(
      itPlatform: ItPlatform
  ): List[ArchitectureBuildingBlock] = directOutgoingDependencies(
    itPlatform,
    classOf[Realization],
    classOf[ArchitectureBuildingBlock]
  )
  def realizingPlatforms(
      architectureBuildingBlock: ArchitectureBuildingBlock
  ): List[ItPlatform] = directIncomingDependencies(
    architectureBuildingBlock,
    classOf[Realization],
    classOf[ItPlatform]
  )
}

case class ItPlatformConfigurer(
    modelComponent: ItPlatform,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[ItPlatform]
    with CanConfigureDescription[ItPlatform]
    with CanConfigureCompositionSource[ItPlatform]
    with CanConfigureArchitectureVerdict[ItPlatform]
    with CanConfigureLinks[ItPlatform]
    with CanConfigureFatherTime[ItPlatform]
    with CanConfigureExternalIds[ItPlatform]
    with CanConfigureSWOT[ItPlatform]
    with CanConfigureCriticality[ItPlatform]
    with CanConfigureResilienceMeasures[ItPlatform]
    with CanConfigureFlowSource[ItPlatform]
    with CanConfigureFlowTarget[ItPlatform]
    with CanConfigureTriggerSource[ItPlatform]
    with CanConfigureTriggerTarget[ItPlatform]
    with CanConfigureAssociations[ItPlatform]
    with CanConfigureRealizationSource[ItPlatform]
    with CanConfigureImplementationTarget[ItPlatform]
    with CanConfigureKnowledgeTarget[ItPlatform]
    with CanConfigureDataObjectInteractionSource[ItPlatform] {
  def as(
      body: ItPlatformConfigurer => Any
  ): ItPlatform = {
    body.apply(this)
    propertyAdder.townPlan
      .platform(modelComponent.key)
      .get
  }
}

trait CanAddItPlatforms extends CanAddProperties with CanAddRelationships {
  def describes(platform: ItPlatform): ItPlatformConfigurer =
    ItPlatformConfigurer(has(platform), this, this)

  def hasRandom(itPlatform: ItPlatform): ItPlatform =
    describesRandom(itPlatform) as { _ => }
  def describesRandom(itPlatform: ItPlatform): ItPlatformConfigurer = {
    val configurer = ItPlatformConfigurer(has(itPlatform), this, this)
    val body = { it: ItPlatformConfigurer =>
      it has Title.random
      Description.randoms.foreach(it.has)
      Link.randoms.foreach(it.has)
      ExternalId.randoms.foreach(r =>
        it isIdentifiedAs (r.id) on r.externalSystemName
      )
      it should ArchitectureVerdict.random
      it ratesImpactAs Criticality.random
      FatherTime.randoms.foreach(ft => it is ft on ft.date)
      it provides ResilienceMeasure.random
      SWOT.randoms.foreach(it.has)
    }
    body.apply(configurer)
    configurer
  }

}
