package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._
import com.innovenso.townplanner.model.samples

case class ItSystem(
    key: Key = Key("it system"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Element
    with HasDescription
    with HasArchitectureVerdict
    with HasCriticality
    with HasLinks
    with HasExternalIds
    with HasSWOT
    with HasFatherTime
    with HasResilienceMeasures
    with HasPlatformLayerProperties
    with HasTagProperties
    with HasSecurityMeasures
    with CanBeFlowSource
    with CanBeFlowTarget
    with CanTrigger
    with CanBeTriggered
    with CanRealize
    with CanBeAssociated
    with CanCompose
    with CanBeComposedOf
    with CanBeImplemented
    with CanImplement
    with CanBeImpacted
    with CanBeDelivered
    with CanBeKnown
    with CanInteractWithDataObjects {
  val layer: Layer = ApplicationLayer
  val aspect: Aspect = ActiveStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "IT System",
    classOf[ItSystem]
  )

  def withProperty(property: Property): ItSystem =
    copy(properties = this.properties + (property.key -> property))
}

trait HasItSystems extends HasModelComponents with HasRelationships {
  def systems: List[ItSystem] = components(classOf[ItSystem])
  def system(key: Key): Option[ItSystem] = component(key, classOf[ItSystem])
  def platformSystems(itPlatform: ItPlatform): List[ItSystem] =
    directOutgoingDependencies(
      itPlatform,
      classOf[Composition],
      classOf[ItSystem]
    )
  def systemPlatform(itSystem: ItSystem): Option[ItPlatform] =
    directIncomingDependencies(
      itSystem,
      classOf[Composition],
      classOf[ItPlatform]
    ).headOption

  def realizedArchitectureBuildingBlocks(
      itSystem: ItSystem
  ): List[ArchitectureBuildingBlock] = directOutgoingDependencies(
    itSystem,
    classOf[Realization],
    classOf[ArchitectureBuildingBlock]
  )
  def realizingSystems(
      architectureBuildingBlock: ArchitectureBuildingBlock
  ): List[ItSystem] = directIncomingDependencies(
    architectureBuildingBlock,
    classOf[Realization],
    classOf[ItSystem]
  )
}

case class ItSystemConfigurer(
    modelComponent: ItSystem,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[ItSystem]
    with CanConfigureDescription[ItSystem]
    with CanConfigureLinks[ItSystem]
    with CanConfigureExternalIds[ItSystem]
    with CanConfigureSWOT[ItSystem]
    with CanConfigureFatherTime[ItSystem]
    with CanConfigureCompositionSource[ItSystem]
    with CanConfigureCompositionTarget[ItSystem]
    with CanConfigureArchitectureVerdict[ItSystem]
    with CanConfigureCriticality[ItSystem]
    with CanConfigureResilienceMeasures[ItSystem]
    with CanConfigureFlowSource[ItSystem]
    with CanConfigureFlowTarget[ItSystem]
    with CanConfigureTriggerSource[ItSystem]
    with CanConfigureTriggerTarget[ItSystem]
    with CanConfigureAssociations[ItSystem]
    with CanConfigureRealizationSource[ItSystem]
    with CanConfigureImplementationTarget[ItSystem]
    with CanConfigureImplementationSource[ItSystem]
    with CanConfigureDeliveryTarget[ItSystem]
    with CanConfigureKnowledgeTarget[ItSystem]
    with CanConfigurePlatformLayerProperties[ItSystem]
    with CanConfigureTagProperties[ItSystem]
    with CanConfigureSecurityMeasures[ItSystem]
    with CanConfigureDataObjectInteractionSource[ItSystem] {
  def as(
      body: ItSystemConfigurer => Any
  ): ItSystem = {
    body.apply(this)
    propertyAdder.townPlan
      .system(modelComponent.key)
      .get
  }
}

trait CanAddItSystems extends CanAddProperties with CanAddRelationships {
  def describes(system: ItSystem): ItSystemConfigurer =
    ItSystemConfigurer(has(system), this, this)

  def hasRandom(itSystem: ItSystem): ItSystem = describesRandom(itSystem) as {
    _ =>
  }

  def describesRandom(itSystem: ItSystem): ItSystemConfigurer = {
    val configurer = ItSystemConfigurer(has(itSystem), this, this)
    val body = { it: ItSystemConfigurer =>
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
      samples.times(5, i => it provides SecurityMeasure.random)
      SWOT.randoms.foreach(it.has)
    }
    body.apply(configurer)
    configurer
  }

}
