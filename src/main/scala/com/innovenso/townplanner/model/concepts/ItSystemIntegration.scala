package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.language.HasModelComponents
import com.innovenso.townplanner.model.meta._
import com.innovenso.townplanner.model.samples

case class ItSystemIntegration(
    key: Key = Key("integration"),
    source: Key = Key(),
    target: Key = Key(),
    dataObject: Key = Key(),
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
    with HasSecurityMeasures
    with HasThroughput
    with HasInteractions
    with CanBeIllustrated
    with CanBeAssociated
    with CanBeImplemented
    with CanBeImpacted
    with CanBeDelivered
    with CanBeKnown
    with CanInteractWithDataObjects {
  val layer: Layer = ApplicationLayer
  val aspect: Aspect = ActiveStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "IT System Integration",
    classOf[ItSystemIntegration]
  )

  def withProperty(property: Property): ItSystemIntegration =
    copy(properties = this.properties + (property.key -> property))

  def hasSystem(system: ItSystem): Boolean =
    source == system.key || target == system.key

  def hasDataObject(theDataObject: DataObject): Boolean =
    dataObject == theDataObject.key

  def participants: Set[Key] = Set(source, target)

}

trait HasItSystemIntegrations extends HasModelComponents with HasRelationships {
  def systemIntegration(key: Key): Option[ItSystemIntegration] =
    component(key, classOf[ItSystemIntegration])

  def systemIntegrations(system: ItSystem): List[ItSystemIntegration] =
    systemIntegrations.filter(_.hasSystem(system))

  def systemIntegrations(
      source: ItSystem,
      target: ItSystem
  ): List[ItSystemIntegration] = systemIntegrations.filter(it =>
    it.hasSystem(source) && it.hasSystem(target)
  )

  def systemIntegrations(dataObject: DataObject): List[ItSystemIntegration] =
    systemIntegrations.filter(_.hasDataObject(dataObject))

  def systemIntegrations: List[ItSystemIntegration] = components(
    classOf[ItSystemIntegration]
  )

  def systemIntegrationParticipants(
      integration: ItSystemIntegration
  ): Set[ItSystem] =
    integration.participants
      .map(component(_, classOf[ItSystem]))
      .filter(_.nonEmpty)
      .map(_.get)

}

case class ItSystemIntegrationConfigurer(
    modelComponent: ItSystemIntegration,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships,
    source: Option[ItSystem] = None,
    target: Option[ItSystem] = None,
    dataObject: Option[DataObject] = None
) extends CanConfigureTitle[ItSystemIntegration]
    with CanConfigureDescription[ItSystemIntegration]
    with CanConfigureArchitectureVerdict[ItSystemIntegration]
    with CanConfigureCriticality[ItSystemIntegration]
    with CanConfigureLinks[ItSystemIntegration]
    with CanConfigureExternalIds[ItSystemIntegration]
    with CanConfigureSWOT[ItSystemIntegration]
    with CanConfigureFatherTime[ItSystemIntegration]
    with CanConfigureResilienceMeasures[ItSystemIntegration]
    with CanConfigureSecurityMeasures[ItSystemIntegration]
    with CanConfigureThroughput[ItSystemIntegration]
    with CanConfigureInteractions[ItSystemIntegration]
    with CanConfigureAssociations[ItSystemIntegration]
    with CanConfigureImplementationTarget[ItSystemIntegration]
    with CanConfigureDeliveryTarget[ItSystemIntegration]
    with CanConfigureKnowledgeTarget[ItSystemIntegration]
    with CanConfigureIllustrations[ItSystemIntegration]
    with CanConfigureDataObjectInteractionSource[ItSystemIntegration] {

  def carrying(dataObject: DataObject): ItSystemIntegrationConfigurer =
    copy(
      modelComponent = modelComponent.copy(dataObject = dataObject.key),
      dataObject = Some(dataObject)
    )
  def between(system: ItSystem): ItSystemIntegrationConfigurer =
    copy(
      modelComponent = modelComponent.copy(source = system.key),
      source = Some(system)
    )
  def and(system: ItSystem): ItSystemIntegrationConfigurer =
    copy(
      modelComponent = modelComponent.copy(target = system.key),
      target = Some(system)
    )
  def as(
      body: ItSystemIntegrationConfigurer => Any
  ): ItSystemIntegration = {
    propertyAdder.has(
      modelComponent.withTitle(
        Title(s"${source.map(_.title).getOrElse(s"?")} - ${target
            .map(_.title)
            .getOrElse("?")}: ${dataObject.map(_.title).getOrElse("?")}")
      )
    )
    body.apply(this)
    propertyAdder.townPlan
      .systemIntegration(modelComponent.key)
      .get
  }
}

trait CanAddItSystemIntegrations
    extends CanAddProperties
    with CanAddRelationships {
  def describes(
      integration: ItSystemIntegration
  ): ItSystemIntegrationConfigurer =
    ItSystemIntegrationConfigurer(integration, this, this)

  def hasRandomItSystemIntegration(
      source: ItSystem,
      target: ItSystem,
      dataObject: Option[DataObject] = None
  ): ItSystemIntegration =
    describesRandomItSystemIntegration(source, target, dataObject) as { _ => }
  def describesRandomItSystemIntegration(
      source: ItSystem,
      target: ItSystem,
      dataObject: Option[DataObject] = None
  ): ItSystemIntegrationConfigurer = {
    val configurer = ItSystemIntegrationConfigurer(
      ItSystemIntegration(
        source = source.key,
        target = target.key,
        dataObject = dataObject.map(_.key).getOrElse(Key())
      ),
      this,
      this
    )
    val body = { (it: ItSystemIntegrationConfigurer) =>
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
      it has Throughput.randomFrequency
      it has Throughput.randomVolume
      SWOT.randoms.foreach(it.has)
    }
    body.apply(configurer)
    configurer
  }

}
