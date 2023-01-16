package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._

import scala.annotation.tailrec

case class BusinessCapability(
    key: Key = Key("capability"),
    sortKey: SortKey = SortKey.next,
    title: String,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends Element
    with HasDescription
    with HasLinks
    with HasExternalIds
    with HasArchitectureVerdict
    with HasSWOT
    with HasTagProperties
    with CanBeRealized
    with CanBeServed
    with CanServe
    with CanBeFlowSource
    with CanBeFlowTarget
    with CanBeTriggered
    with CanBeImpacted
    with CanTrigger {
  val layer: Layer = StrategyLayer
  val aspect: Aspect = Behavior
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Business Capability",
    classOf[BusinessCapability]
  )

  def withProperty(property: Property): BusinessCapability =
    copy(properties = this.properties + (property.key -> property))
}

trait HasBusinessCapabilities
    extends HasModelComponents
    with HasEnterprises
    with HasRelationships {
  def businessCapabilities: List[BusinessCapability] = components(
    classOf[BusinessCapability]
  )
  def businessCapability(key: Key): Option[BusinessCapability] =
    component(key, classOf[BusinessCapability])
  def level(businessCapability: BusinessCapability): Int =
    parentBusinessCapability(businessCapability)
      .map(level(_) + 1)
      .getOrElse(0)

  def level0businessCapabilities(
      enterprise: Enterprise
  ): List[BusinessCapability] = directIncomingDependencies(
    enterprise,
    classOf[Serving],
    classOf[BusinessCapability]
  ).filter(cap => parentBusinessCapability(cap).isEmpty)
    .sortWith(_.sortKey < _.sortKey)

  def parentBusinessCapability(
      businessCapability: BusinessCapability
  ): Option[BusinessCapability] = directOutgoingDependencies(
    businessCapability,
    classOf[Serving],
    classOf[BusinessCapability]
  ).headOption
  def enterprise(businessCapability: BusinessCapability): Option[Enterprise] =
    directOutgoingDependencies(
      businessCapability,
      classOf[Serving],
      classOf[Enterprise]
    ).headOption.orElse(
      parentBusinessCapability(businessCapability).flatMap(bc => enterprise(bc))
    )
  def childBusinessCapabilities(
      businessCapability: BusinessCapability
  ): List[BusinessCapability] = directIncomingDependencies(
    businessCapability,
    classOf[Serving],
    classOf[BusinessCapability]
  ).sortWith(_.sortKey < _.sortKey)

  def businessCapabilityMap(
      enterprise: Enterprise
  ): List[BusinessCapability] = {
    val level0 = level0businessCapabilities(enterprise)
    children(level0)
  }

  private def parents(capability: BusinessCapability): Set[BusinessCapability] =
    directOutgoingDependencies(
      capability,
      classOf[Serving],
      classOf[BusinessCapability]
    ).toSet

  private def containsParent(
      capability: BusinessCapability,
      listToScan: List[BusinessCapability]
  ) = {
    val p = parents(capability)
    listToScan.exists(p)
  }

  @tailrec
  private def children(
      capabilities: List[BusinessCapability]
  ): List[BusinessCapability] = {
    val c = businessCapabilities
      .filterNot(capabilities.toSet)
      .filter(cap => containsParent(cap, capabilities))
    if (c.isEmpty) capabilities else children(capabilities ::: c)
  }

  def businessCapabilityHierarchy(
      businessCapability: BusinessCapability
  ): Set[BusinessCapability] =
    (businessCapabilityParentHierarchy(
      businessCapability
    ) ++ children(List(businessCapability))).toSet

  def businessCapabilityParentHierarchy(
      businessCapability: BusinessCapability
  ): List[BusinessCapability] =
    if (parentBusinessCapability(businessCapability).isDefined)
      businessCapability :: businessCapabilityParentHierarchy(
        parentBusinessCapability(businessCapability).get
      )
    else List(businessCapability)
}

case class BusinessCapabilityMapConfigurerConfigurer(
    modelComponent: BusinessCapability,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureDescription[BusinessCapability]
    with CanConfigureLinks[BusinessCapability]
    with CanConfigureExternalIds[BusinessCapability]
    with CanConfigureSWOT[BusinessCapability]
    with CanConfigureArchitectureVerdict[BusinessCapability]
    with CanConfigureServingSource[BusinessCapability]
    with CanConfigureServingTarget[BusinessCapability]
    with CanConfigureRealizationTarget[BusinessCapability]
    with CanConfigureFlowSource[BusinessCapability]
    with CanConfigureFlowTarget[BusinessCapability]
    with CanConfigureTriggerSource[BusinessCapability]
    with CanConfigureTriggerTarget[BusinessCapability]
    with CanConfigureTagProperties[BusinessCapability] {
  def as(
      body: BusinessCapabilityMapConfigurerConfigurer => Any
  ): BusinessCapability = {
    body.apply(this)
    propertyAdder.townPlan
      .businessCapability(modelComponent.key)
      .get
  }
}

trait CanAddBusinessCapabilities
    extends CanAddProperties
    with CanAddRelationships {
  def describes(
      businessCapability: BusinessCapability
  ): BusinessCapabilityMapConfigurerConfigurer =
    BusinessCapabilityMapConfigurerConfigurer(
      has(businessCapability),
      this,
      this
    )
}
