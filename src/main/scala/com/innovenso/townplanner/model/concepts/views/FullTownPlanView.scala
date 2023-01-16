package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

case class FullTownPlanView(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    forEnterprise: Key,
    includeContainers: Boolean = true,
    title: String = "Full Town Plan",
    pointInTime: ADay = Today,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends View {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Full Town Plan",
    classOf[FullTownPlanView]
  )
  val layer: Layer = ApplicationLayer

  def withProperty(
      property: Property
  ): FullTownPlanView =
    copy(properties = this.properties + (property.key -> property))
}

object FullTownPlanView {
  def apply(
      forEnterprise: Enterprise,
      pointInTime: ADay
  ) = new FullTownPlanView(
    forEnterprise = forEnterprise.key,
    pointInTime = pointInTime
  )

  def apply(
      forEnterprise: Enterprise
  ) = new FullTownPlanView(forEnterprise = forEnterprise.key)
}

trait HasFullTownPlanViews
    extends HasViews
    with HasBusinessCapabilities
    with HasArchitectureBuildingBlocks
    with HasItPlatforms
    with HasItSystems
    with HasItContainers
    with HasEnterprises
    with HasTechnologies {
  def fullTownPlanViews: List[CompiledFullTownPlanView] =
    components(
      classOf[FullTownPlanView]
    ).map(view => FullTownPlanViewCompiler(view, this).compile)
  def fullTownPlanView(
      key: Key
  ): Option[CompiledFullTownPlanView] =
    component(key, classOf[FullTownPlanView]).map(
      FullTownPlanViewCompiler(_, this).compile
    )

}

trait CanAddFullTownPlanViews
    extends CanAddProperties
    with CanAddRelationships {
  def needs(
      fullTownPlanView: FullTownPlanView
  ): FullTownPlanView =
    has(fullTownPlanView)
}

case class CompiledFullTownPlanView(
    view: FullTownPlanView,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[FullTownPlanView]
    with HasRelationships
    with HasBusinessCapabilities
    with HasEnterprises
    with HasArchitectureBuildingBlocks
    with HasItPlatforms
    with HasItSystems
    with HasItContainers
    with HasTechnologies {
  def enterprise: Option[Enterprise] = enterprises.headOption
}

case class FullTownPlanViewCompiler(
    view: FullTownPlanView,
    source: HasFullTownPlanViews
) extends ViewCompiler[
      FullTownPlanView,
      CompiledFullTownPlanView,
      HasFullTownPlanViews
    ] {
  def compile: CompiledFullTownPlanView =
    CompiledFullTownPlanView(
      view,
      viewTitle,
      groupTitle(view.forEnterprise),
      viewComponents(
        enterprises ++ capabilities ++ buildingBlocks ++ platforms ++ systems ++ containers ++ technologies ++ servingCapabilities ++ servingEnterprises ++ realizingBuildingBlocks ++ realizingPlatforms ++ realizingSystems ++ composingSystems ++ composingContainers ++ implementingTechnologies
      )
    )

  def enterprises: Set[Enterprise] = source.enterprise(view.forEnterprise).toSet

  def capabilities: Set[BusinessCapability] =
    enterprises.flatMap(source.businessCapabilityMap)

  def buildingBlocks: Set[ArchitectureBuildingBlock] =
    capabilities.flatMap(source.realizingArchitectureBuildingBlocks)

  def platforms: Set[ItPlatform] =
    buildingBlocks.flatMap(source.realizingPlatforms)

  def directlyRealizingSystems: Set[ItSystem] =
    buildingBlocks.flatMap(source.realizingSystems)

  def platformRealizingSystems: Set[ItSystem] =
    platforms.flatMap(source.platformSystems)

  def systems: Set[ItSystem] =
    directlyRealizingSystems ++ platformRealizingSystems

  def containers: Set[ItContainer] =
    if (view.includeContainers) systems.flatMap(source.containers) else Set()

  def technologies: Set[Technology] = containers.flatMap(source.technologies(_))

  def servingCapabilities: Set[Serving] = capabilities
    .flatMap(c =>
      source.relationships(c, classOf[Serving], classOf[BusinessCapability])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[BusinessCapability])
        .forall(capabilities.contains(_))
    )
    .map(_.asInstanceOf[Serving])

  def servingEnterprises: Set[Serving] = capabilities
    .flatMap(c =>
      source.relationships(c, classOf[Serving], classOf[Enterprise])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[Enterprise])
        .forall(enterprises.contains(_))
    )
    .map(_.asInstanceOf[Serving])

  def realizingBuildingBlocks: Set[Realization] = buildingBlocks
    .flatMap(bb =>
      source
        .relationships(bb, classOf[Realization], classOf[BusinessCapability])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[BusinessCapability])
        .forall(capabilities.contains(_))
    )
    .map(_.asInstanceOf[Realization])

  def realizingPlatforms: Set[Realization] = platforms
    .flatMap(bb =>
      source
        .relationships(
          bb,
          classOf[Realization],
          classOf[ArchitectureBuildingBlock]
        )
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ArchitectureBuildingBlock])
        .forall(buildingBlocks.contains(_))
    )
    .map(_.asInstanceOf[Realization])

  def realizingSystems: Set[Realization] = systems
    .flatMap(bb =>
      source
        .relationships(
          bb,
          classOf[Realization],
          classOf[ArchitectureBuildingBlock]
        )
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ArchitectureBuildingBlock])
        .forall(buildingBlocks.contains(_))
    )
    .map(_.asInstanceOf[Realization])

  def composingSystems: Set[Composition] = systems
    .flatMap(bb =>
      source
        .relationships(bb, classOf[Composition], classOf[ItPlatform])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ItPlatform])
        .forall(platforms.contains(_))
    )
    .map(_.asInstanceOf[Composition])

  def composingContainers: Set[Composition] = containers
    .flatMap(it =>
      source
        .relationships(it, classOf[Composition], classOf[ItSystem])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ItSystem])
        .forall(systems.contains(_))
    )
    .map(_.asInstanceOf[Composition])

  def implementingTechnologies: Set[Implementation] = technologies
    .flatMap(it =>
      source.relationships(it, classOf[Implementation], classOf[ItContainer])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ItContainer])
        .forall(containers.contains(_))
    )
    .map(_.asInstanceOf[Implementation])

}
