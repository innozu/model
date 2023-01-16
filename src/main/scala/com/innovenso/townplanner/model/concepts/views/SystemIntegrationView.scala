package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.concepts.{
  HasItSystemIntegrations,
  HasItSystems,
  ItSystem,
  ItSystemIntegration
}
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

case class SystemIntegrationView(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    forSystemIntegration: Key,
    title: String = "System Integration View",
    pointInTime: ADay = Today,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends View {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "System Integration View",
    classOf[SystemIntegrationView]
  )
  val layer: Layer = ApplicationLayer

  def withProperty(property: Property): SystemIntegrationView =
    copy(properties = this.properties + (property.key -> property))
}

object SystemIntegrationView {
  def apply(
      forSystemIntegration: ItSystemIntegration,
      pointInTime: ADay
  ) = new SystemIntegrationView(
    forSystemIntegration = forSystemIntegration.key,
    pointInTime = pointInTime
  )

  def apply(
      forSystemIntegration: ItSystemIntegration
  ) = new SystemIntegrationView(forSystemIntegration = forSystemIntegration.key)
}

trait HasSystemIntegrationViews
    extends HasViews
    with HasItSystems
    with HasItSystemIntegrations
    with HasRelationships {
  def systemIntegrationViews: List[CompiledSystemIntegrationView] = components(
    classOf[SystemIntegrationView]
  ).map(view => SystemIntegrationViewCompiler(view, this).compile)
  def systemIntegrationView(key: Key): Option[CompiledSystemIntegrationView] =
    component(key, classOf[SystemIntegrationView]).map(
      SystemIntegrationViewCompiler(_, this).compile
    )
}

trait CanAddSystemIntegrationViews
    extends CanAddProperties
    with CanAddRelationships {
  def needs(
      systemIntegrationView: SystemIntegrationView
  ): SystemIntegrationView =
    has(systemIntegrationView)
}

case class CompiledSystemIntegrationView(
    view: SystemIntegrationView,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[SystemIntegrationView]
    with HasItSystems
    with HasItSystemIntegrations
    with HasRelationships {
  def source: Option[ItSystem] = integration.map(_.source).flatMap(system)

  def target: Option[ItSystem] = integration.map(_.target).flatMap(system)

  def integration: Option[ItSystemIntegration] = systemIntegration(
    view.forSystemIntegration
  )

  def implementingSystems: List[ItSystem] = integration
    .map(i =>
      directIncomingDependencies(i, classOf[Implementation], classOf[ItSystem])
    )
    .getOrElse(Nil)

  def implementationRelationships: List[Relationship] = integration
    .map(i => relationships(i, classOf[Implementation], classOf[ItSystem]))
    .getOrElse(Nil)
}

case class SystemIntegrationViewCompiler(
    view: SystemIntegrationView,
    source: HasSystemIntegrationViews
) extends ViewCompiler[
      SystemIntegrationView,
      CompiledSystemIntegrationView,
      HasSystemIntegrationViews
    ] {
  def compile: CompiledSystemIntegrationView =
    CompiledSystemIntegrationView(
      view,
      viewTitle,
      groupTitle(view.forSystemIntegration),
      viewComponents(
        integration.toSet ++ systems ++ implementers ++ relationships
      )
    )

  def systems: Set[ItSystem] = integration
    .map(it => source.system(it.source).toSet ++ source.system(it.target).toSet)
    .getOrElse(Set())

  def relationships: Set[Relationship] =
    implementationRelationships ++ associationRelationships

  def implementationRelationships: Set[Relationship] = integration
    .map(it =>
      source.relationships(it, classOf[Implementation], classOf[ItSystem])
    )
    .getOrElse(Set())
    .toSet

  def associationRelationships: Set[Relationship] = integration
    .map(it =>
      Set(
        Association(source = it.source, target = it.key),
        Association(source = it.target, target = it.key)
      )
    )
    .getOrElse(Nil)
    .toSet

  def integration: Option[ItSystemIntegration] =
    source.systemIntegration(view.forSystemIntegration)

  def implementers: Set[ItSystem] = integration
    .map(it =>
      source
        .relationships(it, classOf[Implementation], classOf[ItSystem])
        .map(r => source.system(r.other(it.key).get))
        .filter(_.nonEmpty)
        .map(_.get)
        .toSet
    )
    .getOrElse(Set())

}
