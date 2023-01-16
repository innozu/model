package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

case class SystemContainerView(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    forSystem: Key,
    title: String = "System Container View",
    pointInTime: ADay = Today,
    withStepCounter: Boolean = true,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends View {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "System Container View",
    classOf[SystemContainerView]
  )
  val layer: Layer = ApplicationLayer

  def withProperty(property: Property): SystemContainerView =
    copy(properties = this.properties + (property.key -> property))
}

object SystemContainerView {
  def apply(
      forSystem: ItSystem,
      pointInTime: ADay
  ) = new SystemContainerView(
    forSystem = forSystem.key,
    pointInTime = pointInTime
  )

  def apply(
      forSystem: ItSystem
  ) = new SystemContainerView(forSystem = forSystem.key)
}

trait HasSystemContainerViews
    extends HasViews
    with HasItSystems
    with HasItContainers
    with HasRelationships
    with HasBusinessActors
    with HasTechnologies {
  def systemContainerViews: List[CompiledSystemContainerView] = components(
    classOf[SystemContainerView]
  ).map(view => SystemContainerViewCompiler(view, this).compile)
  def systemContainerView(key: Key): Option[CompiledSystemContainerView] =
    component(key, classOf[SystemContainerView]).map(
      SystemContainerViewCompiler(_, this).compile
    )
}

trait CanAddSystemContainerViews
    extends CanAddProperties
    with CanAddRelationships {
  def needs(systemContainerView: SystemContainerView): SystemContainerView =
    has(systemContainerView)
}

case class CompiledSystemContainerView(
    view: SystemContainerView,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[SystemContainerView]
    with HasItSystems
    with HasItContainers
    with HasRelationships
    with HasBusinessActors
    with HasTechnologies {
  def otherSystems: List[ItSystem] = systems.filterNot(centralSystem.toSet)

  def centralSystem: Option[ItSystem] = system(view.forSystem)

  def flows: List[Flow] = relationshipsWithType(classOf[Flow])
}

case class SystemContainerViewCompiler(
    view: SystemContainerView,
    source: HasSystemContainerViews
) extends ViewCompiler[
      SystemContainerView,
      CompiledSystemContainerView,
      HasSystemContainerViews
    ] {
  def compile: CompiledSystemContainerView =
    CompiledSystemContainerView(
      view,
      viewTitle,
      groupTitle(view.forSystem),
      viewComponents(
        systems ++ containers ++ actors ++ flows ++ technologies ++ implementingTechnologies
      )
    )

  private def flows: Set[Flow] =
    allFlows.filter(flow =>
      isInDiagram(flow.source) && isInDiagram(flow.target)
    )

  private def isInDiagram(key: Key): Boolean =
    visible(key) && (systems ++ containers ++ actors).exists(_.key == key)

  private def actors: Set[Actor] = allFlows
    .flatMap(relationship => Set(relationship.source, relationship.target))
    .map(key => source.businessActor(key))
    .filter(_.nonEmpty)
    .map(_.get)
    .filter(_.isInstanceOf[Actor])
    .map(_.asInstanceOf[Actor])

  private def containers: Set[ItContainer] =
    centralSystem.flatMap(source.containers)

  def technologies: Set[Technology] = containers.flatMap(source.technologies(_))

  private def systems: Set[ItSystem] =
    centralSystem ++ directlyDependentSystems ++ childDependentSystems

  private def centralSystem: Set[ItSystem] = source.system(view.forSystem).toSet

  private def directlyDependentSystems: Iterable[ItSystem] = {
    centralSystem
      .flatMap(source.directDependenciesOfType(_, classOf[ItSystem]))
      .filterNot(centralSystem)
  }

  private def childDependentSystems: Iterable[ItSystem] = {
    centralSystem
      .flatMap(source.containers)
      .flatMap(source.directDependenciesOfType(_, classOf[ItSystem]))
      .filterNot(centralSystem)
  }

  private def allFlows: Set[Flow] = flowSources
    .flatMap(element =>
      source.relationships(element, classOf[Flow]).map(_.asInstanceOf[Flow])
    )

  private def flowSources: Set[Element] = if (containers.nonEmpty)
    containers.map(container => container.asInstanceOf[Element])
  else centralSystem.toSet

  private def implementingTechnologies: Set[Implementation] = technologies
    .flatMap(it =>
      source.relationships(it, classOf[Implementation], classOf[ItContainer])
    )
    .filter(r =>
      source
        .relationshipParticipantsOfType(r, classOf[ItContainer])
        .forall(containers.contains(_))
    )
    .map(_.asInstanceOf[Implementation])

  private def compositions: Set[Composition] =
    centralSystem.flatMap(element =>
      source
        .relationships(element, classOf[Composition])
        .filter(r =>
          r.other(element.key).exists(o => containers.exists(_.key == o))
        )
        .map(_.asInstanceOf[Composition])
    )

}
