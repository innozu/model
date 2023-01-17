package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._

sealed trait ItContainer
    extends Element
    with HasDescription
    with HasAPI
    with HasArchitectureVerdict
    with HasCriticality
    with HasLinks
    with HasExternalIds
    with HasSWOT
    with HasFatherTime
    with HasResilienceMeasures
    with CanBeFlowSource
    with CanBeFlowTarget
    with CanTrigger
    with CanBeTriggered
    with CanBeAssociated
    with CanCompose
    with CanBeImplemented
    with CanBeDelivered
    with CanBeImpacted
    with CanBeImplementedByTechnologies
    with CanBeKnown
    with CanInteractWithDataObjects {
  val layer: Layer = ApplicationLayer
  val aspect: Aspect = ActiveStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "IT Container",
    classOf[ItContainer]
  )
  def containerType: String
  def containerLayer: ItContainerLayer
}

case class Microservice(
    key: Key = Key("microservice"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "microservice"
  val containerLayer: ItContainerLayer = ServiceLayer
  def withProperty(property: Property): Microservice =
    copy(properties = this.properties + (property.key -> property))
}

case class Database(
    key: Key = Key("database"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "database"
  val containerLayer: ItContainerLayer = StorageLayer
  def withProperty(property: Property): Database =
    copy(properties = this.properties + (property.key -> property))
}

case class Service(
    key: Key = Key("service"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "service"
  val containerLayer: ItContainerLayer = ServiceLayer
  def withProperty(property: Property): Service =
    copy(properties = this.properties + (property.key -> property))
}

case class Function(
    key: Key = Key("function"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "function"
  val containerLayer: ItContainerLayer = ServiceLayer
  def withProperty(property: Property): Function =
    copy(properties = this.properties + (property.key -> property))
}

case class Filesystem(
    key: Key = Key("filesystem"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "file system"
  val containerLayer: ItContainerLayer = StorageLayer
  def withProperty(property: Property): Filesystem =
    copy(properties = this.properties + (property.key -> property))
}

case class Queue(
    key: Key = Key("queue"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "queue"
  val containerLayer: ItContainerLayer = StreamLayer
  def withProperty(property: Property): Queue =
    copy(properties = this.properties + (property.key -> property))
}

case class Topic(
    key: Key = Key("topic"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "topic"
  val containerLayer: ItContainerLayer = StreamLayer
  def withProperty(property: Property): Topic =
    copy(properties = this.properties + (property.key -> property))
}

case class EventStream(
    key: Key = Key("event stream"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "event stream"
  val containerLayer: ItContainerLayer = StreamLayer
  def withProperty(property: Property): EventStream =
    copy(properties = this.properties + (property.key -> property))
}

case class Gateway(
    key: Key = Key("gateway"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "gateway"
  val containerLayer: ItContainerLayer = InterfaceLayer
  def withProperty(property: Property): Gateway =
    copy(properties = this.properties + (property.key -> property))
}

case class Proxy(
    key: Key = Key("proxy"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "proxy"
  val containerLayer: ItContainerLayer = InterfaceLayer
  def withProperty(property: Property): Proxy =
    copy(properties = this.properties + (property.key -> property))
}

case class Firewall(
    key: Key = Key("firewall"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "firewall"
  val containerLayer: ItContainerLayer = InterfaceLayer
  def withProperty(property: Property): Firewall =
    copy(properties = this.properties + (property.key -> property))
}

case class Cache(
    key: Key = Key("cache"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "cache"
  val containerLayer: ItContainerLayer = InterfaceLayer
  def withProperty(property: Property): Cache =
    copy(properties = this.properties + (property.key -> property))
}

case class WebUI(
    key: Key = Key("web UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "web UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): WebUI =
    copy(properties = this.properties + (property.key -> property))
}

case class MobileUI(
    key: Key = Key("mobile UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "mobile UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): MobileUI =
    copy(properties = this.properties + (property.key -> property))
}

case class WatchUI(
    key: Key = Key("watch UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "watch UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): WatchUI =
    copy(properties = this.properties + (property.key -> property))
}

case class DesktopUI(
    key: Key = Key("desktop UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "desktop UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): DesktopUI =
    copy(properties = this.properties + (property.key -> property))
}

case class TerminalUI(
    key: Key = Key("terminal UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "terminal UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): TerminalUI =
    copy(properties = this.properties + (property.key -> property))
}

case class SmartTVUI(
    key: Key = Key("smart TV UI"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "smart TV UI"
  val containerLayer: ItContainerLayer = UILayer
  def withProperty(property: Property): SmartTVUI =
    copy(properties = this.properties + (property.key -> property))
}

case class Batch(
    key: Key = Key("batch"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  val containerType: String = "batch"
  val containerLayer: ItContainerLayer = ServiceLayer
  def withProperty(property: Property): Batch =
    copy(properties = this.properties + (property.key -> property))
}

case class GenericContainer(
    key: Key = Key("container"),
    sortKey: SortKey = SortKey.next,
    containerType: String = "container",
    containerLayer: ItContainerLayer = ServiceLayer,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends ItContainer {
  def withProperty(property: Property): GenericContainer =
    copy(properties = this.properties + (property.key -> property))
}

trait HasItContainers extends HasModelComponents with HasRelationships {
  def containers: List[ItContainer] = components(classOf[ItContainer])
  def container(key: Key): Option[ItContainer] =
    component(key, classOf[ItContainer])
  def containers(itSystem: ItSystem): List[ItContainer] =
    directOutgoingDependencies(
      itSystem,
      classOf[Composition],
      classOf[ItContainer]
    )
  def system(itContainer: ItContainer): Option[ItSystem] =
    directIncomingDependencies(
      itContainer,
      classOf[Composition],
      classOf[ItSystem]
    ).headOption
  def layers(containers: Iterable[ItContainer]): List[ItContainerLayer] =
    containers.map(_.containerLayer).toList.sortBy(_.order).distinct
}

case class ItContainerConfigurer[ContainerType <: ItContainer](
    modelComponent: ContainerType,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[ContainerType]
    with CanConfigureDescription[ContainerType]
    with CanConfigureArchitectureVerdict[ContainerType]
    with CanConfigureLinks[ContainerType]
    with CanConfigureExternalIds[ContainerType]
    with CanConfigureSWOT[ContainerType]
    with CanConfigureFatherTime[ContainerType]
    with CanConfigureCriticality[ContainerType]
    with CanConfigureResilienceMeasures[ContainerType]
    with CanConfigureImplementationTarget[ContainerType]
    with CanConfigureTriggerSource[ContainerType]
    with CanConfigureTriggerTarget[ContainerType]
    with CanConfigureFlowSource[ContainerType]
    with CanConfigureFlowTarget[ContainerType]
    with CanConfigureAssociations[ContainerType]
    with CanConfigureCompositionTarget[ContainerType]
    with CanConfigureDeliveryTarget[ContainerType]
    with CanConfigureKnowledgeTarget[ContainerType]
    with CanConfigureDataObjectInteractionSource[ContainerType]
    with CanConfigureAPI[ContainerType] {
  def as(
      body: ItContainerConfigurer[ContainerType] => Any
  ): ContainerType = {
    body.apply(this)
    propertyAdder.townPlan
      .component(modelComponent.key, modelComponent.getClass)
      .get
  }
}

trait CanAddItContainers extends CanAddProperties with CanAddRelationships {
  def describes(
      container: GenericContainer
  ): ItContainerConfigurer[GenericContainer] =
    describesContainer[GenericContainer](container)

  def describes(
      container: Microservice
  ): ItContainerConfigurer[Microservice] =
    describesContainer[Microservice](container)

  def describes(
      container: Database
  ): ItContainerConfigurer[Database] =
    describesContainer[Database](container)

  def describes(
      container: Queue
  ): ItContainerConfigurer[Queue] =
    describesContainer[Queue](container)

  def describes(
      container: Topic
  ): ItContainerConfigurer[Topic] =
    describesContainer[Topic](container)

  def describes(
      container: EventStream
  ): ItContainerConfigurer[EventStream] =
    describesContainer[EventStream](container)

  def describes(
      container: Filesystem
  ): ItContainerConfigurer[Filesystem] =
    describesContainer[Filesystem](container)

  def describes(
      container: Service
  ): ItContainerConfigurer[Service] =
    describesContainer[Service](container)

  def describes(
      container: Function
  ): ItContainerConfigurer[Function] =
    describesContainer[Function](container)

  def describes(
      container: Gateway
  ): ItContainerConfigurer[Gateway] =
    describesContainer[Gateway](container)

  def describes(
      container: Proxy
  ): ItContainerConfigurer[Proxy] =
    describesContainer[Proxy](container)

  def describes(
      container: Firewall
  ): ItContainerConfigurer[Firewall] =
    describesContainer[Firewall](container)

  def describes(
      container: WebUI
  ): ItContainerConfigurer[WebUI] =
    describesContainer[WebUI](container)

  def describes(
      container: MobileUI
  ): ItContainerConfigurer[MobileUI] =
    describesContainer[MobileUI](container)

  def describes(
      container: WatchUI
  ): ItContainerConfigurer[WatchUI] =
    describesContainer[WatchUI](container)

  def describes(
      container: DesktopUI
  ): ItContainerConfigurer[DesktopUI] =
    describesContainer[DesktopUI](container)

  def describes(
      container: SmartTVUI
  ): ItContainerConfigurer[SmartTVUI] =
    describesContainer[SmartTVUI](container)

  def describes(
      container: TerminalUI
  ): ItContainerConfigurer[TerminalUI] =
    describesContainer[TerminalUI](container)

  def describes(
      container: Batch
  ): ItContainerConfigurer[Batch] =
    describesContainer[Batch](container)

  def describes(
      container: Cache
  ): ItContainerConfigurer[Cache] =
    describesContainer[Cache](container)

  private def describesContainer[ContainerType <: ItContainer](
      container: ContainerType
  ): ItContainerConfigurer[ContainerType] =
    ItContainerConfigurer(has(container), this, this)
}

sealed trait ItContainerLayer {
  def title: String
  def order: Int
}

case object UILayer extends ItContainerLayer {
  override val title: String = "UI"
  override val order: Int = 0
}

case object InterfaceLayer extends ItContainerLayer {
  override val title: String = "Interface"
  override val order: Int = 1
}

case object ServiceLayer extends ItContainerLayer {
  override val title: String = "Services"
  override val order: Int = 2
}

case object StorageLayer extends ItContainerLayer {
  override val title: String = "Storage"
  override val order: Int = 3
}

case object StreamLayer extends ItContainerLayer {
  override val title: String = "Stream"
  override val order: Int = 4
}
