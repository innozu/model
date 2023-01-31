package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.{Element, HasModelComponents}
import com.innovenso.townplanner.model.meta._

trait DataObject
    extends Element
    with HasDescription
    with HasDataAttributes
    with HasDataClassification
    with HasLinks
    with HasExternalIds
    with HasFatherTime
    with CanBeImpacted
    with CanBeKnown
    with CanHaveDataRelationship
    with CanBeAccessed
    with CanBeConsumed
    with CanBeOwned
    with CanBeProduced
    with CanBeProcessed
    with CanBeTransported {

  val layer: Layer = ApplicationLayer
  val aspect: Aspect = PassiveStructure
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Data Object",
    classOf[DataObject]
  )

  def dataObjectType: String
}

case class Entity(
    key: Key = Key("entity"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "entity"
  def withProperty(property: Property): Entity =
    copy(properties = this.properties + (property.key -> property))
}

case class ValueObject(
    key: Key = Key("value object"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "value object"

  def withProperty(property: Property): ValueObject =
    copy(properties = this.properties + (property.key -> property))
}

case class AggregateRoot(
    key: Key = Key("aggregate root"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "aggregate root"

  def withProperty(property: Property): AggregateRoot =
    copy(properties = this.properties + (property.key -> property))
}

case class Command(
    key: Key = Key("command"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "command"

  def withProperty(property: Property): Command =
    copy(properties = this.properties + (property.key -> property))
}

case class Event(
    key: Key = Key("event"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "event"

  def withProperty(property: Property): Event =
    copy(properties = this.properties + (property.key -> property))
}

case class Query(
    key: Key = Key("query"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "query"

  def withProperty(property: Property): Query =
    copy(properties = this.properties + (property.key -> property))
}

case class Projection(
    key: Key = Key("projection"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends DataObject {
  val dataObjectType: String = "projection"

  def withProperty(property: Property): Projection =
    copy(properties = this.properties + (property.key -> property))
}

object DataObject {
  def fromString(
      dataObjectType: String,
      key: Key = Key("dataobject"),
      sortKey: SortKey = SortKey.next
  ): Option[DataObject] =
    Option(dataObjectType).map(_.toLowerCase).map(_.trim).map {
      case "projection"     => Projection(key, sortKey)
      case "query"          => Query(key, sortKey)
      case "event"          => Event(key, sortKey)
      case "command"        => Command(key, sortKey)
      case "aggregate root" => AggregateRoot(key, sortKey)
      case "value object"   => ValueObject(key, sortKey)
      case "entity"         => Entity(key, sortKey)
    }
}

trait HasDataObjects extends HasModelComponents with HasRelationships {
  def dataObjects: List[DataObject] = components(classOf[DataObject])
  def dataObject(key: Key): Option[DataObject] =
    component(key, classOf[DataObject])
}

case class DataObjectConfigurer[DataObjectType <: DataObject](
    modelComponent: DataObjectType,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[DataObjectType]
    with CanConfigureDescription[DataObjectType]
    with CanConfigureLinks[DataObjectType]
    with CanConfigureExternalIds[DataObjectType]
    with CanConfigureFatherTime[DataObjectType]
    with CanConfigureKnowledgeTarget[DataObjectType]
    with CanConfigureDataAttributes[DataObjectType]
    with CanConfigureDataRelationships[DataObjectType]
    with CanConfigureAccessingTarget[DataObjectType]
    with CanConfigureConsumingTarget[DataObjectType]
    with CanConfigureOwningTarget[DataObjectType]
    with CanConfigureProcessingTarget[DataObjectType]
    with CanConfigureProducingTarget[DataObjectType]
    with CanConfigureTransportingTarget[DataObjectType]
    with CanConfigureDataClassification[DataObjectType] {
  def as(
      body: DataObjectConfigurer[DataObjectType] => Any
  ): DataObjectType = {
    body.apply(this)
    propertyAdder.townPlan
      .component(modelComponent.key, modelComponent.getClass)
      .get
  }
}

trait CanAddDataObjects extends CanAddProperties with CanAddRelationships {

  def describes(
      dataObject: Entity
  ): DataObjectConfigurer[Entity] =
    describesDataObject[Entity](dataObject)

  def describes(
      dataObject: ValueObject
  ): DataObjectConfigurer[ValueObject] =
    describesDataObject[ValueObject](dataObject)

  def describes(
      dataObject: AggregateRoot
  ): DataObjectConfigurer[AggregateRoot] =
    describesDataObject[AggregateRoot](dataObject)

  def describes(
      dataObject: Event
  ): DataObjectConfigurer[Event] =
    describesDataObject[Event](dataObject)

  def describes(
      dataObject: Command
  ): DataObjectConfigurer[Command] =
    describesDataObject[Command](dataObject)

  def describes(
      dataObject: Query
  ): DataObjectConfigurer[Query] =
    describesDataObject[Query](dataObject)

  def describes(
      dataObject: Projection
  ): DataObjectConfigurer[Projection] =
    describesDataObject[Projection](dataObject)

  private def describesDataObject[DataObjectType <: DataObject](
      dataObject: DataObjectType
  ): DataObjectConfigurer[DataObjectType] =
    DataObjectConfigurer(has(dataObject), this, this)

  def hasRandomDataObject[DataObjectType <: DataObject](
      dataObject: DataObjectType
  ): DataObjectType = describesRandomDataObject(dataObject) as { _ => }
  def describesRandomDataObject[DataObjectType <: DataObject](
      dataObject: DataObjectType
  ): DataObjectConfigurer[DataObjectType] = {
    val configurer = DataObjectConfigurer(has(dataObject), this, this)
    val body = { it: DataObjectConfigurer[DataObjectType] =>
      it has Title.random
      Description.randoms.foreach(it.has)
      Link.randoms.foreach(it.has)
      ExternalId.randoms.foreach(r =>
        it isIdentifiedAs (r.id) on r.externalSystemName
      )
      DataAttribute.randoms.foreach(it.has)
      it has DataClassification.random
      FatherTime.randoms.foreach(ft => it is ft on ft.date)
    }
    body.apply(configurer)
    configurer
  }

}

trait CanInteractWithDataObjects
    extends CanAccess
    with CanConsume
    with CanOwn
    with CanProcess
    with CanProduce
    with CanTransport

trait CanConfigureDataObjectInteractionSource[
    ModelComponentType <: CanInteractWithDataObjects
] extends CanConfigureTransportingSource[ModelComponentType]
    with CanConfigureAccessingSource[ModelComponentType]
    with CanConfigureConsumingSource[ModelComponentType]
    with CanConfigureOwningSource[ModelComponentType]
    with CanConfigureProcessingSource[ModelComponentType]
    with CanConfigureProducingSource[ModelComponentType]
