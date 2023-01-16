package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties.{
  CanAddProperties,
  Property
}
import com.innovenso.townplanner.model.concepts.relationships.{
  CanAddRelationships,
  DataRelationship,
  HasRelationships
}
import com.innovenso.townplanner.model.concepts.{DataObject, HasDataObjects}
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

import scala.annotation.tailrec

case class DataModelView(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    forDataObjects: List[Key],
    title: String = "Data Model View",
    pointInTime: ADay = Today,
    withStepCounter: Boolean = true,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends View {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Data Model View",
    classOf[DataModelView]
  )
  val layer: Layer = ApplicationLayer

  def withProperty(property: Property): DataModelView =
    copy(properties = this.properties + (property.key -> property))

}

object DataModelView {
  def apply(
      forDataObjects: List[DataObject],
      pointInTime: ADay
  ) = new DataModelView(
    forDataObjects = forDataObjects.map(_.key),
    pointInTime = pointInTime
  )

  def apply(
      forDataObjects: List[DataObject]
  ) = new DataModelView(forDataObjects = forDataObjects.map(_.key))
}

trait HasDataModelViews
    extends HasViews
    with HasDataObjects
    with HasRelationships {
  def dataModelViews: List[CompiledDataModelView] = components(
    classOf[DataModelView]
  ).map(view => DataModelViewCompiler(view, this).compile)
  def dataModelView(key: Key): Option[CompiledDataModelView] =
    component(key, classOf[DataModelView]).map(
      DataModelViewCompiler(_, this).compile
    )
}

trait CanAddDataModelViews extends CanAddProperties with CanAddRelationships {
  def needs(dataModelView: DataModelView): DataModelView =
    has(dataModelView)
}

case class CompiledDataModelView(
    view: DataModelView,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[DataModelView]
    with HasDataObjects
    with HasRelationships {}

case class DataModelViewCompiler(
    view: DataModelView,
    source: HasDataModelViews
) extends ViewCompiler[
      DataModelView,
      CompiledDataModelView,
      HasDataModelViews
    ] {
  override def compile: CompiledDataModelView = CompiledDataModelView(
    view,
    viewTitle,
    "Data Model",
    viewComponents(allDataObjects ::: allRelationships)
  )

  private val coreDataObjects: List[DataObject] =
    view.forDataObjects.flatMap(source.dataObject)

  private def dataRelationships(
      dataObjects: List[DataObject]
  ): List[DataRelationship] = dataObjects
    .flatMap(source.relationships(_, classOf[DataRelationship]))
    .map(_.asInstanceOf[DataRelationship])

  @tailrec
  private def dependencyTree(
      dataObjects: List[DataObject]
  ): List[DataObject] = {
    val directDependencies = dataObjects
      .flatMap(source.directDependenciesOfType(_, classOf[DataObject]))
      .filterNot(dataObjects.toSet)
    if (directDependencies.isEmpty) dataObjects
    else dependencyTree(dataObjects ::: directDependencies)
  }

  private val allDataObjects: List[DataObject] = dependencyTree(coreDataObjects)
  private val allRelationships: List[DataRelationship] = dataRelationships(
    allDataObjects
  )

}
