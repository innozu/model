package com.innovenso.townplanner.model.concepts.properties

import com.innovenso.townplanner.model.concepts.{DataObject, Technology}
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.meta.{Key, SortKey}

abstract class Interaction extends Property {
  val key: Key = Key("interation")
  val canBePlural: Boolean = true
  val sortKey: SortKey = SortKey.next
  def name: String
  def description: String
  def source: Key
  def target: Key
  def technology: List[String]
  def payload: List[String]

  def dataPayload: List[DataObject]

  def payloadLabel: Option[String] = Option(
    (payload ::: dataPayload.map(_.title)).mkString(", ")
  ).filter(!_.isBlank)
  def radarTechnologies: List[Technology]

  def technologyLabel: Option[String] = Option(
    (technology ::: radarTechnologies.map(_.title)).mkString(", ")
  ).filter(!_.isBlank)

  def withSource(newSource: Key): Interaction
  def withTarget(newTarget: Key): Interaction
  def withPayload(payload: String): Interaction
  def withTechnology(technology: String): Interaction

  def withDataPayload(payload: List[DataObject]): Interaction

  def withRadarTechnologies(technology: List[Technology]): Interaction

  def label(counter: Int = -1): String =
    s"${labelCounter(counter)}${name}${labelTechnologyAndPayload}"

  private def labelCounter(counter: Int): String =
    if (counter < 0) "" else s"${counter}. "
  private def labelTechnologyAndPayload: String = {
    val contents = technologyLabel.toList ::: payloadLabel.toList
    if (contents.nonEmpty) s" [${contents.mkString(", ")}]" else ""
  }
}

case class Message(
    name: String,
    description: String = "",
    source: Key = Key(),
    target: Key = Key(),
    technology: List[String] = Nil,
    payload: List[String] = Nil,
    radarTechnologies: List[Technology] = Nil,
    dataPayload: List[DataObject] = Nil
) extends Interaction {
  override def withSource(newSource: Key): Interaction =
    copy(source = newSource)

  override def withTarget(newTarget: Key): Interaction =
    copy(target = newTarget)

  override def withPayload(newPayload: String): Interaction =
    copy(payload = newPayload :: payload)

  override def withTechnology(newTechnology: String): Interaction =
    copy(technology = newTechnology :: technology)

  override def withDataPayload(payload: List[DataObject]): Interaction =
    copy(dataPayload = payload)

  override def withRadarTechnologies(
      technology: List[Technology]
  ): Interaction =
    copy(radarTechnologies = technology)
}

case class Request(
    name: String,
    description: String = "",
    source: Key = Key(),
    target: Key = Key(),
    technology: List[String] = Nil,
    payload: List[String] = Nil,
    radarTechnologies: List[Technology] = Nil,
    dataPayload: List[DataObject] = Nil
) extends Interaction {
  override def withSource(newSource: Key): Interaction =
    copy(source = newSource)

  override def withTarget(newTarget: Key): Interaction =
    copy(target = newTarget)

  override def withPayload(newPayload: String): Interaction =
    copy(payload = newPayload :: payload)

  override def withTechnology(newTechnology: String): Interaction =
    copy(technology = newTechnology :: technology)

  override def withDataPayload(payload: List[DataObject]): Interaction =
    copy(dataPayload = payload)

  override def withRadarTechnologies(
      technology: List[Technology]
  ): Interaction =
    copy(radarTechnologies = technology)

}

case class Response(
    name: String,
    description: String = "",
    source: Key = Key(),
    target: Key = Key(),
    technology: List[String] = Nil,
    payload: List[String] = Nil,
    radarTechnologies: List[Technology] = Nil,
    dataPayload: List[DataObject] = Nil
) extends Interaction {
  override def withSource(newSource: Key): Interaction =
    copy(source = newSource)

  override def withTarget(newTarget: Key): Interaction =
    copy(target = newTarget)

  override def withPayload(newPayload: String): Interaction =
    copy(payload = newPayload :: payload)

  override def withTechnology(newTechnology: String): Interaction =
    copy(technology = newTechnology :: technology)

  override def withDataPayload(payload: List[DataObject]): Interaction =
    copy(dataPayload = payload)

  override def withRadarTechnologies(
      technology: List[Technology]
  ): Interaction =
    copy(radarTechnologies = technology)

}

trait HasInteractions extends HasProperties {
  def interactions: List[Interaction] = props(classOf[Interaction])
  def interaction(key: Key): Option[Interaction] =
    prop(key, classOf[Interaction])
  def withInteraction(interaction: Interaction): HasProperties =
    withProperty(interaction)
}

case class InteractionConfigurer(
    modelComponent: HasInteractions,
    interaction: Interaction,
    propertyAdder: CanAddProperties
) {
  def containing(payload: String): InteractionConfigurer =
    copy(interaction = interaction.withPayload(payload))

  def containing(dataObject: DataObject): InteractionConfigurer =
    copy(interaction =
      interaction.withDataPayload(dataObject :: interaction.dataPayload)
    )

  def using(technology: String): InteractionConfigurer =
    copy(interaction = interaction.withTechnology(technology))

  def using(technology: Technology): InteractionConfigurer =
    copy(interaction =
      interaction.withRadarTechnologies(
        technology :: interaction.radarTechnologies
      )
    )

  def from(source: Element): InteractionConfigurer =
    copy(interaction = interaction.withSource(source.key))
  def to(target: Element): HasInteractions =
    propertyAdder.withProperty(
      modelComponent,
      interaction.withTarget(target.key)
    )
}

trait CanConfigureInteractions[ModelComponentType <: HasInteractions] {
  def propertyAdder: CanAddProperties
  def modelComponent: ModelComponentType

  def has(interaction: Interaction): InteractionConfigurer =
    InteractionConfigurer(modelComponent, interaction, propertyAdder)
}
