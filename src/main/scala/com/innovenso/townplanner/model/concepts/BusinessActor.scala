package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.language.HasModelComponents
import com.innovenso.townplanner.model.meta._

sealed trait BusinessActor
    extends Element
    with HasDescription
    with HasLinks
    with HasExternalIds
    with HasSWOT
    with CanBeFlowSource
    with CanBeFlowTarget
    with CanTrigger
    with CanDeliver
    with CanInfluence
    with CanServe
    with CanBeAssociated
    with CanBeStakeholder
    with CanBeRaci
    with CanCompose
    with CanBeComposedOf
    with CanKnow {
  val layer: Layer = BusinessLayer
  val aspect: Aspect = ActiveStructure
}

case class Actor(
    key: Key = Key("actor"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends BusinessActor {
  def withProperty(property: Property): Actor =
    copy(properties = this.properties + (property.key -> property))
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Business Actor",
    classOf[Actor]
  )
}

case class Person(
    key: Key = Key("person"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends BusinessActor {
  def withProperty(property: Property): Person =
    copy(properties = this.properties + (property.key -> property))
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Person",
    classOf[Person]
  )
}

case class Organisation(
    key: Key = Key("organisation"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends BusinessActor {
  def withProperty(property: Property): Organisation =
    copy(properties = this.properties + (property.key -> property))
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Organisation",
    classOf[Organisation]
  )
}

case class Team(
    key: Key = Key("team"),
    sortKey: SortKey = SortKey.next,
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends BusinessActor {
  def withProperty(property: Property): Team =
    copy(properties = this.properties + (property.key -> property))
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Team",
    classOf[Team]
  )
}

trait HasBusinessActors
    extends HasModelComponents
    with HasEnterprises
    with HasRelationships {
  def actorNouns: List[BusinessActor] =
    businessActors.filter(a => a.isInstanceOf[Actor])
  def individualActors: List[BusinessActor] =
    businessActors.filter(a => a.isInstanceOf[Person])
  def organisationActors: List[BusinessActor] =
    businessActors.filter(a => a.isInstanceOf[Organisation])
  def teamActors: List[BusinessActor] =
    businessActors.filter(a => a.isInstanceOf[Team])

  def teams(forPerson: Person): List[Team] =
    directIncomingDependencies(forPerson, classOf[Composition], classOf[Team])
  def teamsDelivering(canBeDelivered: CanBeDelivered): List[Team] =
    directIncomingDependencies(canBeDelivered, classOf[Delivery], classOf[Team])
  def teamMembers(ofTeam: Key): List[Person] =
    businessActor(ofTeam)
      .map(team =>
        directOutgoingDependencies(
          team,
          classOf[Composition],
          classOf[Person]
        )
      )
      .getOrElse(Nil)

  def businessActors: List[BusinessActor] = components(
    classOf[BusinessActor]
  )

  def businessActor(key: Key): Option[BusinessActor] =
    component(key, classOf[BusinessActor])

  def enterprise(businessActor: BusinessActor): Option[Enterprise] =
    directOutgoingDependencies(
      businessActor,
      classOf[Serving],
      classOf[Enterprise]
    ).headOption
}

case class BusinessActorConfigurer[BusinessActorType <: BusinessActor](
    modelComponent: BusinessActorType,
    propertyAdder: CanAddProperties,
    relationshipAdder: CanAddRelationships
) extends CanConfigureTitle[BusinessActorType]
    with CanConfigureDescription[BusinessActorType]
    with CanConfigureLinks[BusinessActorType]
    with CanConfigureExternalIds[BusinessActorType]
    with CanConfigureSWOT[BusinessActorType]
    with CanConfigureServingSource[BusinessActorType]
    with CanConfigureFlowSource[BusinessActorType]
    with CanConfigureFlowTarget[BusinessActorType]
    with CanConfigureTriggerSource[BusinessActorType]
    with CanConfigureAssociations[BusinessActorType]
    with CanConfigureDeliverySource[BusinessActorType]
    with CanConfigureRaciSource[BusinessActorType]
    with CanConfigureInfluenceSource[BusinessActorType]
    with CanConfigureStakeholderSource[BusinessActorType]
    with CanConfigureCompositionSource[BusinessActorType]
    with CanConfigureCompositionTarget[BusinessActorType]
    with CanConfigureKnowledgeSource[BusinessActorType] {
  def as(
      body: BusinessActorConfigurer[BusinessActorType] => Any
  ): BusinessActorType = {
    body.apply(this)
    propertyAdder.townPlan
      .component(modelComponent.key, modelComponent.getClass)
      .get
  }
}

trait CanAddBusinessActors extends CanAddProperties with CanAddRelationships {
  def describes(
      businessActor: Person
  ): BusinessActorConfigurer[Person] =
    BusinessActorConfigurer(
      has(businessActor),
      this,
      this
    )
  def describes(
      businessActor: Actor
  ): BusinessActorConfigurer[Actor] =
    BusinessActorConfigurer(
      has(businessActor),
      this,
      this
    )
  def describes(
      businessActor: Team
  ): BusinessActorConfigurer[Team] =
    BusinessActorConfigurer(
      has(businessActor),
      this,
      this
    )
  def describes(
      businessActor: Organisation
  ): BusinessActorConfigurer[Organisation] =
    BusinessActorConfigurer(
      has(businessActor),
      this,
      this
    )

  def hasRandomActor[ActorType <: BusinessActor](
      businessActor: ActorType
  ): ActorType = describesRandomActor(businessActor) as { _ => }

  def describesRandomActor[ActorType <: BusinessActor](
      businessActor: ActorType
  ): BusinessActorConfigurer[ActorType] = {
    val configurer = BusinessActorConfigurer(
      has(businessActor),
      this,
      this
    )
    val body = { (it: BusinessActorConfigurer[ActorType]) =>
      it has Title.random
      Description.randoms.foreach(it.has)
      Link.randoms.foreach(it.has)
      ExternalId.randoms.foreach(r =>
        it isIdentifiedAs (r.id) on r.externalSystemName
      )
      SWOT.randoms.foreach(it.has)
    }
    body.apply(configurer)
    configurer
  }

}
