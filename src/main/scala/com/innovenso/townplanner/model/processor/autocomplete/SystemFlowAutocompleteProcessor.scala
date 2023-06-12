package com.innovenso.townplanner.model.processor.autocomplete

import com.innovenso.townplanner.model.EnterpriseArchitecture
import com.innovenso.townplanner.model.concepts.ItContainer
import com.innovenso.townplanner.model.concepts.ItSystem
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.concepts.relationships.Flow
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.processor.TownPlanProcessor
import fish.genius.logging.Loggable

case class SystemFlowAutocompleteProcessor()(implicit
    ea: EnterpriseArchitecture
) extends TownPlanProcessor
    with Loggable {
  override def process(): Unit = {
    flowsToBeCreated.foreach(sf => {
      val sourceElement: Option[Element] =
        ea.townPlan.component(sf._1, classOf[Element])
      val targetElement: Option[Element] =
        ea.townPlan.component(sf._2, classOf[Element])
      val title: String = sf._3

      if (sourceElement.isDefined && targetElement.isDefined) {
        debug(
          s"autocompleting relationship: ${sourceElement
              .map(_.title)
              .getOrElse("UNKNOWN")} -- ${title} --> ${targetElement.map(_.title).getOrElse("UNKNOWN")}"
        )
        ea hasRelationship Flow(source = sf._1, target = sf._2)
          .withTitle(Title(sf._3))
          .asInstanceOf[Flow]
      } else {
        debug(
          s"not autocompleting relationship: source ${sourceElement} or target ${targetElement} does not exist"
        )
      }
    })
  }

  val containerFlows: List[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .map(_.asInstanceOf[Flow])
    .filter(
      ea.townPlan
        .relationshipParticipants(_)
        .forall(_.isInstanceOf[ItContainer])
    )

  val projectedSystemToSystemFlows: List[ProjectedSystemToSystemFlow] =
    containerFlows.map(flow => ProjectedSystemToSystemFlow(flow))

  val projectedContainerToSystemFlows: List[ProjectedContainerToSystemFlow] =
    containerFlows.map(flow => ProjectedContainerToSystemFlow(flow))

  val projectedSystemToContainerFlows: List[ProjectedSystemToContainerFlow] =
    containerFlows.map(flow => ProjectedSystemToContainerFlow(flow))

  val systemToSystemFlowsToBeCreated: List[(Key, Key, String)] =
    projectedSystemToSystemFlows
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.containerFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val containerToSystemFlowsToBeCreated: List[(Key, Key, String)] =
    projectedContainerToSystemFlows
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.containerFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val systemToContainerFlowsToBeCreated: List[(Key, Key, String)] =
    projectedSystemToContainerFlows
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.containerFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val flowsToBeCreated: List[(Key, Key, String)] =
    systemToSystemFlowsToBeCreated ::: containerToSystemFlowsToBeCreated ::: systemToContainerFlowsToBeCreated
}

case class ProjectedSystemToSystemFlow(containerFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.source)
  val targetContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.target)
  val sourceSystem: Option[ItSystem] =
    sourceContainer.flatMap(ea.townPlan.system(_))
  val targetSystem: Option[ItSystem] =
    targetContainer.flatMap(ea.townPlan.system(_))
  val sourceSystemKey: Key = sourceSystem.map(_.key).getOrElse(Key())
  val targetSystemKey: Key = targetSystem.map(_.key).getOrElse(Key())
  val existingSystemFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == sourceSystemKey && r.target == targetSystemKey)
    .map(_.asInstanceOf[Flow])
  val isDifferentSystems: Boolean =
    sourceSystem.isDefined && targetSystem.isDefined && sourceSystem.get.key != targetSystem.get.key
  val shouldBeCreated: Boolean =
    existingSystemFlow.isEmpty && isDifferentSystems
  val keyPair: SystemFlowProjectedKeyPair =
    SystemFlowProjectedKeyPair(sourceSystemKey, targetSystemKey)
}

case class ProjectedContainerToSystemFlow(containerFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.source)
  val targetContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.target)
  val sourceSystem: Option[ItSystem] =
    sourceContainer.flatMap(ea.townPlan.system(_))
  val targetSystem: Option[ItSystem] =
    targetContainer.flatMap(ea.townPlan.system(_))
  val targetSystemKey: Key = targetSystem.map(_.key).getOrElse(Key())
  val existingContainerToSystemFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == containerFlow.source && r.target == targetSystemKey)
    .map(_.asInstanceOf[Flow])
  val isDifferentSystems: Boolean =
    sourceSystem.isDefined && targetSystem.isDefined && sourceSystem.get.key != targetSystem.get.key
  val shouldBeCreated: Boolean =
    existingContainerToSystemFlow.isEmpty && isDifferentSystems
  val keyPair: SystemFlowProjectedKeyPair =
    SystemFlowProjectedKeyPair(containerFlow.source, targetSystemKey)
}

case class ProjectedSystemToContainerFlow(containerFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.source)
  val targetContainer: Option[ItContainer] =
    ea.townPlan.container(containerFlow.target)
  val sourceSystem: Option[ItSystem] =
    sourceContainer.flatMap(ea.townPlan.system(_))
  val targetSystem: Option[ItSystem] =
    targetContainer.flatMap(ea.townPlan.system(_))
  val sourceSystemKey: Key = sourceSystem.map(_.key).getOrElse(Key())
  val existingSystemToContainerFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == sourceSystemKey && r.target == containerFlow.target)
    .map(_.asInstanceOf[Flow])
  val isDifferentSystems: Boolean =
    sourceSystem.isDefined && targetSystem.isDefined && sourceSystem.get.key != targetSystem.get.key
  val shouldBeCreated: Boolean =
    existingSystemToContainerFlow.isEmpty && isDifferentSystems
  val keyPair: SystemFlowProjectedKeyPair =
    SystemFlowProjectedKeyPair(sourceSystemKey, containerFlow.target)
}

case class SystemFlowProjectedKeyPair(source: Key, target: Key)
