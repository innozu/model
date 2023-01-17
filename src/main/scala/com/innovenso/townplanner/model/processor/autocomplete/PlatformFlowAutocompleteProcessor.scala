package com.innovenso.townplanner.model.processor.autocomplete

import com.innovenso.townplanner.model.EnterpriseArchitecture
import com.innovenso.townplanner.model.concepts.properties.Title
import com.innovenso.townplanner.model.concepts.relationships.Flow
import com.innovenso.townplanner.model.concepts.{ItPlatform, ItSystem}
import com.innovenso.townplanner.model.language.Element
import com.innovenso.townplanner.model.meta.Key
import com.innovenso.townplanner.model.processor.TownPlanProcessor
import fish.genius.logging.Loggable

case class PlatformFlowAutocompleteProcessor()(implicit
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

  val systemFlows: List[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .map(_.asInstanceOf[Flow])
    .filter(
      ea.townPlan
        .relationshipParticipants(_)
        .forall(_.isInstanceOf[ItSystem])
    )

  val pppList: List[PPP] =
    systemFlows.map(flow => PPP(flow))

  val ppsList: List[PPS] =
    systemFlows.map(flow => PPS(flow))

  val pspList: List[PSP] =
    systemFlows.map(flow => PSP(flow))

  val cpp: List[(Key, Key, String)] =
    pppList
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.systemFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val csp: List[(Key, Key, String)] =
    pspList
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.systemFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val cps: List[(Key, Key, String)] =
    ppsList
      .filter(_.shouldBeCreated)
      .groupBy(_.keyPair)
      .view
      .mapValues(_.map(_.systemFlow.title).distinct.mkString(", "))
      .toMap
      .toList
      .map(tuple => (tuple._1.source, tuple._1.target, tuple._2))

  val flowsToBeCreated: List[(Key, Key, String)] =
    cpp ::: csp ::: cps
}

case class PPP(systemFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.source)
  val targetSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.target)
  val sourcePlatform: Option[ItPlatform] =
    sourceSystem.flatMap(ea.townPlan.systemPlatform(_))
  val targetPlatform: Option[ItPlatform] =
    targetSystem.flatMap(ea.townPlan.systemPlatform(_))
  val sourcePlatformKey: Key = sourcePlatform.map(_.key).getOrElse(Key())
  val targetPlatformKey: Key = targetPlatform.map(_.key).getOrElse(Key())
  val existingPlatformToPlatformFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == sourcePlatformKey && r.target == targetPlatformKey)
    .map(_.asInstanceOf[Flow])
  val isDifferentPlatforms: Boolean =
    sourcePlatform.isDefined && targetPlatform.isDefined && sourcePlatform.get.key != targetPlatform.get.key
  val shouldBeCreated: Boolean =
    existingPlatformToPlatformFlow.isEmpty && isDifferentPlatforms

  val keyPair: PlatformFlowProjectedKeyPair =
    PlatformFlowProjectedKeyPair(sourcePlatformKey, targetPlatformKey)
}

case class PSP(systemFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.source)
  val targetSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.target)
  val sourcePlatform: Option[ItPlatform] =
    sourceSystem.flatMap(ea.townPlan.systemPlatform(_))
  val targetPlatform: Option[ItPlatform] =
    targetSystem.flatMap(ea.townPlan.systemPlatform(_))
  val targetPlatformKey: Key = targetPlatform.map(_.key).getOrElse(Key())
  val existingSystemToPlatformFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == systemFlow.source && r.target == targetPlatformKey)
    .map(_.asInstanceOf[Flow])
  val isDifferentPlatforms: Boolean =
    sourcePlatform.isDefined && targetPlatform.isDefined && sourcePlatform.get.key != targetPlatform.get.key
  val shouldBeCreated: Boolean =
    existingSystemToPlatformFlow.isEmpty && isDifferentPlatforms
  val keyPair: PlatformFlowProjectedKeyPair =
    PlatformFlowProjectedKeyPair(systemFlow.source, targetPlatformKey)
}

case class PPS(systemFlow: Flow)(implicit
    ea: EnterpriseArchitecture
) {
  val sourceSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.source)
  val targetSystem: Option[ItSystem] =
    ea.townPlan.system(systemFlow.target)
  val sourcePlatform: Option[ItPlatform] =
    sourceSystem.flatMap(ea.townPlan.systemPlatform(_))
  val targetPlatform: Option[ItPlatform] =
    targetSystem.flatMap(ea.townPlan.systemPlatform(_))
  val sourcePlatformKey: Key = sourcePlatform.map(_.key).getOrElse(Key())
  val existingPlatformToSystemFlow: Option[Flow] = ea.townPlan.relationships
    .filter(_.isInstanceOf[Flow])
    .find(r => r.source == sourcePlatformKey && r.target == systemFlow.target)
    .map(_.asInstanceOf[Flow])
  val isDifferentPlatforms: Boolean =
    sourcePlatform.isDefined && targetPlatform.isDefined && sourcePlatform.get.key != targetPlatform.get.key
  val shouldBeCreated: Boolean =
    existingPlatformToSystemFlow.isEmpty && isDifferentPlatforms
  val keyPair: PlatformFlowProjectedKeyPair =
    PlatformFlowProjectedKeyPair(sourcePlatformKey, systemFlow.target)
}

case class PlatformFlowProjectedKeyPair(source: Key, target: Key)
