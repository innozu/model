package com.innovenso.townplanner.model.concepts.views

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.relationships._
import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.language._
import com.innovenso.townplanner.model.meta._

case class ArchitectureDecisionRecord(
    key: Key = Key("view"),
    sortKey: SortKey = SortKey.next,
    title: String = "Architecture Decision Record",
    properties: Map[Key, Property] = Map.empty[Key, Property]
) extends TimelessView {
  val modelComponentType: ModelComponentType = ModelComponentType(
    "Architecture Decision Record",
    classOf[ArchitectureDecisionRecord]
  )
  val layer: Layer = MotivationLayer

  def withProperty(property: Property): ArchitectureDecisionRecord =
    copy(properties = this.properties + (property.key -> property))

}

trait HasArchitectureDecisionRecords
    extends HasViews
    with HasBusinessActors
    with HasItPlatforms
    with HasItSystems
    with HasItSystemIntegrations
    with HasItContainers
    with HasTechnologies
    with HasEnterprises
    with HasBusinessCapabilities
    with HasArchitectureBuildingBlocks
    with HasDecisions
    with HasPrinciples {
  def architectureDecisionRecords: List[CompiledArchitectureDecisionRecord] =
    components(
      classOf[ArchitectureDecisionRecord]
    ).map(view => ArchitectureDecisionRecordCompiler(view, this).compile)
  def architectureDecisionRecord(
      key: Key
  ): Option[CompiledArchitectureDecisionRecord] =
    component(key, classOf[ArchitectureDecisionRecord]).map(
      ArchitectureDecisionRecordCompiler(_, this).compile
    )

}

trait CanAddArchitectureDecisionRecords
    extends CanAddProperties
    with CanAddRelationships {
  def needs(
      architectureDecisionRecord: ArchitectureDecisionRecord
  ): ArchitectureDecisionRecord =
    has(architectureDecisionRecord)
}

case class CompiledArchitectureDecisionRecord(
    view: ArchitectureDecisionRecord,
    title: String,
    groupTitle: String,
    modelComponents: Map[Key, ModelComponent]
) extends CompiledView[ArchitectureDecisionRecord]
    with HasRelationships
    with HasBusinessActors
    with HasItPlatforms
    with HasItSystems
    with HasItSystemIntegrations
    with HasItContainers
    with HasTechnologies
    with HasEnterprises
    with HasBusinessCapabilities
    with HasArchitectureBuildingBlocks
    with HasDecisions
    with HasPrinciples
    with HasFlowViews {
  val decoratedDecisions: List[DecisionDecorator] =
    decisions.map(DecisionDecorator(this, _))

  def decoratedDecision(key: Key): Option[DecisionDecorator] =
    decision(key).map(DecisionDecorator(this, _))
}

case class DecisionOptionDecorator(
    view: CompiledArchitectureDecisionRecord,
    option: DecisionOption
) {
  val hasStrengths: Boolean = option.strengths.nonEmpty
  val hasWeaknesses: Boolean = option.weaknesses.nonEmpty
  val hasOpportunities: Boolean = option.opportunities.nonEmpty
  val hasThreats: Boolean = option.threats.nonEmpty
  val hasSWOT: Boolean =
    hasStrengths || hasWeaknesses || hasOpportunities || hasThreats
  val hasCostImpact: Boolean = option.costs.nonEmpty
  val functionalScores: List[(Requirement, RequirementScore)] =
    view.functionalRequirementScores(option)
  val qualityAttributeRequirementScores: List[(Requirement, RequirementScore)] =
    view.qualityAttributeRequirementScores(option)
  val constraintScores: List[(Requirement, RequirementScore)] =
    view.constraintScores(option)
  val decision: Option[Decision] = view.decision(option)
  def illustration(context: Context): Option[FlowView] =
    context.illustratedByView.flatMap(key => view.flowView(key).map(_.view))

  def hasIllustration(context: Context): Boolean = illustration(
    context
  ).isDefined

  val scores: List[(Requirement, RequirementScore)] =
    functionalScores ::: qualityAttributeRequirementScores ::: constraintScores
  val hasFunctionalRequirementScores: Boolean = functionalScores.nonEmpty
  val hasQualityAttributeRequirementScores: Boolean =
    qualityAttributeRequirementScores.nonEmpty
  val hasConstraintScores: Boolean = constraintScores.nonEmpty
  val hasRequirementScores: Boolean =
    hasFunctionalRequirementScores || hasQualityAttributeRequirementScores || hasConstraintScores
}

case class DecisionDecorator(
    view: CompiledArchitectureDecisionRecord,
    decision: Decision
) {
  val hasCurrentConditions: Boolean = decision.currentState.nonEmpty
  def illustration(context: Context): Option[FlowView] =
    context.illustratedByView.flatMap(key => view.flowView(key).map(_.view))

  def hasIllustration(context: Context): Boolean = illustration(
    context
  ).isDefined

  val hasDescriptions: Boolean = decision.descriptions.nonEmpty
  val hasLinks: Boolean = decision.links.nonEmpty
  val hasGoals: Boolean = decision.goals.nonEmpty
  val hasAssumptions: Boolean =
    decision.assumptions.nonEmpty
  val hasConsequences: Boolean =
    decision.consequences.nonEmpty

  val hasOpenQuestions: Boolean = decision.openQuestions.nonEmpty
  val hasDueDate: Boolean = decision.dueDate.isDefined
  val hasOutcome: Boolean = !decision.outcome.isBlank
  val responsible: List[Person] =
    view.directDependencies(decision, classOf[Responsible], classOf[Person])
  val accountable: List[Person] =
    view.directDependencies(decision, classOf[Accountable], classOf[Person])
  val consulted: List[Person] =
    view.directDependencies(decision, classOf[Consulted], classOf[Person])
  val informed: List[Person] =
    view.directDependencies(decision, classOf[Informed], classOf[Person])
  val stakeholders: List[Person] =
    view.directDependencies(decision, classOf[Stakeholder], classOf[Person])
  val influencers: List[Person] =
    view.directDependencies(decision, classOf[Influence], classOf[Person])
  val influencingPrinciples: List[Principle] =
    view.directDependencies(decision, classOf[Influence], classOf[Principle])

  val hasResponsible: Boolean = responsible.nonEmpty
  val hasAccountable: Boolean = accountable.nonEmpty
  val hasConsulted: Boolean = consulted.nonEmpty
  val hasInformed: Boolean = informed.nonEmpty
  val hasStakeholders: Boolean = stakeholders.nonEmpty
  val hasInfluencers: Boolean = influencers.nonEmpty
  val hasInfluencingPrinciples: Boolean =
    influencingPrinciples.nonEmpty
  val hasNemawashi: Boolean =
    hasResponsible || hasAccountable || hasConsulted || hasInformed

  val hasContext: Boolean =
    hasNemawashi || hasDescriptions || hasCurrentConditions || hasGoals || hasAssumptions || hasInfluencers || hasInfluencingPrinciples

  val hasRequirements: Boolean = decision.requirements.nonEmpty
  val hasFunctionalRequirements: Boolean =
    decision.functionalRequirements.nonEmpty
  val hasQualityAttributeRequirements: Boolean =
    decision.qualityAttributeRequirements.nonEmpty
  val hasConstraints: Boolean = decision.constraints.nonEmpty

  val hasSecurityImpact: Boolean = decision.securityImpacts.nonEmpty
  val hasComplianceImpact: Boolean = decision.complianceConcerns.nonEmpty

  val options: List[DecisionOptionDecorator] =
    view.options(decision).map(DecisionOptionDecorator(view, _))

}

case class ArchitectureDecisionRecordCompiler(
    view: ArchitectureDecisionRecord,
    source: HasArchitectureDecisionRecords
) extends ViewCompiler[
      ArchitectureDecisionRecord,
      CompiledArchitectureDecisionRecord,
      HasArchitectureDecisionRecords
    ] {
  def compile: CompiledArchitectureDecisionRecord =
    CompiledArchitectureDecisionRecord(
      view,
      viewTitle,
      "Architecture Decision Record",
      viewComponents(
        source.modelComponents.values
      )
    )
}
