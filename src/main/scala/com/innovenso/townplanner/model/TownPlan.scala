package com.innovenso.townplanner.model

import com.innovenso.townplanner.model.concepts._
import com.innovenso.townplanner.model.concepts.properties.CanAddProperties
import com.innovenso.townplanner.model.concepts.relationships.{
  CanAddRelationships,
  HasRelationships
}
import com.innovenso.townplanner.model.concepts.views._
import com.innovenso.townplanner.model.language.{
  CanAddModelComponents,
  HasModelComponents,
  ModelComponent
}
import com.innovenso.townplanner.model.meta.Key

case class TownPlan(
    modelComponents: Map[Key, ModelComponent]
) extends HasModelComponents
    with HasEnterprises
    with HasRelationships
    with HasTechnologies
    with HasBusinessCapabilities
    with HasBusinessActors
    with HasArchitectureBuildingBlocks
    with HasItPlatforms
    with HasItSystems
    with HasItContainers
    with HasDecisions
    with HasPrinciples
    with HasItSystemIntegrations
    with HasProjects
    with HasFlowViews
    with HasSystemContainerViews
    with HasSystemIntegrationViews
    with HasPlatformSystemViews
    with HasBusinessCapabilityMaps
    with HasBusinessCapabilityPositions
    with HasArchitectureBuildingBlockRealizationViews
    with HasIntegrationMaps
    with HasFullTownPlanViews
    with HasSystemIntegrationInteractionViews
    with HasProjectMilestoneImpactViews
    with HasDecisionImpactViews
    with HasTechnologyRadars
    with HasArchitectureDecisionRecords
    with HasKnowledgeMatrices
    with HasTags
    with HasProjectMilestoneTransitionSystemContainerViews
    with HasProjectMilestoneOverviews
    with HasPlatformLayers
    with HasDataObjects
    with HasDataModelViews
    with HasRisks
    with HasRiskRegister

case class EnterpriseArchitecture()
    extends CanAddModelComponents
    with CanAddProperties
    with CanAddRelationships
    with CanAddEnterprises
    with CanAddTechnologies
    with CanAddBusinessCapabilities
    with CanAddArchitectureBuildingBlocks
    with CanAddBusinessActors
    with CanAddPrinciples
    with CanAddItPlatforms
    with CanAddItSystems
    with CanAddItContainers
    with CanAddDecisions
    with CanAddItSystemIntegrations
    with CanAddFlowViews
    with CanAddProjects
    with CanAddSystemContainerViews
    with CanAddSystemIntegrationViews
    with CanAddPlatformSystemViews
    with CanAddBusinessCapabilityMaps
    with CanAddBusinessCapabilityPositions
    with CanAddArchitectureBuildingBlockRealizationViews
    with CanAddIntegrationMaps
    with CanAddFullTownPlanViews
    with CanAddSystemIntegrationInteractionViews
    with CanAddProjectMilestoneImpactViews
    with CanAddDecisionImpactViews
    with CanAddTechnologyRadars
    with CanAddArchitectureDecisionRecords
    with CanAddKnowledgeMatrices
    with CanAddTags
    with CanAddProjectMilestoneTransitionSystemContainerViews
    with CanAddProjectMilestoneOverviews
    with CanAddPlatformLayers
    with CanAddDataObjects
    with CanAddDataModelViews
    with CanAddRisks
    with CanAddRiskRegister
