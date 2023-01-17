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
    with HasTags
    with HasPlatformLayers
    with HasDataObjects
    with HasRisks

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
    with CanAddTags
    with CanAddPlatformLayers
    with CanAddDataObjects
    with CanAddRisks
