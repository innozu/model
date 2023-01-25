package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.relationships.{DataRelationship, Owning, Relationship}
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class DataObjectSpec extends AnyFlatSpec with GivenWhenThen {
  "data objects" can "be added to the town plan" in new EnterpriseArchitectureContext {
    When("data objects are added to the town plan")
    val entity: Entity = ea hasRandomDataObject (Entity())
    val aggregateRoot: AggregateRoot = ea hasRandomDataObject (AggregateRoot())
    val relationship: Relationship = ea hasRelationship DataRelationship(
      source = aggregateRoot.key,
      target = entity.key
    )
    And("a system owns them")
    val system: ItSystem = ea hasRandom (ItSystem())
    val systemOwnsEntity: Relationship =
      ea hasRelationship Owning(source = system.key, target = entity.key)
    val systemOwnsAggregateRoot: Relationship =
      ea hasRelationship Owning(source = system.key, target = aggregateRoot.key)
    Then("The data objects exist")
    assert(exists(entity))
    assert(exists(aggregateRoot))
    And("The relationships exist")
    assert(exists(relationship))
    assert(exists(systemOwnsEntity))
    assert(exists(systemOwnsAggregateRoot))
  }
}
