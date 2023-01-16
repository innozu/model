package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.relationships.Relationship
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class DataObjectSpec extends AnyFlatSpec with GivenWhenThen {
  "data objects" can "be added to the town plan" in new EnterpriseArchitectureContext {
    When("data objects are added to the town plan")
    val entity: Entity = samples.entity
    val aggregateRoot: AggregateRoot = samples.aggregateRoot
    val relationship: Relationship =
      samples.dataRelationship(aggregateRoot, entity)
    And("a system owns them")
    val system: ItSystem = samples.system()
    val systemOwnsEntity: Relationship = samples.own(system, entity)
    val systemOwnsAggregateRoot: Relationship =
      samples.own(system, aggregateRoot)
    Then("The data objects exist")
    assert(exists(entity))
    assert(exists(aggregateRoot))
    And("The relationships exist")
    assert(exists(relationship))
    assert(exists(systemOwnsEntity))
    assert(exists(systemOwnsAggregateRoot))
  }
}
