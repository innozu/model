package com.innovenso.townplanner.model.views

import com.innovenso.townplanner.model.concepts.EnterpriseArchitectureContext
import com.innovenso.townplanner.model.concepts.views.DataModelView
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class DataModelViewSpec extends AnyFlatSpec with GivenWhenThen {
  "data model views" should "contain the full dependency tree of every data object included" in new EnterpriseArchitectureContext {
    Given("some data objects")
    val core1 = samples.aggregateRoot
    val core2 = samples.entity
    val core3 = samples.entity
    val dep1 = samples.entity
    val dep2 = samples.entity
    val dep11 = samples.entity
    val dep111 = samples.entity
    val dep21 = samples.entity
    val dep22 = samples.entity
    val dep221 = samples.entity
    val dep222 = samples.entity
    val ext1 = samples.entity
    val ext2 = samples.entity

    And("Relationships between them")
    samples.dataRelationship(core1, core2)
    samples.dataRelationship(dep1, core1)
    samples.dataRelationship(dep2, core2)
    samples.dataRelationship(dep11, dep1)
    samples.dataRelationship(dep111, dep11)
    samples.dataRelationship(dep21, dep2)
    samples.dataRelationship(dep22, dep2)
    samples.dataRelationship(dep221, dep22)
    samples.dataRelationship(dep222, dep22)
    samples.dataRelationship(ext1, ext2)

    When("a data model view is requested")
    val view =
      ea needs DataModelView(forDataObjects = List(core1, core2, core3))
    val compiledView = townPlan.dataModelView(view.key).get

    Then("it contains all relevant dependencies and relationships")
    assert(compiledView.dataObjects.contains(dep222))
    assert(compiledView.dataObjects.contains(dep1))
    assert(compiledView.dataObjects.contains(core3))
    assert(!compiledView.dataObjects.contains(ext1))
    assert(!compiledView.dataObjects.contains(ext2))
    assert(compiledView.dataObjects.size == 11)
    assert(compiledView.relationships.size == 9)
  }
}
