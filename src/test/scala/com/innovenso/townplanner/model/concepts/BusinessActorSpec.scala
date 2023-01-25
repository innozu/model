package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.relationships.Knowledge
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class BusinessActorSpec extends AnyFlatSpec with GivenWhenThen {
  "Business Actors" can "be added to the town plan" in new EnterpriseArchitectureContext {
    val townplanner: Tool = ea hasRandomTech Tool()
    val jurgen: Person = ea describesRandomActor (Person()) as { he =>
      he knows townplanner
    }

    assert(exists(jurgen))
    assert(
      townPlan.businessActor(jurgen.key).exists(he => he.descriptions.nonEmpty)
    )
    assert(townPlan.relationshipsWithType(classOf[Knowledge]).size == 1)
  }

}
