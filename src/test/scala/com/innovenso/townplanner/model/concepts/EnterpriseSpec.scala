package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.concepts.properties.{
  Description,
  Title,
  Website
}
import com.innovenso.townplanner.model.concepts.relationships.Composition
import org.scalatest.GivenWhenThen
import org.scalatest.flatspec.AnyFlatSpec

class EnterpriseSpec extends AnyFlatSpec with GivenWhenThen {
  "Enterprises" can "be added to the townplan" in new EnterpriseArchitectureContext {
    val innovenso: Enterprise =
      ea describes Enterprise() as { it =>
        it has Title("Innovenso")
        it has Description("hello")
        it has Website(url = "https://innovenso.com")
      }

    val geniusfish: Enterprise = ea hasRandomEnterprise ()

    val innovensogroup: Enterprise =
      ea describes Enterprise() as { it =>
        it has Title("Innovenso Group")
        it isComposedOf geniusfish
      }

    ea hasRelationship Composition(
      source = innovenso.key,
      target = geniusfish.key
    ).withTitle(Title("composition")).asInstanceOf[Composition]

    assert(townPlan.enterprise(innovenso.key).exists(_.descriptions.nonEmpty))
    assert(townPlan.enterprise(innovenso.key).exists(_.websiteLinks.nonEmpty))
    assert(exists(geniusfish))
    assert(exists(innovensogroup))
    assert(townPlan.relationships.size == 2)
  }
}
