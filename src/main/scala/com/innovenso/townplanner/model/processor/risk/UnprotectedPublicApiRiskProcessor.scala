package com.innovenso.townplanner.model.processor.risk

import com.innovenso.townplanner.model.EnterpriseArchitecture
import com.innovenso.townplanner.model.concepts.{
  ItContainer,
  Risk,
  SecurityVulnerability
}
import com.innovenso.townplanner.model.concepts.properties.{
  CounterMeasure,
  Description,
  Hazardous,
  Major,
  NoAuthentication,
  NoDDosProtection,
  NoRateLimiting,
  PublicScope
}
import com.innovenso.townplanner.model.processor.TownPlanProcessor

case class UnprotectedPublicApiRiskProcessor()(implicit
    ea: EnterpriseArchitecture
) extends TownPlanProcessor {
  override def process(): Unit = {
    risksForPublicUnprotectedApis()
    risksForPublicApisWithoutDdosProtection()
    risksForPublicApisWithoutRateLimiting()
  }

  val containersWithApis: List[ItContainer] =
    ea.townPlan.containers.filter(_.api.isDefined)

  val containersWithPublicApis: List[ItContainer] =
    containersWithApis.filter(_.api.get.scope.getClass == classOf[PublicScope])

  val containersWithUnprotectedPublicApis: List[ItContainer] =
    containersWithPublicApis.filter(c =>
      c.api.get.authentication match {
        case none: NoAuthentication => true
        case _ => false
      }
    )

  val containersWithPublicApisWithoutRateLimiting: List[ItContainer] =
    containersWithPublicApis.filter( c =>
      c.api.get.rateLimiting match {
        case none: NoRateLimiting => true
        case _ => false
      }
    )

  val containersWithPublicApisWithoutDdosProtection: List[ItContainer] =
    containersWithPublicApis.filter(c =>
      c.api.get.ddoSProtection match {
        case none: NoDDosProtection => true
        case _                      => false
      }
    )

  def risksForPublicUnprotectedApis(): Unit =
    containersWithUnprotectedPublicApis.foreach(unprotectedContainer => {
      println(
        s"adding risk for public API without authentication on IT Container ${unprotectedContainer.title}"
      )
      ea describes Risk(
        title = s"${unprotectedContainer.title}: no authentication",
        typeOfRisk = SecurityVulnerability
      ) as { it =>
        it ratesImpactAs Hazardous(
          "exposing vulnerable APIs, especially those containing personal or sensitive data, would have a major impact on the organisation"
        )
        it has Description(
          s"${unprotectedContainer.title} has an API that is available publicly (through the public internet), without having any form of authentication."
        )
        it has CounterMeasure(description =
          s"Add some form of authentication to the API of ${unprotectedContainer.title}, or do not expose the API to the internet."
        )
        it isAssociatedWith unprotectedContainer
      }
    })

  def risksForPublicApisWithoutRateLimiting(): Unit =
    containersWithPublicApisWithoutRateLimiting.foreach(unlimitedContainer => {
      println(
        s"adding risk for public API without rate limiting on IT Container ${unlimitedContainer.title}"
      )
      ea describes Risk(
        title = s"${unlimitedContainer.title}: no rate limiting",
        typeOfRisk = SecurityVulnerability
      ) as { it =>
        it ratesImpactAs Major(
          "when a public API has no rate limiting, it is open to potential brute force attacks."
        )
        it has Description(
          s"${unlimitedContainer.title} has an API that is available publicly (through the public internet), without having any form of rate limiting."
        )
        it has CounterMeasure(description =
          s"Add some form of rate limiting to the API of ${unlimitedContainer.title}, typically by putting an API Gateway in front of it, or do not expose the API to the internet."
        )
        it isAssociatedWith unlimitedContainer
      }
    })

  def risksForPublicApisWithoutDdosProtection(): Unit =
    containersWithPublicApisWithoutDdosProtection.foreach(
      unprotectedContainer => {
        println(
          s"adding risk for public API without DDoS protection on IT Container ${unprotectedContainer.title}"
        )
        ea describes Risk(
          title = s"${unprotectedContainer.title}: no DDoS protection",
          typeOfRisk = SecurityVulnerability
        ) as { it =>
          it ratesImpactAs Major(
            "when a public API has no DDoS protect, it is vulnerable to distributed denial of service attacks, which risks bringing the entire system down."
          )
          it has Description(
            s"${unprotectedContainer.title} has an API that is available publicly (through the public internet), without having any form of DDoS protection."
          )
          it has CounterMeasure(description =
            s"Add some form of DDoS protection to the API of ${unprotectedContainer.title}, for example putting a Web Application Firewall (WAF) in front of it, or do not expose the API to the internet."
          )
          it isAssociatedWith unprotectedContainer
        }
      }
    )

}
