package com.innovenso.townplanner.model.processor.risk

import com.innovenso.townplanner.model.EnterpriseArchitecture
import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.concepts.{ItContainer, Risk, TechnicalDebt, Technology}
import com.innovenso.townplanner.model.processor.TownPlanProcessor
import fish.genius.logging.Loggable

case class LegacyTechnologyTechnicalDebtRiskProcessor()(implicit
    ea: EnterpriseArchitecture
) extends TownPlanProcessor with Loggable {
  override def process(): Unit = createRiskForContainersWithLegacyTechnologies()

  val legacyTechnologies: List[Technology] =
    ea.townPlan.technologies.filter(t =>
      t.architectureVerdict match {
        case eliminated: BeEliminated => true
        case _                        => false
      }
    )

  val containersImplementingLegacyTechnologies
      : List[(Technology, ItContainer)] = legacyTechnologies.flatMap(t =>
    ea.townPlan
      .modelComponentsImplementedByTechnology(t, classOf[ItContainer])
      .map(c => (t, c))
  )

  def createRiskForContainersWithLegacyTechnologies()
      : Unit = containersImplementingLegacyTechnologies.foreach(tc => {
    debug(
      s"creating Technical Debt for IT Container ${tc._2} using Technology ${tc._1}"
    )
    ea describes Risk(
      typeOfRisk = TechnicalDebt
    ) as { it =>
      it has Title(s"${tc._2.title}: legacy technology ${tc._1.title}")
      it has Description(
        s"IT Container ${tc._2.title} still uses technology ${tc._1.title}, even though this technology has been deprecated."
      )
      it ratesImpactAs Minor(
        "Unless a technology has known security vulnerabilities, the risk of this technical debt is relatively minor. It does however need to be repaid."
      )
      it has CounterMeasure(description =
        s"${tc._2.title} should be refactored to replace ${tc._1.title} by a technology we invest in."
      )
      it isAssociatedWith tc._2
      it isAssociatedWith tc._1
    }
  })
}
