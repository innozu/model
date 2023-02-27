package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.language.ModelComponent
import com.innovenso.townplanner.model.processor.TownPlanProcessor
import com.innovenso.townplanner.model.{EnterpriseArchitecture, TownPlan}
import fish.genius.logging.Loggable

trait EnterpriseArchitectureContext extends Loggable {
  val ea: EnterpriseArchitecture = EnterpriseArchitecture()

  def exists[ModelComponentType <: ModelComponent](
      modelComponent: ModelComponentType
  ): Boolean = {
    def e = ea.townPlan.has(modelComponent)
    info(s"does $modelComponent exist? $e")
    e
  }

  def townPlan: TownPlan = ea.townPlan

  def process(processor: TownPlanProcessor): Unit = processor.process()
}
