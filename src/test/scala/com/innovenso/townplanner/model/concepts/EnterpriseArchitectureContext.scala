package com.innovenso.townplanner.model.concepts

import com.innovenso.townplanner.model.language.ModelComponent
import com.innovenso.townplanner.model.processor.TownPlanProcessor
import com.innovenso.townplanner.model.samples.SampleFactory
import com.innovenso.townplanner.model.{EnterpriseArchitecture, TownPlan}

trait EnterpriseArchitectureContext {
  val ea: EnterpriseArchitecture = EnterpriseArchitecture()

  def exists[ModelComponentType <: ModelComponent](
      modelComponent: ModelComponentType
  ): Boolean = {
    ea.townPlan.has(modelComponent)
  }

  def townPlan: TownPlan = ea.townPlan

  def process(processor: TownPlanProcessor): Unit = processor.process()
}
