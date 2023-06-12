package com.innovenso.townplanner.model.language

import com.innovenso.townplanner.model.meta.Aspect
import com.innovenso.townplanner.model.meta.Layer

trait Element extends Concept {
  def aspect: Aspect
  def layer: Layer
}
