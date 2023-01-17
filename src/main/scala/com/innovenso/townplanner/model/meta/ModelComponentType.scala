package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.language.ModelComponent

case class ModelComponentType(
    value: String,
    modelComponentClass: Class[_ <: ModelComponent]
) {
  override def toString: String = value
}
