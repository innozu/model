package com.innovenso.townplanner.model.meta

import java.util.UUID

import org.apache.commons.text.WordUtils

case class Key(value: String) {
  def camelCased: String = WordUtils
    .capitalizeFully(value, '-', '_')
    .replaceAll("-", "")
    .replaceAll("_", "")
    .replaceAll("0", "Zero")
    .replaceAll("1", "One")
    .replaceAll("2", "Two")
    .replaceAll("3", "Three")
    .replaceAll("4", "Four")
    .replaceAll("5", "Five")
    .replaceAll("6", "Six")
    .replaceAll("7", "Seven")
    .replaceAll("8", "Eight")
    .replaceAll("9", "Nine")

  override def toString: String = value
}

object Key {
  val regex = "[^A-Za-z0-9]"
  private val counterMap = scala.collection.mutable.Map[String, Int]()

  private def next(category: String) = {
    val index: Int = counterMap.getOrElse(category, 1)
    counterMap(category) = index + 1
    index
  }

  def apply(): Key = apply("concept")

  def apply(category: String): Key = new Key(value(category, next(category)))

  def value(category: String, number: Int): String =
    s"${cleanString(category)}_${number}"

  def random: Key = new Key(cleanString(UUID.randomUUID().toString))
  def fromString(value: String): Key = new Key(cleanString(fillString(value)))

  def cleanString(input: String): String =
    input.replaceAll(regex, "_").toLowerCase()

  def fillString(input: String): String = Option(input) match {
    case Some(string) if string.isBlank  => UUID.randomUUID().toString
    case Some(string) if !string.isBlank => string
    case _                               => UUID.randomUUID().toString
  }

}
