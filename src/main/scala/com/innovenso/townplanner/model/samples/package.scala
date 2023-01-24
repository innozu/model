package com.innovenso.townplanner.model

import com.innovenso.townplanner.model.concepts.properties._
import com.innovenso.townplanner.model.meta.{
  MonetaryAmount,
  UnitCount,
  UnitOfMeasure
}
import com.thedeanda.lorem.LoremIpsum

import java.security.SecureRandom
import java.util.{Currency, Locale, UUID}

package object samples {
  private val random = new SecureRandom()
  private val lorem = LoremIpsum.getInstance()

  def locale: Locale = {
    val index = randomInt(Locale.getAvailableLocales.length)
    Locale.getAvailableLocales.toList(index)
  }

  def randomInt(bound: Int): Int =
    random.nextInt(bound) + 1

  def randomDouble(bound: Double): Double = random.nextDouble(bound)

  def times[A](bound: Int, f: Int => A): List[A] =
    (1 to randomInt(bound)).map(f.apply).toList

  def url: String = lorem.getUrl

  def email: String = lorem.getEmail

  def word: String = lorem.getWords(1)

  def description: String = lorem.getWords(5, 100)

  def id: String = UUID.randomUUID().toString.replace("-", "_")

  def title: String = lorem.getTitle(1, 3)

  def name: String = lorem.getNameFemale

  def randomBoolean: Boolean = if (randomInt(2) % 2 == 0) true else false

  def unitCount: UnitCount = UnitCount(randomDouble(20))

  def unitOfMeasure: UnitOfMeasure = UnitOfMeasure(title)

  def monetaryAmount: MonetaryAmount =
    MonetaryAmount(randomDouble(1000), Currency.getInstance("EUR"))

  def title(configurer: CanConfigureTitle[_]): Unit =
    configurer.has(Title(title))

  def descriptions(configurer: CanConfigureDescription[_]): Unit =
    times(4, i => configurer.has(Description(description)))

  def verdict(configurer: CanConfigureArchitectureVerdict[_]): Unit =
    configurer.should(ArchitectureVerdict.random)

  def externalIds(configurer: CanConfigureExternalIds[_]): Unit =
    times(4, i => configurer.isIdentifiedAs(word).on(name))

  def resilience(configurer: CanConfigureResilienceMeasures[_]): Unit =
    times(5, i => configurer.provides(ResilienceMeasure(description)))

  def throughput(configurer: CanConfigureThroughput[_]): Unit = {
    configurer.has(Frequency(description))
    configurer.has(Volume(description))
  }
}
