package com.innovenso.townplanner.model.meta

import com.innovenso.townplanner.model.samples

import java.util.Currency

case class MonetaryAmount(
    amount: Double,
    currency: Currency
) {
  override def toString: String = f"${currency.getSymbol}${amount}%2.2f"
}

object MonetaryAmount {
  def apply(
      amount: Double,
      currency: Currency = Currency.getInstance("EUR")
  ): MonetaryAmount = new MonetaryAmount(amount, currency)

  def random: MonetaryAmount = apply(samples.randomDouble(10000d))
}
