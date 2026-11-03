package com.alecdorrington.codeswitch.models

import com.alecdorrington.omnibrowser.StorageKey
import scala.math.BigDecimal.RoundingMode

/**
  * What the reader's requests to models have cost together, in US dollars, as
  * the popup shows it: each reply priced as it comes, at the rates Iris lists
  * for its model (see [[services.SpendingService]]). It is an estimate, as a
  * reply cancelled before it ends is not priced, and the provider's bill is the
  * record.
  */
object Spending:

  /**
    * What the requests made on this device have cost, in US dollars, kept
    * locally, as each device makes its own and sync storage allows few writes.
    */
  val spent = StorageKey.local[BigDecimal]("spent", BigDecimal(0))

  /** `amount`, in US dollars, to the cent, or less than a cent if it is. */
  def shown(amount: BigDecimal): String =
    if amount > 0 && amount < cent then "less than US$0.01"
    else s"US$$${ amount.setScale(2, RoundingMode.HALF_UP) }"

  private val cent = BigDecimal("0.01")
