package ImplementLift

import reflect.macros.blackbox.Context

trait MacroImplementLiftables extends ImplementLiftables with XMLNodes {
  val c: Context
  protected lazy val __universe: c.universe.type = c.universe
}

