package org.getshaka.shaka.router

import scala.util.matching.Regex

trait Routable:
  def path: Regex
