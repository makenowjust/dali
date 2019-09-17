package codes.quine.labo
package dali

import minitest.runner.Options

class Framework extends minitest.runner.Framework {
  override def options: Options = Options(useSbtLogging = true)
}
