package dali

import minitest.runner.Options

class MinitestFramework extends minitest.runner.Framework {
  override def options: Options = Options(useSbtLogging = true)
}
