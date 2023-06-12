import sbt._

object Dependencies {
  object Testing {
    private val _version = "3.2.16"
    final val * = Seq(
      "org.scalactic" %% "scalactic" % _version,
      "org.scalatest" %% "scalatest" % _version % Test
    )
  }
  object Apache {
    object Commons {
      final val commonsText = "org.apache.commons" % "commons-text" % "1.10.0"
      final val * = Seq(commonsText)
    }
  }

  object GeniusFish {
    val _version = "2.1.3"
    final val config = "fish.genius" %% "config" % _version
    final val logging = "fish.genius" %% "logging" % _version
    final val lorem = "fish.genius" %% "lorem" % _version
    final val * = Seq(config, logging, lorem)
  }
}
