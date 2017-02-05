package asuna.lucinda

import asuna.proto.league.{ Locale, Region }
import asuna.proto.league.vulgate.Context

object VulgateHelpers {

  def makeVulgateContext(patches: Seq[String], region: Region, locale: Locale = Locale.en_US) = {
    import scala.math.Ordering.Implicits._

    val sorted = patches.sortWith { (a, b) =>
      val l = a.split("\\.").map(_.toInt) match { case Array(x, y) => (x, y) }
      val r = b.split("\\.").map(_.toInt) match { case Array(x, y) => (x, y) }
      l < r
    }

    // Default to an empty version if a patch range is not specified.
    // This tells Vulgate to use the latest version.
    Context(
      region = region,
      release = sorted.lastOption match {
        case Some(patch) => Context.Release.Patch(patch)
        case None => Context.Release.Empty
      }
    )
  }

  def makeVulgateContextOfPatch(patch: String, region: Region, locale: Locale = Locale.en_US) = {
    Context(
      region = region,
      release = Context.Release.Patch(patch)
    )
  }

}
