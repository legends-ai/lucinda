package asuna.lucinda

import asuna.proto.league.{ Locale, PatchRange, Region }
import asuna.proto.league.vulgate.Context

object VulgateHelpers {

  def makeVulgateContext(patches: Option[PatchRange], region: Region, locale: Locale = Locale.en_US) = {
    // Default to an empty version if a patch range is not specified. This tells Vulgate to use the latest version.
    val release = patches match {
      case Some(range) => Context.Release.Patch(range.max)
      case None => Context.Release.Empty
    }
    Context(
      region = region,
      release = release
    )
  }

  def makeVulgateContextOfPatch(patch: String, region: Region, locale: Locale = Locale.en_US) = {
    Context(
      region = region,
      release = Context.Release.Patch(patch)
    )
  }

}
