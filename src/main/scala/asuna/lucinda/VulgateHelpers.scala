package asuna.lucinda

import asuna.proto.enums.{ Locale, Region }
import asuna.proto.range.PatchRange
import asuna.proto.vulgate.VulgateData
import asuna.proto.service_vulgate.VulgateRpc


object VulgateHelpers {

  def makeVulgateContext(patches: Option[PatchRange], region: Region, locale: Locale = Locale.en_US) = {
    // Default to an empty version if a patch range is not specified. This tells Vulgate to use the latest version.
    val release = patches match {
      case Some(range) => VulgateData.Context.Release.Patch(range.max)
      case None => VulgateData.Context.Release.Empty
    }
    VulgateData.Context(
      region = region,
      release = release
    )
  }

  def makeVulgateContextOfPatch(patch: String, region: Region, locale: Locale = Locale.en_US) = {
    VulgateData.Context(
      region = region,
      release = VulgateData.Context.Release.Patch(patch)
    )
  }

}