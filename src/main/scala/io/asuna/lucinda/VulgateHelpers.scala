package io.asuna.lucinda

import io.asuna.proto.enums.{ Locale, Region }
import io.asuna.proto.range.PatchRange
import io.asuna.proto.vulgate.VulgateData
import io.asuna.proto.service_vulgate.VulgateRpc


object VulgateHelpers {

  def makeVulgateContext(patches: Option[PatchRange], region: Region, locale: Locale = Locale.en_US) = {
    // Default to an empty version if a patch range is not specified. This tells Vulgate to use the latest version.
    val release = patches match {
      case Some(range) => VulgateRpc.Context.Release.Patch(range.max)
      case None => VulgateRpc.Context.Release.Empty
    }
    VulgateRpc.Context(
      region = region,
      release = release
    )
  }

  def makeVulgateContextOfPatch(patch: String, region: Region, locale: Locale = Locale.en_US) = {
    VulgateRpc.Context(
      region = region,
      release = VulgateRpc.Context.Release.Patch(patch)
    )
  }

}
