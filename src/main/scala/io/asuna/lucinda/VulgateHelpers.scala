package io.asuna.lucinda

import io.asuna.proto.enums.{ Locale, Region }
import io.asuna.proto.range.PatchRange
import io.asuna.proto.vulgate.VulgateData


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

}
