import $file.BigIntHelper_v1

object Voxel {

  // John Amanatides, Andrew Woo
  // A Fast Voxel Traversal Algorithm for Ray Tracing

  def traverse(setVoxel: (BigInt, BigInt) => Boolean)
              (startX: BigInt,
               startY: BigInt,
               endX: BigInt,
               endY: BigInt): Unit = {

    val deltaSignedX = endX - startX
    val deltaSignedY = endY - startY

    val incX = deltaSignedX.signum
    val incY = deltaSignedY.signum

    val deltaX = deltaSignedX.abs
    val deltaY = deltaSignedY.abs

    var cx = startX
    var cy = startY

    if (!setVoxel(cx, cy)) {
      return
    }

    if (cx == endX && cy == endY) {
      return
    }

    val justOutsideX = endX + incX
    val justOutsideY = endY + incY

    var tMaxXOpt = if (deltaX != 0) Some(BigDecimal(1) / BigDecimal(deltaX)) else None
    var tMaxYOpt = if (deltaY != 0) Some(BigDecimal(1) / BigDecimal(deltaY)) else None

    val tDeltaXOpt = if (tMaxXOpt.isDefined) Some(BigDecimal(1) / BigDecimal(deltaX)) else None
    val tDeltaYOpt = if (tMaxYOpt.isDefined) Some(BigDecimal(1) / BigDecimal(deltaY)) else None

    while (true) {
      (tMaxXOpt, tMaxYOpt) match {
        case (Some(tMaxX), None) =>
          cx = cx + incX
          if (cx == justOutsideX) {
            return
          }
          tMaxXOpt = Some(tMaxX + tDeltaXOpt.get)

        case (Some(tMaxX), Some(tMaxY)) if tMaxX < tMaxY =>
          cx = cx + incX
          if (cx == justOutsideX) {
            return
          }
          tMaxXOpt = Some(tMaxX + tDeltaXOpt.get)

        case (_, Some(tMaxY)) =>
          cy = cy + incY

          if (cy == justOutsideY) {
            return
          }
          tMaxYOpt = Some(tMaxY + tDeltaYOpt.get)
        case _ =>
      }

      if (!setVoxel(cx, cy)) {
        return
      }
    }
  }
}
