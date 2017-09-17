plotSiberObject<- function (siber, iso.order = c(1, 2), ax.pad = 1, hulls = TRUE, 
          community.hulls.args = NULL, ellipses = TRUE, group.ellipses.args = NULL, 
          group.hulls = FALSE, group.hulls.args = NULL, bty = "L", 
          xlab = "Isotope 1", ylab = "Isotope 2", las = 1, x.limits = NULL, 
          y.limits = NULL, points.order = 1:25, ...) 
{
  if (length(points.order) < siber$n.communities) {
    points.order = (1:25)
    warning(strwrap("Your specified vector of point types to use does not \\n                    contain enough entries to plot each of the communites. \\n                    Your chosen vector has been ignored, and the default \\n                    sequence 1:25 has been used in its place.", 
                    width = 1000))
  }
  x <- iso.order[1]
  y <- iso.order[2]
  with(siber, {
    if (any(is.null(x.limits), is.null(y.limits))) {
      plot(0, 0, type = "n", xlim = c(siber$iso.summary["min", 
                                                        x] - ax.pad, siber$iso.summary["max", x] + ax.pad), 
           ylim = c(siber$iso.summary["min", y] - ax.pad, 
                    siber$iso.summary["max", y] + ax.pad), ylab = ylab, 
           xlab = xlab, bty = bty, las = las)
    }
    else {
      plot(0, 0, type = "n", ylab = ylab, xlab = xlab, 
           bty = bty, las = las, xlim = x.limits, ylim = y.limits, 
           ...)
    }
    # group = stage, community = species #
    for (i in 1:siber$n.communities) {
      points(siber$raw.data[[i]][, x], siber$raw.data[[i]][, 
                                                           y], col = siber$raw.data[[i]]$group, pch = points.order[i], cex = 1.5)
    }
    if (hulls) {
      plotCommunityHulls(siber, community.hulls.args, iso.order, lwd = 1.3)
    }
    if (ellipses) {
      plotGroupEllipses(siber, group.ellipses.args, iso.order, lwd = 1.3)
    }
    if (group.hulls) {
      plotGroupHulls(siber, group.hull.args, iso.order, lwd = 1.3)
    }
  })
}



