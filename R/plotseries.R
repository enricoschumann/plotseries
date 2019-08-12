plotseries <-
function(series, col, ...,
         labels = FALSE,
         labels.cex = 0.7,
         add.returns = TRUE,
         add.dollars = TRUE,
         add.last = FALSE,
         log.scale = FALSE,
         ylab = "",
         ylim = NULL,
         main = "",
         add1 = TRUE,
         add.yearly.grid = FALSE,
         time.axis = TRUE,
         mar = c(1.25,4,1.25,4.5),
         white.underlay = FALSE,
         lwd = 1,
         font.family = "Gentium Plus",
         arrow = "\u2192"
         ) {

    .fmt_r <- function(x)
        format(round(x*100, 1), nsmall = 1)

    if (is.null(ylim))
        ylim <- range(series, na.rm = TRUE)

    ylab <- paste(ylab, collapse = "")
    R <- returns(series, period = "ann")
    par(las = 1,
        bty = "n",
        mar = mar,
        tck = 0.01,
        family = font.family,
        ps = 9.5,
        mgp = c(2, 0.25, 0))

    plot(series[, 1], plot.type = "single",
         main = main,
         xlab = "",
         col = if (white.underlay) "white" else col[1],
         ylab = ylab,
         log = if (log.scale) "y" else "",
         xaxt = "n",
         yaxt = "n",
         lwd = if (white.underlay) lwd*2 else lwd,
         ylim = ylim, ...)

    if (add1)
        x2 <- unique(sort(c(1, setdiff(axTicks(2), 0))))
    else
        x2 <- unique(sort(c(0, setdiff(axTicks(2), 0))))

    axis(2, lwd = 0, at = x2,
         labels = format(x2, big.mark = "'"))
    abline(h = x2, lwd = 0.25, col = grey(0.8))

    x1 <- seq(as.Date("1871-1-1"),
              as.Date("2020-1-1"), by = "5 years")
    if (time.axis)
        axis.Date(1, lwd = 0, at = x1)
    abline(v = x1, lwd = 0.25, col = grey(0.8))
    if (add.yearly.grid) {
        x1 <- seq(as.Date("1999-1-1"), as.Date("2019-1-1"), by = "1 year")
        abline(v = x1, lwd = 0.25, col = grey(0.8))
    }

    lines(series[, 1],  col = col[1], ...)

    if (!is.null(dim(series)) && ncol(series) > 1)
        for (i in 2:ncol(series)) {
            if (white.underlay)
                lines(series[, i], col = "white", lwd = 2*lwd, ...)
            lines(series[, i], col = col[i], ...)
        }

    if (!isFALSE(labels)) {
        do.show <- labels != ""
        lab <- labels

        if (add.returns)
            lab <- paste0(lab, ": ", .fmt_r(R), "%")
        if (add.dollars)
            lab <- paste0(lab, "\nUSD 1 ", arrow, " ",
                          round(coredata(tail(series, 1))))
        if (add.last)
            lab <- paste0(lab, ": ",
                          paste0(round(coredata(tail(series, 1)), 1)))

        y <- coredata(tail(series, 1))
        par(xpd = TRUE)
        text(max(index(series)),
             y[do.show],
             lab[do.show], pos = 4,
             cex = labels.cex)
        par(xpd = FALSE)
    }
    invisible(NULL)
}
