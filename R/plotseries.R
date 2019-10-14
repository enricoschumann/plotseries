plotseries <-function(series,
                      col,
                      ...,
                      labels = NA,
                      add.labels = TRUE,
                      add.returns = TRUE,
                      add.dollars = TRUE,
                      add.last = FALSE,
                      labels.cex = 0.7,
                      labels.pos = 4,
                      labels.col = NULL,
                      log.scale = FALSE,
                      ylab = "",
                      ylim = NULL,
                      lwd = 1,
                      main = "",
                      main.cex = 0.7,
                      main.col = grey(0.5),
                      add0 = FALSE,
                      add1 = TRUE,
                      y.axis = TRUE,
                      time.axis = TRUE,
                      time.labels.at = NULL,
                      time.labels.format = NULL,
                      time.labels = TRUE,
                      time.grid = TRUE,
                      time.grid.at = NULL,
                      add.yearly.grid = FALSE,
                      mar = c(1.25,4,1.25,4.5),
                      cex.axis = 1,
                      white.underlay = FALSE,
                      font.family = "Gentium Plus",
                      arrow = "\u2192",
                      currency = "USD",
                      percent = "%",
                      big.mark = "'",
                      bm = NULL,
                      show.returns = FALSE,
                      do.scale1 = FALSE,
                      xpd.hlines = FALSE,
                      xpd.vlines = FALSE,
                      returns.period = "ann",
                      series.type = "level"
                      ) {

    .fmt_r <- function(x)
        format(round(x*100, 1), nsmall = 1)

    ylab <- paste(ylab, collapse = "")

    R <- returns(series, period = returns.period)
    if (!is.null(bm)) {
        R.bm <- returns(bm, period = returns.period)
        R <- R - R.bm
        series <- scale1(series/bm)
        if (show.returns)
            series <- 100*(series - 1)
    }

    if (is.null(ylim))
        ylim <- range(series, na.rm = TRUE)

    if (is.character(time.grid.at) &&
        grep("year", time.grid.at, ignore.case = TRUE)) {

        n <- as.numeric(strsplit(time.grid.at, " +")[[1]][1])
        time.grid.at <- first_of_year(seq(index(series)[1],
                                          tail(index(series),1),
                                          by = paste(time.grid.at)))
    }

    par(las = 1,
        bty = "n",
        mar = mar,
        tck = 0.01,
        family = font.family,
        ps = 9.5,
        mgp = c(2, 0.25, 0),
        col.axis = grey(.5),
        cex.axis = cex.axis)

    plot(series[, 1L],
         plot.type = "single",
         main = "",
         xlab = "",
         col = if (white.underlay) "white" else col[1L],
         ylab = ylab,
         log = if (log.scale) "y" else "",
         xaxt = "n",
         yaxt = "n",
         lwd = if (white.underlay) lwd*2 else lwd,
         ylim = ylim, ...)
    mtext(main, 3, cex = main.cex, col = grey(0.5))


    x2 <- axTicks(2)
    if (add1)
        x2 <- unique(sort(c(1, setdiff(x2, 0))))
    else if (add0)
        x2 <- unique(sort(c(0, setdiff(x2, 1))))

    if (y.axis)
        axis(2, lwd = 0, at = x2,
             labels = format(x2, big.mark = big.mark))

    if (xpd.hlines)
        par(xpd = TRUE)
    abline(h = x2, lwd = 0.25, col = grey(0.8))
    if (xpd.hlines)
        par(xpd = FALSE)


    ## x1 <- seq(as.Date("1871-1-1"),
    ##           as.Date("2020-1-1"), by = "1 years")

    if (time.axis)
        if (is.null(time.labels.at))
            xx <- axis.Date(1, lwd = 0, x = index(series))
        else {
            if (is.null(time.labels.format))
                axis.Date(1, lwd = 0, at = time.labels.at)
            else
                axis.Date(1, lwd = 0,
                          at = time.labels.at,
                          format = time.labels.format,
                          labels = time.labels)
        }
    else
        xx <- axis.Date(1, lwd = 0, x = index(series),
                        labels = FALSE)

    if (xpd.vlines)
        par(xpd = TRUE)
    if (time.grid) {
        if (is.null(time.grid.at))
            abline(v = xx, lwd = 0.25, col = grey(0.8))
        else
            abline(v = time.grid.at, lwd = 0.25, col = grey(0.8))
    }
    if (xpd.vlines)
        par(xpd = FALSE)

    if (!is.null(time.grid.at)) {
        abline(v = time.grid.at, lwd = 0.25, col = grey(0.8))
    } else if (add.yearly.grid) {
        x1 <- seq(first_of_year(head(index(series), 1)),
                  first_of_year(tail(index(series), 1)), by = "1 year")
        abline(v = x1, lwd = 0.25, col = grey(0.8))
    }

    lines(series[, 1],  col = col[1], ...)

    if (!is.null(dim(series)) && ncol(series) > 1)
        for (i in 2:ncol(series)) {
            if (white.underlay)
                lines(series[, i], col = "white", lwd = 2*lwd, ...)
            lines(series[, i], col = col[i], ...)
        }

    if (FALSE) {
        lab <- paste0(.fmt_r(R))
        y.temp <- na.locf(series)
        y <- coredata(tail(y.temp, 1))
        if (any(is.na(y))) {
            warning("NA in series: series/labels may be missing")
        }
        par(xpd = TRUE)
        text(max(index(series)),
             y,
             lab, pos = labels.pos,
             cex = labels.cex)
        par(xpd = FALSE)

    } else if (!isFALSE(labels)) {
        do.show <- labels != ""
        lab <- labels

        if (add.returns)
            lab <- paste0(lab, ifelse(labels != " ", ": ", ""),
                                      .fmt_r(R), "%")
        if (add.dollars)
            lab <- paste0(lab, "\n", currency ," 1 ", arrow, " ",
                          round(coredata(tail(series, 1))))
        if (add.last)
            lab <- paste0(lab, ": ",
                          paste0(round(coredata(tail(series, 1)), 1)))

        y.temp <- na.locf(series)
        y <- coredata(tail(y.temp, 1))
        if (any(is.na(y[do.show]))) {
            warning("NA in series: series/labels may be missing")
        }
        if (isTRUE(labels.col))
            labels.col <- col[do.show]
        par(xpd = TRUE)
        text(max(index(series)),
             y[do.show],
             lab[do.show],
             pos = labels.pos,
             cex = labels.cex,
             col = labels.col)
        par(xpd = FALSE)
    }
    invisible(NULL)
}
