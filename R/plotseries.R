plotseries <-function(series,
                      col,
                      ...,
                      labels = NA,
                      add.labels = TRUE,
                      add.returns = TRUE,
                      add.dollars = TRUE,
                      add.last = FALSE,
                      lab.fun = NULL,
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
                      y.axis.pos = "left",
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
                      colon = ": ",
                      percent = "%",
                      big.mark = "'",
                      bm = NULL,
                      show.returns = FALSE,
                      do.scale1 = FALSE,
                      xpd.hlines = FALSE,
                      xpd.vlines = FALSE,
                      returns.period = "ann",
                      series.type = "level",
                      probs = NULL
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

    if (series.type == "level") {
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
    } else if (series.type == "fan") {

        .fan.default(series,
                     t = index(series),
                     n.levels = 5,
                     probs = probs,
                     lines = FALSE,
                     log.scale = log.scale,
                     initial.value = 1,
                     add.median = TRUE)

    }
    if (main != "")
        mtext(main, 3, cex = main.cex, col = grey(0.5))

    x2 <- axTicks(2)
    if (add1)
        x2 <- unique(sort(c(1, setdiff(x2, 0))))
    else if (add0)
        x2 <- unique(sort(c(0, setdiff(x2, 1))))

    if (y.axis) {
        if (y.axis.pos %in% c("left", "both"))
            axis(2, lwd = 0, at = x2,
                 labels = format(x2, big.mark = big.mark))
        if (y.axis.pos %in% c("right", "both"))
            axis(4, lwd = 0, at = x2,
                 labels = format(x2, big.mark = big.mark))
    }
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

    if (time.grid) {
        if (xpd.vlines)
            par(xpd = TRUE)
        if (is.null(time.grid.at))
            abline(v = xx, lwd = 0.25, col = grey(0.8))
        else
            abline(v = time.grid.at, lwd = 0.25, col = grey(0.8))
        if (xpd.vlines)
            par(xpd = FALSE)
    }

    if (!is.null(time.grid.at)) {
        abline(v = time.grid.at, lwd = 0.25, col = grey(0.8))
    } else if (add.yearly.grid) {
        x1 <- seq(first_of_year(head(index(series), 1)),
                  first_of_year(tail(index(series), 1)), by = "1 year")
        abline(v = x1, lwd = 0.25, col = grey(0.8))
    }

    if (series.type == "level") {
        lines(series[, 1],  col = col[1], ...)

        if (NCOL(series) > 1)
            for (i in 2:ncol(series)) {
                if (white.underlay)
                    lines(series[, i], col = "white", lwd = 2*lwd, ...)
                lines(series[, i], col = col[i], ...)
            }
    }

    if (!isFALSE(labels)) {
        if (length(labels) == 1L && (is.na(labels) || labels = ""))
            labels <- rep(labels, NCOL(series))
        do.show <- !is.na(labels)
        lab <- labels
        lab[is.na(lab)] <- ""

        if (add.returns)
            lab <- paste0(lab, ifelse(do.show, colon, ""),
                          .fmt_r(R), "%")
        if (add.last)
            lab <- paste0(lab, colon,
                          paste0(round(coredata(tail(series, 1)), 1)))
        if (!is.null(lab.fun)) {
            if (NCOL(series) == 1)
                lab <- paste0(lab, colon, lab.fun(series))
            else
                for (i in 1:NCOL(series))
                    lab[i] <- paste0(lab[i], colon, lab.fun(series[, i]))
        }
        if (add.dollars)
            lab <- paste0(lab, "\n", currency ," 1 ", arrow, " ",
                          round(coredata(tail(series, 1))))


        if (series.type == "level") {
            y.temp <- na.locf(series)
            y <- coredata(tail(y.temp, 1))
        } else if (series.type == "fan") {
            lab <- paste("median ", .fmt_r(median(R)))  ## FIXME
            y.temp <- na.locf(series)
            y <- median(coredata(tail(y.temp, 1)))
        }

        if (any(is.na(y[do.show]))) {
            warning("NA in series: series/labels may be missing")
        }
        if (isTRUE(labels.col))
            labels.col <- col[do.show]
        if (any(do.show)) {
            par(xpd = TRUE)
            text(max(index(series)),
                 y[do.show],
                 lab[do.show],
                 pos = labels.pos,
                 cex = labels.cex,
                 col = labels.col)
            par(xpd = FALSE)
        }
    }
    invisible(NULL)
}

.fan.zoo <- function(P,
                     n.levels = 5,
                     probs = NULL,
                     lines = FALSE,
                     log.scale  = FALSE,
                     aggregate = NULL, ...) {
    if (!is.null(aggregate) && aggregate == "monthly")
        P <- aggregate(P, datetimeutils::end_of_month(index(P)),
                       tail, 1)
    .fan.default(P, t= index(P), n.levels = n.levels, probs = probs,
                 log.scale = log.scale, ...)
}

.fan.default <- function(P, t, n.levels = 5,
                         probs = NULL,
                         lines = FALSE,
                         log.scale = FALSE,
                         initial.value = 1,
                         add.median = TRUE,
                         ...) {

    if (is.finite(initial.value))
        P <- scale1(P, level = initial.value)
    if (is.null(dim) || ncol(P) == 1L)
        warning("a single series makes a slim fan")
    nt <- nrow(P)
    if (!is.null(probs)) {
        levels <- probs
    } else {
        levels <- seq(0.10, 0.25, length.out = n.levels)
    }
    greys  <- seq(0.7,  0.50, length.out = length(levels))

    args <- list(...)

    if (!lines) {
        par(las = 1,
            bty = "n",
            mar = c(1.25,4,1.25,4.5),
            tck = 0.01,
            ps = 9.5,
            mgp = c(2, 0.25, 0),
            col.axis = grey(.5),
            cex.axis = 1)

        plot(t, rep(100, nt),
             ylim = range(P),
             xlab = "",
             ylab = "",
             lty = 0,
             type = "n",
             log = if (log.scale) "y" else "",
             xaxt = "n",
             yaxt = "n")

    }

    for (level in levels) {

        l <- apply(P, 1, quantile, level)
        u <- apply(P, 1, quantile, 1 - level)
        col <- grey(greys[level == levels])
        polygon(c(t, rev(t)), c(l, rev(u)),
                col = col, border = NA)
    }
    lines(t, apply(P, 1, median))
    invisible(NULL)
}

