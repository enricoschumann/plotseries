## -*- truncate-lines: t; -*-
## Copyright (C) 2019-22  Enrico Schumann

plotseries <- function(series, ...) {
    UseMethod("plotseries")
}

plotseries.zoo <- function(series, ...) {
    t <- index(series)
    series <- coredata(series)
    plotseries.default(series, t = t, ...)
}

.fmt_r <- function(x)
    format(round(x*100, 1), nsmall = 1)

plotseries.default <-
function(series,
         t,
         col,
         log.scale = FALSE,
         labels = NA,
         labels.show = TRUE,
         labels.cex = 0.75,
         labels.pos = 4,
         labels.col = NULL,
         labels.at = NULL,
         labels.at.offset = NULL,
         labels.min.height = 0.05,

         returns.show = TRUE,
         returns.period = "ann",

         dollars.show = FALSE,
         dollars.arrow = "\u2192",
         dollars.currency = "USD",

         last.show = FALSE,
         last.format = NULL,



         ylab = "",
         ylim = NULL,

         lwd = 1,
         type = "l",

         main = "",
         main.cex = 0.7,
         main.col = grey(0.5),

         y.axis = TRUE,
         y.axis.pos = "left",

         y.grid = TRUE,
         y.grid.at = NULL,
         y.grid.col = grey(0.8),

         y.labels = TRUE,
         y.labels.at = NULL,
         y.labels.at.add = 1,
         y.labels.at.remove = 0,

         time.axis = TRUE,

         time.grid = TRUE,
         time.grid.at = NULL,
         time.grid.col = grey(0.8),

         time.labels = TRUE,
         time.labels.at = NULL,
         time.labels.format = NULL,


         add.yearly.grid = FALSE,  ## remove?

         ## mar = c(1.25,4,1.25,4.5),
         ## mgp = c(2, 0.25, 0),

         par.list = list(),

         axis.cex = 1,
         axis.col = grey(0.5),

         white.underlay = FALSE,
         white.underlay.width = 2,

         ## font.family = "Gentium Plus",
         font.family = "",

         colon = ": ",
         percent = "%",
         big.mark = "'",
         bm = NULL,
         bm.returns = FALSE,

         xpd.hlines = FALSE,
         xpd.vlines = FALSE,

         series.type = "level",
         lines = FALSE,
         do.scale1 = series.type != "streaks",

         probs = NULL,
         streaks.up = 0.2,
         streaks.down = -streaks.up,
         streaks.vlines = FALSE,
         streaks.relative = TRUE,
         streaks.up.labels.y.mult = 1.1,
         streaks.up.labels.pos = NULL,
         streaks.up.labels.srt = 90,
         streaks.up.labels.col = NULL,
         streaks.up.labels.cex = 0.6,
         streaks.down.labels.y.mult = if (streaks.relative) 0.9 else 1.1,
         streaks.down.labels.pos = NULL,
         streaks.down.labels.srt = 90,
         streaks.down.labels.col = NULL,
         streaks.down.labels.cex = streaks.up.labels.cex,

         median.show = TRUE,
         median.col = grey(.4),
         ...


         ) {

    .series.types <- c("level", "streaks",
                       "fan", "quantile",
                       "drawdown", "returns")
    if (!series.type %in% .series.types) {
        stop("unknown series.type")
    }

    if (series.type == "streaks") {
         y.labels.at.add <- numeric(0)
         y.labels.at.remove <- numeric(0)
    }

    par.lst <- list(las = 1,
                    bty = "n",
                    mar = c(1.25, 4, 1.25, 4.5),
                    tck = 0.01,
                    ps = 9.5,
                    mgp = c(2, 0.25, 0),
                    col.axis = grey(0.5))

    par.lst[names(par.list)] <- par.list


    ylab <- paste(ylab, collapse = "")


    if (is.null(last.format))
        last.format <-
            function(x, ...) round(x, 1)

    if (missing(col))
        col <- rep(grey(0.4), NCOL(series))


    if (numeric.t <- inherits(try(as.Date(t[1]), silent = TRUE), "try-error")) {
        returns.period <- "total"
    }
    R <- returns(series, t = t, period = returns.period)
    if (!is.null(bm)) {
        bm <- coredata(bm)
        R.bm <- returns(bm, t = t, period = returns.period)
        R <- R - R.bm
        if (do.scale1)
            series <- scale1(series/bm)
        if (bm.returns)
            series <- 100*(series - 1)
    }


    if (is.character(time.grid.at) &&
        grep("year", time.grid.at, ignore.case = TRUE)) {

        n <- as.numeric(strsplit(time.grid.at, " +")[[1]][1])
        time.grid.at <- first_of_year(seq(index(series)[1],
                                          tail(index(series),1),
                                          by = paste(time.grid.at)))
    }

    do.call(par, par.lst)

    series <- as.matrix(series)
    if (series.type == "level") {

        if (is.null(ylim))
            ylim <- range(series, na.rm = TRUE)

        if (!lines)
            plot(t,
                 series[, 1L],
                 type = type,
                 main = "",
                 xlab = "",
                 col = if (white.underlay) "white" else col[1L],
                 ylab = ylab,
                 log = if (log.scale) "y" else "",
                 xaxt = "n",
                 yaxt = "n",
                 lwd = if (white.underlay) lwd*white.underlay.width else lwd,
                 ylim = ylim, ...)

    } else if (series.type == "fan") {

        if (is.null(ylim))
            ylim <- range(series, na.rm = TRUE)

        if (!lines)
            plot(t,
                 rep(100, length(t)),
                 ylim = ylim,
                 xlab = "",
                 ylab = "",
                 lty = 0,
                 type = "n",
                 log = if (log.scale) "y" else "",
                 xaxt = "n",
                 yaxt = "n")

        .fan(series,
             t = t,
             n.levels = 5,
             probs = probs,
             lines = FALSE,
             log.scale = log.scale,
             ## initial.value = 1,
             median.show = median.show,
             ...)

    } else if (series.type == "quantile") {

        if (is.null(ylim))
            ylim <- range(series, na.rm = TRUE)

        if (!lines)
            plot(t,
                 rep(100, length(t)),
                 ylim = ylim,
                 xlab = "",
                 ylab = "",
                 lty = 0,
                 type = "n",
                 log = if (log.scale) "y" else "",
                 xaxt = "n",
                 yaxt = "n")

        .quantile(series,
                  t = t,
                  probs = probs,
                  lines = FALSE,
                  log.scale = log.scale,
                  ## initial.value = 1,
                  median.show = median.show,
                  median.col = median.col,
                  ...)

    } else if (series.type == "streaks") {

        up_down <- streaks(series,
                           up = streaks.up,
                           down = streaks.down,
                           y = bm,
                           relative = streaks.relative)
        if ("change" %in% colnames(up_down))
            colnames(up_down)[colnames(up_down) == "change"] <- "return"
        if (is.null(ylim)) {
            if (streaks.relative)
                ylim <- range(100*up_down$return + 100)
            else
                ylim <- range(100*up_down$return)
            }
        if (!lines)
            plot(t,
                 rep(100, length(t)),
                 ylim = ylim,
                 xlab = "",
                 ylab = "",
                 lty = 0,
                 type = "n",
                 main = "",
                 log = if (streaks.relative) "y" else "", ## if (is.null(bm)) "y" else "", ## relative: *no* log scale
                 xaxt = "n",
                 yaxt = "n")

    } else if (series.type == "drawdown") {

    } else if (series.type == "returns") {

    }

    if (!lines) {

        if (main != "")
            mtext(main, 3, cex = main.cex, col = grey(0.5))

        x2 <- axTicks(2)
        if (length(y.labels.at.add))
            x2 <- unique(sort(c(y.labels.at.add, x2)))

        if (length(y.labels.at.remove))
            x2 <- setdiff(x2, y.labels.at.remove)

        if (y.axis) {
            y_ <- x2
            if (series.type == "streaks" && streaks.relative)
                y_ <- y_ - 100
            if (y.axis.pos %in% c("left", "both"))
                axis(2, lwd = 0, at = x2,
                     labels = format(y_, big.mark = big.mark))
            if (y.axis.pos %in% c("right", "both"))
                axis(4, lwd = 0, at = x2,
                     labels = format(y_, big.mark = big.mark))
        }

        if (y.grid) {
            if (xpd.hlines)
                par(xpd = TRUE)
            if (is.null(y.grid.at))
                abline(h = x2, lwd = 0.25, col = y.grid.col)
            else
                abline(h = y.grid.at, lwd = 0.25, col = y.grid.col)
            if (xpd.hlines)
                par(xpd = FALSE)
        }

        ## x1 <- seq(as.Date("1871-1-1"),
        ##           as.Date("2020-1-1"), by = "1 years")

        if (time.axis) {
            if (is.null(time.labels.at))
                if (numeric.t)
                    xx <- axis(1, lwd = 0)
                else
                    xx <- axis.Date(1, lwd = 0, x = t)
            else {
                if (is.null(time.labels.format))
                    axis.Date(1, lwd = 0, at = time.labels.at)
                else
                    axis.Date(1, lwd = 0,
                              at = time.labels.at,
                              format = time.labels.format,
                              labels = time.labels)
            }
        } else {
            if (numeric.t)
                xx <- axis(1, lwd = 0, labels = FALSE)
            else
                xx <- axis.Date(1, lwd = 0, x = t, labels = FALSE)

        }
        if (time.grid) {
            if (xpd.vlines)
                par(xpd = TRUE)
            if (is.null(time.grid.at))
                abline(v = xx, lwd = 0.25, col = time.grid.col)
            else
                abline(v = time.grid.at, lwd = 0.25, col = time.grid.col)
            if (xpd.vlines)
                par(xpd = FALSE)
        }

        if (!is.null(time.grid.at)) {
            abline(v = time.grid.at, lwd = 0.25, col = grey(0.8))
        } else if (add.yearly.grid) {
            x1 <- seq(first_of_year(head(t, 1)),
                      first_of_year(tail(t, 1)), by = "1 year")
            abline(v = x1, lwd = 0.25, col = grey(0.8))
        }
    }

    if (series.type == "level") {
        lines(t, series[, 1L],  col = col[1L], lwd = lwd, ...)

        if (NCOL(series) > 1)
            for (i in 2:ncol(series)) {
                if (white.underlay)
                    lines(t, series[, i],
                          col = "white",
                          lwd = white.underlay.width, ...)
                lines(t, series[, i], col = col[i],
                      lwd = if (length(lwd) > 1L) lwd[i] else lwd, ...)
            }
    } else if (series.type == "streaks") {
        .streaks(series[, 1], t = t, streaks = up_down,
                 col = col[1L], y = bm,
                 labels.show = labels.show,
                 streaks.up.labels.y.mult = streaks.up.labels.y.mult,
                 streaks.up.labels.pos    = streaks.up.labels.pos,
                 streaks.up.labels.srt    = streaks.up.labels.srt,
                 streaks.up.labels.col    = streaks.up.labels.col,
                 streaks.up.labels.cex    = streaks.up.labels.cex,
                 streaks.down.labels.y.mult = streaks.down.labels.y.mult,
                 streaks.down.labels.pos  = streaks.down.labels.pos,
                 streaks.down.labels.srt  = streaks.down.labels.srt,
                 streaks.down.labels.col  = streaks.down.labels.col,
                 streaks.down.labels.cex  = streaks.down.labels.cex,
                 streaks.relative = streaks.relative,
                 ...)
    }

    if (!isFALSE(labels)) {
        if (length(labels) == 1L && (is.na(labels) || labels == ""))
            labels <- rep(labels, NCOL(series))

        if (length(labels) > NCOL(series)) {
            warning("some labels are dropped (more labels than series)")
            labels <- labels[1:NCOL(series)]
        }
        do.show <- !is.na(labels)
        lab <- labels
        lab[is.na(lab)] <- ""

        if (returns.show)
            lab <- paste0(lab, ifelse(do.show, colon, ""),
                          .fmt_r(R), "%")
        if (last.show)
            lab <- paste0(lab, colon,
                          paste0(last.format(coredata(tail(series, 1)))))
        ## if (!is.null(lab.fun)) {
        ##     if (NCOL(series) == 1)
        ##         lab <- paste0(lab, colon, lab.fun(series))
        ##     else
        ##         for (i in 1:NCOL(series))
        ##             lab[i] <- paste0(lab[i], colon, lab.fun(series[, i]))
        ## }
        if (dollars.show)
            lab <- paste0(lab, "\n", dollars.currency ," 1 ", dollars.arrow, " ",
                          round(tail(series, 1)))


        if (series.type %in% c("level", "quantile")) {
            if (is.null(labels.at)) {
                y.temp <- na.locf(series)
                y <- tail(y.temp, 1)
            } else if (is.numeric(labels.at)) {
                y <- labels.at
            } else if (labels.at == "auto") {
                y <- .spread_labels(
                    y = drop(coredata(tail(series, 1))),
                    y.min = labels.min.height,
                    y.range = diff(par("usr")[3:4]),
                    log.scale = log.scale)
            }
            if (!is.null(labels.at.offset)) {
                y <- y + labels.at.offset
            }
            if (any(is.na(y[do.show]))) {
                warning("NA in series: series/labels may be missing")
            }
            if (isTRUE(labels.col))
                labels.col <- col[do.show]
            if (any(do.show)) {
                par(xpd = TRUE)
                text(max(t),
                     y[do.show],
                     lab[do.show],
                     pos = labels.pos,
                     cex = labels.cex,
                     col = labels.col)
                par(xpd = FALSE)
            }

        } else if (series.type == "fan") {
            lab <- paste("median ", .fmt_r(median(R)))  ## FIXME
            y.temp <- na.locf(series)
            y <- median(coredata(tail(y.temp, 1)))
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
    .fan(P, t= index(P), n.levels = n.levels, probs = probs,
                 log.scale = log.scale, ...)
}

.fan <- function(P, t, n.levels = 5,
                 probs = NULL,
                 log.scale = FALSE,
                 median.show = TRUE,
                 median.col = grey(.4),
                 ...) {

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

    for (level in levels) {
        l <- apply(P, 1, quantile, level)
        u <- apply(P, 1, quantile, 1 - level)
        col <- grey(greys[level == levels])
        polygon(c(t, rev(t)), c(l, rev(u)),
                col = col, border = NA)
    }
    if (median.show)
        lines(t, apply(P, 1, median), col = median.col)
    invisible(NULL)
}

.quantile <- function(P, t,
                      probs = NULL,
                      log.scale = FALSE,
                      median.show = TRUE,
                      median.col = grey(.4),
                      ...) {

    ## if (is.finite(initial.value))
    ##     P <- scale1(P, level = initial.value)
    if (is.null(dim) || ncol(P) == 1L)
        warning("a single series makes a slim fan")
    nt <- nrow(P)
    if (!is.null(probs)) {
        levels <- probs
    } else {
        levels <- 0.1
    }
    greys  <- seq(0.7,  0.50, length.out = length(levels))

    args <- list(...)

    for (level in levels) {
        l <- apply(P, 1, quantile, level)
        u <- apply(P, 1, quantile, 1 - level)
        col <- grey(greys[level == levels])
        lines(t, l, col = col)
        lines(t, u, col = col)
    }
    if (median.show)
        lines(t, apply(P, 1, median), col = median.col)
    invisible(NULL)
}

.streaks <-
function(x,
         t,
         streaks,
         col,
         y,
         labels.show,
         streaks.up.labels.y.mult,
         streaks.up.labels.pos,
         streaks.up.labels.srt,
         streaks.up.labels.col,
         streaks.up.labels.cex,
         streaks.down.labels.y.mult,
         streaks.down.labels.pos,
         streaks.down.labels.srt,
         streaks.down.labels.col,
         streaks.down.labels.cex,
         streaks.relative,
         ...) {

    for (i in seq_len(nrow(streaks))) {
        tt <- seq(streaks$start[i], streaks$end[i])

        ## if (unclass(t[streaks$end[i]] - t[streaks$start[i]]) > 700)
        ##     abline(v = c(t[streaks$start[i]],
        ##                  t[streaks$end[i]]), lty = 3,
        ##            lwd = 0.25)
        ## labels.up.mult <- 1.1
        ## labels.down.mult <- 0.9
        if (streaks.relative)
            v <- 100*scale1(x[tt]) / if (is.null(y)) 1 else scale1(y[tt])
        else
            v <- 100*(x[tt] - x[tt][1])
        lines(t[tt], v, col = col)
        if (labels.show) {
            par(xpd = TRUE)
            if (streaks$return[i] > 0)
                text(max(t[tt]),
                     max(v)*streaks.up.labels.y.mult,
                     .fmt_r(streaks$return[i]),
                     srt = streaks.up.labels.srt,
                     pos = streaks.up.labels.pos,
                     col = streaks.up.labels.col,
                     cex = streaks.up.labels.cex)
            else
                text(max(t[tt]),
                     min(v)*streaks.down.labels.y.mult,
                     .fmt_r(streaks$return[i]),
                     srt = streaks.down.labels.srt,
                     pos = streaks.down.labels.pos,
                     col = streaks.down.labels.col,
                     cex = streaks.down.labels.cex)
            par(xpd = FALSE)
        }
    }

}

.spread_labels <- function(y, x = NULL,
                           y.min, x.min = NULL,
                           y.range, x.range = NULL,
                           log.scale = FALSE) {

    y.original <- y

    if (log.scale)
        y <- log(y, 10)
    h <- y.min*y.range/2

    ii <- order(y, decreasing = TRUE)
    y <- sort(y, decreasing = TRUE)
    y0 <- y

    .overlap <- function(y, y0, h) {
        yy <- y[-length(y)] - y[-1] - 2*h
        sum(abs(y - y0)) - sum(yy - abs(yy))/2
    }

    if (.overlap(y, y0, h) == 0)
        return(y.original)

    nb <- neighbours::neighbourfun(type = "numeric",
                                   stepsize = 0.01,
                                   n = length(y),
                                   min = rep(y.min - y.range*0.1, length(y)),
                                   max = rep(y.min + y.range*1.1, length(y)))

    sol <- NMOF::LSopt(.overlap,
                       list(x0 = y0,
                            neighbour = nb,
                            nI = 500),
                       y0 = y0, h = h)
    ans <- numeric(length(y))
    ans[ii] <- sol$xbest
    if (log.scale)
        ans <- 10^ans
    ans
}

