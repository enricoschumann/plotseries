\name{plotseries}
\alias{plotseries}
\alias{plotseries.default}
\alias{plotseries.zoo}
\title{
  Plot a Financial Time-Series
}
\description{

  Plot a financial time-series and add annotation, such as total return.

}
\usage{
plotseries(series, ...)

\method{plotseries}{default}(series, t,
    col, log.scale = FALSE,
    labels = NA, labels.show = TRUE, labels.cex = 0.75,
    labels.pos = 4, labels.col = NULL,
    labels.at = NULL, labels.min.height = 0.05,
    returns.show = TRUE, returns.period = "ann",
    dollars.show = FALSE, dollars.arrow = "\u2192", dollars.currency = "USD",
    last.show = FALSE,
    ylab = "", ylim = NULL, lwd = 1,
    main = "", main.cex = 0.7, main.col = grey(0.5),
    y.axis = TRUE, y.axis.pos = "left",
    y.grid = TRUE, y.grid.at = NULL, y.grid.col = grey(0.8),
    y.labels = TRUE, y.labels.at = NULL,
    y.labels.at.add = 1, y.labels.at.remove = 0,
    time.axis = TRUE, time.grid = TRUE,
    time.grid.at = NULL, time.grid.col = grey(0.8),
    time.labels = TRUE, time.labels.at = NULL,
    time.labels.format = NULL,
    mar = c(1.25, 4, 1.25, 4.5), mgp = c(2, 0.25, 0),
    axis.cex = 1, axis.col = grey(0.5),
    white.underlay = FALSE, white.underlay.width = 2,
    font.family = "",
    colon = ": ", percent = "\%", big.mark = "'",
    bm = NULL,
    xpd.hlines = FALSE, xpd.vlines = FALSE,
    series.type = "level", lines = FALSE,
    probs = NULL, streaks.up = 0.2, streaks.down = -0.2, \dots)

\method{plotseries}{zoo}(series, \dots)

}
\arguments{
  \item{series}{
    a zoo series
  }
  \item{col}{
    character: colors
  }
  \item{\dots}{
    other parameters
  }
  \item{labels}{
    character
  }
  \item{add.labels}{

    logical: a vector with as many elements as series has columms

  }
  \item{add.returns}{
    logical
}
  \item{add.dollars}{
    logical
}
  \item{add.last}{
    logical
}
  \item{labels.cex}{
    numeric
}
  \item{labels.pos}{
    numeric
}
  \item{labels.col}{
    \code{NULL}, \code{TRUE} or character
}
  \item{log.scale}{
    logical
}
  \item{ylab}{
    logical
}
  \item{ylim}{
    numeric
}
  \item{main}{
    a string
}
  \item{main.cex}{
    numeric
}
  \item{main.col}{
    character
}
  \item{add0}{
    logical
}
  \item{add1}{
    logical
}
  \item{y.axis}{
    logical
  }
  \item{time.axis}{
    logical
}
  \item{time.labels.at}{
..
}
  \item{time.labels.format}{
    ..
}
  \item{time.labels}{
    ..
}
  \item{time.grid}{
    logical
}
  \item{time.grid.at}{
    ..
}
  \item{add.yearly.grid}{
    ..
}
  \item{mar}{
    ..
}
  \item{cex.axis}{
    numeric
}
\item{white.underlay}{
    logical
  }
  \item{lwd}{
    logical
}
  \item{font.family}{
    logical
}
  \item{dollars.arrow}{
    character. A safe choice (because it is \acronym{ASCII} is \code{->}.
}
  \item{currency}{
    character
}
  \item{percent}{
    character
}
  \item{big.mark}{
    character
}
  \item{bm}{
    a zoo series
}
  \item{show.returns}{
    logical
}
  \item{do.scale1}{
    logical
}
  \item{xpd.hlines}{
    logical
}
  \item{xpd.vlines}{
    logical
}
  \item{returns.period}{

    character: default is \code{"ann"}; but
    \code{"itd"} (inception to date) may be useful as
    well

  }
  \item{series.type}{

    string: \sQuote{\code{level}} (the default),
    \sQuote{\code{quantile}}, \sQuote{\code{fan}}, or
    \sQuote{\code{streaks}}

  }
}
\details{

  The function calls \code{\link{plot}}. It is very
  experimental; its argument list is not stable.




}
\value{
  Called for its side effect.
}
\author{
  Enrico Schumann
}
\section{Warning}{The function is not stable yet.}
\seealso{

  \code{\link{plot}}

}
\examples{
\donttest{
series <- French("~/Downloads/French/",
                   "10_Portfolios_Prior_12_2_CSV.zip",
                   frequency = "monthly",
                   weighting = "value",
                   price.series = TRUE)
series <- zoo(series, as.Date(row.names(series)))

plotseries(series,
           col = hcl.colors(n = 30, palette = "Greens"),
           add.dollars = FALSE,
           log.scale = TRUE,
           labels = colnames(series))
}
}
\keyword{hplot}
