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

\method{plotseries}{default}(series, t, col, log.scale = FALSE,
           ##
           labels = NA, labels.show = TRUE, labels.cex = 0.75,
           labels.pos = 4, labels.col = NULL, labels.at = NULL,
           labels.at.offset = NULL, labels.min.height = 0.05,
           ##
           returns.show = TRUE, returns.period = "ann",
           ##
           dollars.show = FALSE, dollars.arrow = "\u2192",
           dollars.currency = "USD",
           ##
           last.show = FALSE, last.format = NULL,
           ##
           ylab = "", ylim = NULL, lwd = 1, type = "l",
           ##
           main = "", main.cex = 0.7, main.col = grey(0.5),
           y.axis = TRUE, y.axis.pos = "left",
           y.grid = TRUE, y.grid.at = NULL, y.grid.col = grey(0.8),
           y.labels = TRUE, y.labels.at = NULL, y.labels.at.add = 1,
           y.labels.at.remove = 0,
           ##
           time.axis = TRUE, time.grid = TRUE,
           time.grid.at = NULL, time.grid.col = grey(0.8),
           time.labels = TRUE,
           time.labels.at = NULL, time.labels.format = NULL,
           add.yearly.grid = FALSE,
           ##
           par.list = list(),
           reset.par = TRUE,
           axis.cex = 1, axis.col = grey(0.5),
           white.underlay = FALSE, white.underlay.width = 2,
           font.family = "", colon = ": ", percent = "\%", big.mark = "'",
           bm = NULL, bm.returns = FALSE,
           xpd.hlines = FALSE, xpd.vlines = FALSE, series.type = "level",
           lines = FALSE, do.scale1 = series.type != "streaks", probs = NULL,
           ##
           streaks.up = 0.2, streaks.down = -streaks.up, streaks.vlines = FALSE,
           streaks.relative = TRUE, streaks.up.labels.y.mult = 1.1,
           streaks.up.labels.pos = NULL, streaks.up.labels.srt = 90,
           streaks.up.labels.col = NULL,
           streaks.up.labels.cex = 0.6,
           ##
           streaks.down.labels.y.mult = if (streaks.relative) 0.9 else 1.1,
           streaks.down.labels.pos = NULL, streaks.down.labels.srt = 90,
           streaks.down.labels.col = NULL,
           streaks.down.labels.cex = streaks.up.labels.cex,
           ##
           median.show = TRUE, median.col = grey(0.4),
           \dots)

\method{plotseries}{zoo}(series, \dots, bm = NULL)

}
\arguments{
  \item{series}{%
    \code{numeric} or a zoo series; may be a matrix, with
    series arranged in columns
  }
  \item{t}{
    timestamp, typically of class \code{\link{Date}}
  }
  \item{series.type}{

    string: \sQuote{\code{level}} (the default),
    \sQuote{\code{quantile}}, \sQuote{\code{fan}}, or
    \sQuote{\code{streaks}}

  }

  \item{col}{
    character: colors
  }
  \item{\dots}{
    other parameters
  }
  \item{lines}{%
    logical: if \code{TRUE}, add to existing plot
  }

  Customizing labels:
  \item{labels}{
    character vector
  }
  \item{labels.show}{%

    logical: a vector with as many elements as \code{series} has
    columms, recycled if necessary

  }
  \item{labels.at}{%
    an optional numeric vector to provide the
    vertical positions of the labels
  }
  \item{labels.at.offset}{%
    an optional numeric vector to provide the
    vertical positions \emph{offsets} of the labels, in particular
    for shifting overlapping labels
  }
  \item{labels.min.height}{%
    numeric
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


  \item{last.show}{
    logical
}
  \item{last.format}{
    string
}

  \item{log.scale}{
    logical
  }
    \item{y.labels}{
      logical or character
}
  \item{y.labels.at}{
    numeric
}
\item{y.labels.at.add}{
  numeric
}
\item{y.labels.at.remove}{
    numeric
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
  \item{y.axis}{
    logical
  }
  \item{y.axis.pos}{
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
  \item{time.grid.col}{
    a colour
  }
  \item{add.yearly.grid}{
    logical
  }
  \item{y.grid}{
    logical
  }
  \item{y.grid.at}{
    numeric
  }
  \item{y.grid.col}{
    a colour
  }

  \item{white.underlay}{
    logical
  }
  \item{white.underlay.width}{
    numeric
  }
  \item{axis.cex}{
    numeric
  }
  \item{axis.col}{
    a colour (see \code{\link{colours}})
  }
  \item{lwd}{
    numeric; see \code{\link{plot}}
  }
  \item{type}{
    numeric; see \code{\link{plot}}
  }
  \item{font.family}{
    logical
  }
  \item{dollars.show}{
    logical
  }
  \item{dollars.arrow}{
    character. A safe choice (because it is \acronym{ASCII}) is \code{->}.
  }
  \item{dollars.currency}{%
    string
  }
  \item{par.list}{
    a list with named elements, such as
    \code{list(las = 1, mar = c(3, 3, 3, 3))}
  }
  \item{reset.par}{%
    logical: if TRUE, all changed settings for
    \code{\link{par}} are restored
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
  \item{bm.returns}{
    logical
  }
  \item{do.scale1}{
    logical
  }
  \item{probs}{
    a vector probabilities, used for fan and quantile plots
}
  \item{streaks.up}{
    numeric
}
  \item{streaks.down}{
    numeric
}
  \item{streaks.vlines}{
    logical
}
  \item{streaks.relative}{
    logical
}
  \item{streaks.up.labels.y.mult,streaks.down.labels.y.mult}{
    numeric
}
  \item{streaks.up.labels.pos}{
    numeric
}
  \item{streaks.up.labels.srt}{
    numeric
}
  \item{streaks.up.labels.col}{
    a colour
  }
  \item{streaks.up.labels.cex,streaks.down.labels.cex}{
    relative character size for streaks labels; default is 0.6
  }
  \item{streaks.down.labels.pos}{
    numeric
  }
  \item{streaks.down.labels.srt}{
    numeric
}
  \item{streaks.down.labels.col}{
    a colour
}
  \item{xpd.hlines}{
    logical
}
  \item{xpd.vlines}{
    logical
  }

  Customizing returns:
  \item{returns.show}{
    logical
  }

  \item{returns.period}{

    character: default is \code{"ann"}; but
    \code{"itd"} (inception to date) may be useful as
    well

  }
  \item{colon}{
    character: the separator between series name, and return or last value
  }

  Customizing medians (only for fan and quantile plot):
  \item{median.show}{
    logical
  }
  \item{median.col}{
    a valid colour specification
  }

}
\details{

  The function builds on \code{\link{plot}} and is
  experimental; its argument list is not stable. It is
  strongly suggested that you pass arguments by name,
  not by position.


  \subsection{Plot types}{

    \itemize{
      \item{level} the default type, a simple line plot
      \item{streaks} see
      \url{http://enricoschumann.net/R/packages/PMwR/manual/PMwR.html#orgf6d2a94}
      \item{quantile}{requires a matrix of series:
      plots quantiles of the series. See the vignette.}
      \item{fan}{requires a matrix of series:
      plots series in shades of \code{\link{grey}}. See the vignette.}
      }
  }


  \subsection{Settings to be passed with \code{\link{par}}}{

    \code{plotseries} applies the following default settings via
    \code{\link{par}}:
\preformatted{
        list(las = 1,
             bty = "n",
             mar = c(1.25, 4, 1.25, 4.5),
             tck = 0.01,
             ps = 9.5,
             mgp = c(2, 0.25, 0),
             col.axis = grey(0.5))}

  These parameters can be overwritten by specifying
  \code{par.list}.

}

}
\value{
  Called for its side effect.
}
\references{


  Gilli, M., Maringer, D. and Schumann, E. (2019) \emph{Numerical Methods
    and Optimization in Finance}. 2nd edition. Elsevier.
  \doi{10.1016/C2017-0-01621-X}

  Schumann, E. (2023) \emph{Portfolio Management
    with R}. \url{http://enricoschumann.net/PMwR/}

  Tufte, E.R.(2001) \emph{The Visual Display of Quantitative
  Information}. 2nd edition. Graphics Press. (chapter 6, in particular)

}
\author{
  Enrico Schumann
}
\section{Warning}{
  The function is not stable yet.
}
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
