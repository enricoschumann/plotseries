\name{plotseries}
\alias{plotseries}
\title{
  Plot a Financial Time-Series
}
\description{

  Plot a financial time-series and add annotation, such as total return.
  
}
\usage{
plotseries(series, col, ...,
           labels = NA, add.labels = TRUE,
           add.returns = TRUE,
           add.dollars = TRUE,
           add.last = FALSE,
           labels.cex = 1, log.scale = FALSE,
           ylab = "", ylim = NULL,
           main = "", main.cex = 0.7, main.col = grey(0.5),
           add0 = FALSE, add1 = TRUE,
           y.axis = TRUE,
           time.axis = TRUE, time.labels.at = NULL,
           time.labels.format = NULL, time.labels = TRUE,
           time.grid = TRUE, time.grid.at = NULL,
           add.yearly.grid = FALSE,
           mar = c(1.25, 4, 1.25, 4.5),
           white.underlay = FALSE, lwd = 1,
           font.family = "Gentium Plus", arrow = "\u2192",
           currency = "USD", percent = "\%",
           big.mark = "'", bm = NULL, show.returns = FALSE,
           do.scale1 = FALSE, xpd.hlines = FALSE, xpd.vlines = FALSE,
           returns.period = "ann")
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
  \item{white.underlay}{
    logical
  }
  \item{lwd}{
    logical
}
  \item{font.family}{
    logical
}
  \item{arrow}{
    character
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
}
\details{

  The function calls \code{\link{plot}}. Its is very
  experimental; its argument list is not stable.
  
  

  
}
\value{
  Called for its side effect.
}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
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
