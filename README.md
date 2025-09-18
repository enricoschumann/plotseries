# plotseries -- Time-Series Plots

  Plot financial time-series, such as a portfolio value or
  stock price.  The package provides a single function,
  plotseries(), that creates high-quality, informative, but
  uncluttered graphics.  Multiple series can be aggregated
  into fanplots, the code for which is based on chapter 15
  of [Numerical Methods and Optimization in Finance](https://enricoschumann.net/NMOF.htm), second
  edition, by M. Gilli, D. Maringer and E. Schumann (2019,
  ISBN:978-0128150658).  Also supported is plotting streaks,
  i.e. periods of uninterrupted up or down movement.

## Installing the package

   The latest build of the package is always available from
   [https://enricoschumann.net/R/packages/plotseries/index.htm](https://enricoschumann.net/R/packages/plotseries/index.htm).

   To install the package from within an R session, type:

    install.packages("plotseries",
                     repos = c("https://enricoschumann.net/R",
                               getOption("repos")))


Examples are in the [package
vignette](https://enricoschumann.net/R/packages/plotseries/doc/plotseries_examples.pdf).

## News, feedback and discussion

   Please send bug reports or suggestions directly to the
   package maintainer, for instance by using `bug.report`:

    library("utils")
    bug.report("[plotseries] Unexpected behaviour in function XXX",
               maintainer("plotseries"), package = "plotseries")

