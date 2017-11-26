\name{qqnormPlotX}
\alias{qqnormPlotX}
\title{
QQ Plot
}
\encoding{latin1}

\description{
This function is within the iClick GUI, is executed within iClick.VisOneReturns(dat), the data frame dat has two columns, the first column is date index and the second one is numeric time series data.
}
\usage{
qqnormPlotX(X, labels = TRUE, col = "indianred2", pch = 19,
title = TRUE, mtext = TRUE, grid = FALSE, rug = TRUE, 
scale = TRUE)
}

\arguments{
  \item{X}{
A timeSeries object, single time series returns.
}
  \item{labels}{
Whether to generate label for the graph.
}
  \item{col}{
String for color.
}
  \item{pch}{
Line options.
}
  \item{title}{
Whether to generate title for the graph.
}
  \item{mtext}{
Whether to generate main text for the graph.
}
  \item{grid}{
Whether to use grid in plot.
}
  \item{rug}{
Whether to add rug.
}
  \item{scale}{
Whether to scale the data.
}

}
\details{
This function is an internal function of iClick GUI, which is executed on iClick.VisOneReturns GUI.
}
\value{
Plot
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}

\seealso{
Functions in fBasics.
}

