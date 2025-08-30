\name{drawupPlotX}
\alias{drawupPlotX}

\title{
Drawup Returns Plots
}
\description{
This function is within the iClick GUI, is executed within iClick.VisOneReturns(dat), the data frame dat has two columns, the first column is date index and the second one is numeric time series data.
}
\usage{
drawupPlotX(x, labels = TRUE, type = "l", col = "indianred2",
title = TRUE, ylab = "Up Returns", grid = TRUE, box = TRUE,
rug = TRUE)
}

\arguments{
  \item{x}{
A timeSeries object, single time series returns.
}
  \item{labels}{
Whether to generate label for the graph.
}
  \item{type}{
Type of line.
}
  \item{col}{
Options for color.
}
  \item{title}{
Whether to generate title for the graph.
}
  \item{ylab}{
String for Y axis.
}
  \item{grid}{
Whether to use grid in plot.
}
  \item{box}{
Whether to put the plot into a box.
}
  \item{rug}{
Whether to add rug.
}
}
\details{
This function is an internal function of iClick GUI, which is executed on iClick.VisOneReturns GUI.
}
\value{
Plot
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University
}

\seealso{
Functions in fBasic and fAssets.
}



