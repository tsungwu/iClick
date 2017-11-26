\name{seriesPlotX}
\alias{seriesPlotX}

\title{
Plot Time Series Data
}

\encoding{latin1}

\description{
This function is within the iClick GUI, is executed within iClick.VisOneReturns(dat), the data frame dat has two columns, the first column is date index and the second one is numeric time series data.
}
\usage{seriesPlotX(x,labels=TRUE,type="l",col="indianred2",
ylab="Value", title=TRUE, grid=TRUE, box=TRUE, rug=TRUE)
}

\arguments{
  \item{x}{
A timeSeries object, single time series returns.
}
  \item{labels}{
Whether to generate label for the graph.
}
  \item{type}{
Type of graph.
}
  \item{col}{
Options for color.
}
  \item{ylab}{
String label for Y axis.
}
  \item{title}{
Whether to generate title for the graph.
}
  \item{grid}{
Whether to generate grid for the graph.
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
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}

\seealso{
fBasics
}


