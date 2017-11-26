\name{cutAndStack}
\alias{cutAndStack}

\title{
Cut and Stack Plotting Function
}

\encoding{latin1}

\description{
This function calls cut() to cut tiimeseries into several equal periods and plots over time.
}
\usage{
cutAndStack(x, number, overlap = 0.1, type = "l", xlab = "Time", 
ylab = deparse(substitute(x)))
}
\arguments{
  \item{x}{
A timeSeries object, single time series price.
}
  \item{number}{
Number of equal cut.
}
  \item{overlap}{
Percentage of overlapping across cuts.
}
  \item{type}{
Type of line.
}
  \item{xlab}{
Label of X axis.
}
  \item{ylab}{
Label of Y axis.
}

}
\details{
This function is within the iClick GUI, is executed within iClick.VisAssetprice().
}
\value{
Plot
}

\author{
Ho Tsungwu <tsungwu@mail.shu.edu.tw>
}

