\name{iClick.VisOneReturns}
\alias{iClick.VisOneReturns}

\title{
Visualize Asset Returns
}

\encoding{latin1}


\description{
This GUI conducts plots of daily asset returns, including ACF, PACF, drawdowns, and Talyor effects.
}
\usage{
iClick.VisOneReturns(dat)
}

\arguments{
  \item{dat}{
Time series object,xts.
}
}
\details{
This GUI is designed for financial time series, maily daily stock returns. Other time series data works also, as long as it has a date column.
}
\value{
Output GUI
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University
}


\examples{
data("world20")
y=na.omit(diff(log(world20[,1])))

## Simulation data
#dat=rnorm(200,5,1)
#y=ts(dat, start = c(1970, 1), frequency = 12)
iClick.VisOneReturns(y)

}
