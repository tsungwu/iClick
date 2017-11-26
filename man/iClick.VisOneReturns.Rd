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
dat has two forms:(1) It may be a 2-column data frame, with the 1-st column is the date string, and the 2nd column is the numeric return series. (2) It can also be created as a ts() object as a none daily time series. However, the ts object may not be suitable for some financial time series returns plot, for example, drawdown.
}
}
\details{
This GUI is designed for financial time series, maily daily stock returns. Other time series data works also, as long as it has a date column.
}
\value{
Output GUI
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}


\examples{
##== External data
#data("returnsDaily24")
#y=returnsDaily24[,c(1,10)]  #Select the fifth variable

##== Simulation data
dat=rnorm(100,5,1)
y=ts(dat, start = c(1970, 1), frequency = 12)
iClick.VisOneReturns(y)

}
