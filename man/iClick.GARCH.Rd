\name{iClick.GARCH}
\alias{iClick.GARCH}

\title{
iClick Output GUI for Univariate GARCH Models
}


\encoding{latin1}


\description{
This GUI makes GARCH estimation of comparison easy. With a pre-selected GARCH type, it automatically fits eight probability distributions and conducts all diagnostic tests with a Click.

}
\usage{
iClick.GARCH(dat, meanEQ = meanEQ, garchEQ = garchEQ, n.ahead = 15)
}

\arguments{
  \item{dat}{
dat has two forms:(1) It may be a 2-column data frame, with the 1-st column is the date string, and the 2nd column is the numeric return series. (2) It can also be created as a ts() object as a none daily time series. However, the ts object may not be suitable for some financial time series returns plot, for example, drawdown.
}
  \item{meanEQ}{
Specification of mean equation.
}
  \item{garchEQ}{
Specification of variance equation.
}
  \item{n.ahead}{
Number of out-of-sample forecasting period.
}
}
\details{
This GUI fits 8 distributions for univariate GARCH with pre-selected GARCH types, and returns a 54-button GUI output. The outputs can be individually saved as .RData file for later use, the last row is the save button.The saved filename is automatically generated once clicked, in addition, corresponding .csv files will be generated also.\cr
The 54-button GUI is divided into 9 panes, and the last pane collects coefficient outputs and diagnostic tests together, which aims to make estimation comparison easy.
}
\value{
Fitted GARCH regression output.
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}

\seealso{
library(rugarch)
}


\examples{

##==External data
#data("returnsDaily24")
#y=returnsDaily24[,c(1,5)]

##== Simulation data
dat=rnorm(200,5,1)
y=ts(dat, start = c(1970, 1), frequency = 12)

meanEQ=list(AR=1,MA=0,Exo=NULL, autoFitArma=FALSE,arfimaDiff=FALSE,archM=FALSE)
  # If there are external regressors X, put them as Exo=X
  # autoFitArma=TRUE, If you want to fit arma automatically.
  # arfimaDiff=TRUE,to take ARFIMA difference
  # archM=TRUE, to estimate GARCH-in-mean

garchEQ=list(Type="sGARCH",P=1,Q=1, exo=NULL)
  # Type: "sGARCH","eGARCH","gjrGARCH","iGARCH","apGARCH"
  # please check rugarch for details.
  # P is the ARCH order
  # Q is the GARCH order

#iClick.GARCH(y,meanEQ, garchEQ,  n.ahead=15)
# This computation takes more than 6 seconds, hence I added a # to block it.

}
