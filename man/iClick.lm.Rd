\name{iClick.lm}
\alias{iClick.lm}

\title{
iClick GUI for linear model
}

\encoding{latin1}


\description{
This GUI estimates ARIMA both with automatic lag selection and fixed lag length. The GUI is only only a GUI, but also a output format.
}
\usage{
iClick.lm(dep,indep,data,Formula=NULL,bootrep=99)
}

\arguments{
  \item{data}{A R data object for lm()
}
  \item{dep}{scalar, the number of column as dependent variable
}
  \item{indep}{
scalar, the numbers of column as independent variables
}
  \item{Formula}{A formula for lm, default is NULL, if specified, dep and indep should leave empty. See example below}
  \item{bootrep}{Bootstrap replications, default is 99}
}
\details{
This GUI fits equaiton into lm regression.
}
\value{
Fitted lm regression output.
}

\author{
Ho Tsung-wu <tsungwu@mail.shu.edu.tw>
}

\seealso{
lm()
}
\examples{
data("FFplusMOM")
iClick.lm(dep=2,indep=c(3,5:6),data=FFplusMOM, bootrep=9)

#Eq=RET~(MK_BETA+HML_BETA+SMB_BETA)^2
#iClick.lm(Formula=Eq,data=FFplusMOM, bootrep=9)
}

