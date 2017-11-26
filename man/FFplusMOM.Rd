\name{FFplusMOM}
\alias{FFplusMOM}
\docType{data}
\encoding{latin1}
\title{
Data of Fama-French beta of 811 listed companies of SSEC
}

\description{
Average data of 811 listed companies of SSEC, 2001/1/03~2012
}
\usage{data("FFplusMOM")}
\format{
  A data frame with 811 observations on the following 4 variables.
  \describe{
    \item{\code{company}}{company code}
    \item{\code{RET}}{company-specific average returns}
    \item{\code{MK_BETA}}{CAPM factor beta}
    \item{\code{HML_BETA}}{High-Minus-Low factor beta}
    \item{\code{SMB_BETA}}{Small-Minus-Big factor beta}
    \item{\code{MOM_BETA}}{Momemtum factor beta}
  }
}
\details{
Daily stock returns of 24 world national markets.
}
\source{
Yahoo finance.
}
\examples{
data(FFplusMOM)

}
