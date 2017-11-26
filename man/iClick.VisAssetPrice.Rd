\name{iClick.VisAssetPrice}
\alias{iClick.VisAssetPrice}

\title{
Visualize Daily Asset Price
}
\encoding{latin1}

\description{
This GUI conducts plots of daily asset price, including calendar heatmap and many plots which are not easy to use for new users.
}
\usage{
iClick.VisAssetPrice(DAT, color4 = "r2b", color5 = "jet")
}

\arguments{
  \item{DAT}{
DAT must be a 2-column data frame with the 1-st column as the date string, and the 2nd column is the numeric return series.
}
  \item{color4}{
Color choice for annual calendar heatmap, the default is "r2b".
}
  \item{color5}{
Color choice for 6-year calendar heatmap, the default is "jet".
}
}
\details{
This GUI is designed for financial time series, maily daily stock price. Other time series data works also, as long as it has a date column. We call function calendarPlot() from package "openair", and modified the function calendarHeat() to fit daily price.
}
\value{
Output GUI
}
\author{
Ho Tsungwu <tsungwu@mail.shu.edu.tw>
}


\examples{
#data("IBM")
#assetPrice=IBM[,c(1,4)]
#iClick.VisAssetPrice(assetPrice)
}

