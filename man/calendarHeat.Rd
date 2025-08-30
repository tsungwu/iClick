\name{calendar.heat}
\alias{calendar.heat}

\title{
Calendar Heapmap Plot
}

\encoding{latin1}


\description{
This function generates calendar heatmap plot up to six year, due to visibility.
}
\usage{
calendar.heat(data, ncolors = 99, color = "r2b", date.form = "\%Y-\%m-\%d", main = NULL)
}

\arguments{
  \item{data}{
Daily data of price or others.
}
  \item{ncolors}{
Number of color for heatmap.
}
  \item{color}{
Color plate selected, selection includes c("r2b","r2g","w2b").
}
  \item{date.form}{
Default date form.
}

\item{main}{
main label of graph.
}


}
\details{
This function is within the iClick GUI, is executed within iClick.VisAssetPrice().
}

\value{
Plot
}

\author{
Ho Tsung-wu <tsungwu@ntnu.edu.tw>, College of Management, National Taiwan Normal University
}



