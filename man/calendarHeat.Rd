\name{calendarHeat}
\alias{calendarHeat}

\title{
Calendar Heapmap Plot
}

\encoding{latin1}


\description{
This function generates calendar heatmap plot up to six year, due to visibility.
}
\usage{
calendarHeat(values, ncolors = 99, color = "r2b", date.form = "\%Y-\%m-\%d")
}

\arguments{
  \item{values}{
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
  
}
\details{
This function is within the iClick GUI, is executed within iClick.VisAssetPrice().
}

\value{
Plot
}

\author{
Ho Tsungwu <tsungwu@mail.shu.edu.tw>
}



