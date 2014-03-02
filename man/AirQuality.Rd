\name{AirQuality}
\Rdversion{1.1}
\alias{AirQuality}
\docType{data}
\title{
Daily ozone pollution  with meteorlogical and date inputs
}
\description{
This dataset was derived from the R built-in dataset `airquality` by adding
date information and deleting all missing values.
This dataset is referred to as `environmental` in Cleveland (1993).
}

\usage{data(AirQuality)}

\format{
  A data frame with 111 observations on the following 6 variables.
  \describe{
    \item{\code{Solar.R}}{input, a numeric vector}
    \item{\code{Wind}}{input, a numeric vector}
    \item{\code{Temp}}{input, a numeric vector}
    \item{\code{month}}{input, a factor with levels \code{May} \code{Jun} \code{Jul} \code{Aug} \code{Sep} \code{Oct} \code{Nov} \code{Dec} \code{Jan} \code{Feb} \code{Mar} \code{Apr}}
    \item{\code{weekday}}{input, a factor with levels \code{Sunday} \code{Monday} \code{Tuesday} \code{Wednesday} \code{Thursday} \code{Friday} \code{Saturday}}
    \item{\code{Ozone}}{output, a numeric vector}
  }
}

\details{
Cleveland (1993, Chapter 5) presents an insightful analysis using co-plots and the scatterplot matrix.
Several interesting interactions are noted.
For a fixed 'Wind`, the effect of 'Solar.R' changes as 'Temp' increases.
And for a fixed 'Temp', as 'Wind' decreases, the effect of 'Solar.R' is less.
}


\source{
\code{\link{airquality}}
}

\references{
Cleveland, W.S. (1993). Visualizing Data.
}

\examples{
data(AirQuality)
#Example 1. Find best model
bestglm(AirQuality, IC="BIC")

}
\keyword{datasets}
