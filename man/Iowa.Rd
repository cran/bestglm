\name{Iowa}
\alias{Iowa}
\docType{data}
\title{
Iowa School Test
}
\description{
Dataset on poverty and academic performance. 
}
\usage{data("Iowa")}
\format{
  A data frame with 133 observations on the following 3 variables.
  \describe{
    \item{\code{City}}{a factor with 6 levels \code{Cedar Rapids} \code{Davenport}
     \code{Des Moines} \code{Iowa City} \code{Sioux City} \code{Waterloo}}
    \item{\code{Poverty}}{percentage subsidized}
    \item{\code{Test}}{achievement test score}
  }
}
\details{
There are n=133 average test scores for schools in the K=6 largest cities. 
The test score offers a standardized measure of academic achievement. 
The purpose of the study is to investigate if there is a relationship between 
academic achievement, as measured by the test, and poverty. 
It is expected that students from economically disadvantaged backgrounds 
will do less well. Data on the average income in the school district was not 
available so a proxy variable for poverty was used. 
The percentage of students who received subsidized meals was available so 
this was used as the "Poverty" variable.
}
\source{
Abraham and Ledholter, Introduction to Regression, Wiley.
}
\examples{
data(Iowa)
table(Iowa$City)
}

\keyword{datasets}

