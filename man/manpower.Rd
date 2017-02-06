\name{manpower}
\alias{manpower}
\docType{data}
\title{Hospital manpower data}
\description{
The goal of this study is to predict the manpower requirement as given in 
the output 
variable Hours given the five other input variables.
Data is from Table 3.8 of Myers (1990). See also Examples 3.8, 4.5, 8.8.
}
\usage{data(manpower)}
\format{
  A data frame with 17 observations. The output variable is Hours and the
  inputs are Load, Xray, BedDays, AreaPop and Stay. The site 1 through 17 
  is indicated by the row name.
  \describe{
    \item{\code{Load}}{a numeric vector}
    \item{\code{Xray}}{a numeric vector}
    \item{\code{BedDays}}{a numeric vector}
    \item{\code{AreaPop}}{a numeric vector}
    \item{\code{Stay}}{a numeric vector}
    \item{\code{Hours}}{a numeric vector}
  }
}
\details{
This data illustrates the multicollinearity problem and the use of VIF to 
identify it. It provides an illustrative example for ridge regression and 
more modern methods such as lasso and lars.
}
\source{
Myers (1990) indicates the source was 
"Procedures and Analysis for Staffing Standards Development: 
Data/Regression Analysis Handbook", 
Navy Manpower and Material Analysis
Center, San Diego, 1979. 
}
\references{
Myers, R. (1990). Classical and Modern Regression with Applications. 
The Duxbury Advanced Series in Statistics and Decision Sciences. 
Boston: PWS-KENT Publishing Company.  
}
\examples{
data(manpower)
}
\keyword{datasets}
