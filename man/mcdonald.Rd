\name{mcdonald}
\alias{mcdonald}
\docType{data}
\title{
Pollution dataset from McDonald and Schwing (1973)
}
\description{
Regression data used to illustrate ridge regression
}
\usage{data("mcdonald")}
\format{
  A data frame with 60 observations on the following 16 variables.
  \describe{
    \item{\code{PREC}}{Average annual precipitation in inches}	
    \item{\code{JANT}}{Average January temperature in degrees F}	
    \item{\code{JULT}}{Same for July}	
    \item{\code{OVR65}}{Percent of 1960 SMSA population aged 65 or older}	
    \item{\code{POPN}}{Average household size} 	
    \item{\code{EDUC}}{Median school years completed by those over 22}	
    \item{\code{HOUS}}{Percent of housing units which are sound & with all facilities}	
    \item{\code{DENS}}{Population per sq. mile in urbanized areas, 1960}	
    \item{\code{NONW}}{Percent non-white population in urbanized areas, 1960}	
    \item{\code{WWDRK}}{Percent employed in white collar occupations}	
    \item{\code{POOR}}{Percent of families with income < $3000}	
    \item{\code{HC}}{Relative hydrocarbon pollution potential}	
    \item{\code{NOX}}{Same for nitric oxides}	
    \item{\code{SOx}}{Same for sulphur dioxide}
    \item{\code{HUMID}}{Annual average percent relative humidity at 1pm}	
    \item{\code{MORT}}{Total age-adjusted mortality rate per 100,000}	  
  }
}
\details{
Ridge regression example
}
\source{
Gary C. McDonald and Richard C. Schwing (1973),
Instabilities of Regression Estimates Relating Air Pollution to Mortality,
Technometrics 15/3, 463-481.
}

\examples{
data(mcdonald)
vifx(mcdonald[, -ncol(mcdonald)])
}
\keyword{datasets}
