\name{Detroit}
\alias{Detroit}
\docType{data}
\title{Detroit homicide data for 1961-73 used in the book Subset Regression by A.J. Miller}
\description{
For convenience we have labelled the input variables 1 through 11 to be consistent
with the notation used in Miller (2002).
Only the first 11 variables were used in Miller's analyses.
The best fitting subset regression with these 11 variables, uses only 3 inputs 
and has a residual sum of squares of
6.77 while using forward selection produces a best fit with 3 inputs with residual
sum of squares 21.19.  
Backward selection and stagewise methods produce similar results.
It is remarkable that there is such a big difference.
Note that the usual forward and backward selection algorithms may fail since
the linear regression using 11 variables gives essentially a perfect fit.
}
\usage{data(Detroit)}
\format{
  A data frame with 13 observations on the following 14 variables.
  \describe{
    \item{\code{FTP.1}}{Full-time police per 100,000 population}
    \item{\code{UEMP.2}}{Percent unemployed in the population}
    \item{\code{MAN.3}}{Number of manufacturing workers in thousands}
    \item{\code{LIC.4}}{Number of handgun licences per 100,000 population}
    \item{\code{GR.5}}{Number of handgun registrations per 100,000 population}
    \item{\code{CLEAR.6}}{Percent homicides cleared by arrests}
    \item{\code{WM.7}}{Number of white males in the population}
    \item{\code{NMAN.8}}{Number of non-manufacturing workers in thousands}
    \item{\code{GOV.9}}{Number of government workers in thousands}
    \item{\code{HE.10}}{Average hourly earnings}
    \item{\code{WE.11}}{Average weekly earnings}
    \item{\code{ACC}}{Death rate in accidents per 100,000 population}
    \item{\code{ASR}}{Number of assaults per 100,000 population}
    \item{\code{HOM}}{Number of homicides per 100,000 of population}
  }
}

\details{
The data were orginally collected and discussed by Fisher (1976) but
the complete dataset first appeared in Gunst and Mason (1980, Appendix A).
Miller (2002) discusses this dataset throughout his book.
The data were obtained from StatLib.
}

\source{
\url{http://lib.stat.cmu.edu/datasets/detroit}
}

\references{
Fisher, J.C. (1976).  Homicide in Detroit: The Role of Firearms. Criminology, vol.14, 387-400.

Gunst, R.F. and Mason, R.L. (1980). Regression analysis and its application: A data-oriented approach. Marcel Dekker.

Miller, A. J. (2002). Subset Selection in Regression. 2nd Ed. Chapman & Hall/CRC. Boca Raton.
}

\examples{
#Detroit data example
data(Detroit)
#As in Miller (2002) columns 1-11 are used as inputs
p<-11
#For possible comparison with other algorithms such as LARS
#  it is preferable to work with the scaled inputs.
#From Miller (2002, Table 3.14), we see that the
#best six inputs are: 1, 2, 4, 6, 7, 11
X<-as.data.frame(scale(Detroit[,c(1,2,4,6,7,11)]))
y<-Detroit[,ncol(Detroit)]
Xy<-cbind(X,HOM=y)
#Use backward stepwise regression with BIC selects full model
out <- lm(HOM~., data=Xy)
step(out, k=log(nrow(Xy)))
#
#Same story with exhaustive search algorithm
out<-bestglm(Xy, IC="BIC")
out
#But many coefficients have p-values that are quite large considering
#  the selection bias. Note: 1, 6 and 7 are all about 5% only.
#We can use BICq to reduce the number of variables.
#The qTable let's choose q for other possible models,
out$qTable
#This suggest we try q=0.05 or q=0.0005 
bestglm(Xy,IC="BICq", t=0.05)
bestglm(Xy,IC="BICq", t=0.00005)
#It is interesting that the subset model of size 2 is not a subset
# itself of the size 3 model. These results agree with Miller (2002, Table 3.14).
#
#Using delete-d CV with d=4 suggests variables 2,4,6,11
set.seed(1233211)
bestglm(Xy, IC="CV", CVArgs=list(Method="d", K=4, REP=50))
}

\keyword{datasets}
