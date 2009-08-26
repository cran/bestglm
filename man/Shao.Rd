\name{Shao}
\alias{Shao}
\docType{data}
\title{ Simulated Regression Data}
\description{
Data a simulation study reported by Shao (1993, Table 1).
The linear regression model 
Shao (1993, Table 2) reported 4 simulation experiments using
4 different values for the regression coefficients:

\deqn{y = 2 + \beta_2 x_2 + \beta_3 x_3 + \beta_4 x_4 + \beta_5 x_5 + e,}{%
y = 2 + b[2] x2 + b[3] x3 + b[4] x4 + b[5] x5 + e,}
where \eqn{e} is an independent normal error with unit variance.

The four regression coefficients for the four experiments
are shown in the table below,

\tabular{crrrr}{
Experiment 	\tab \eqn{\beta_2}{b[2]} 
		\tab \eqn{\beta_3}{b[3]}
		\tab \eqn{\beta_4}{b[4]}
		\tab \eqn{\beta_5}{b[5]}\cr 
1\tab	0\tab 0\tab 4\tab 0\cr 
2\tab	0\tab 0\tab 4\tab 8\cr 
3\tab	9\tab 0\tab 4\tab 8\cr 
4\tab	9\tab 6\tab 4\tab 8}


The table below summarizes the probability of correct model selection
in the experiment reported by Shao (1993, Table 2).
Three model selection methods are compared: LOOCV (leave-one-out CV),
CV(d=25) or the delete-d method with d=25 and APCV which is
a very efficient computation CV method but specialized to the
case of linear regression.

\tabular{rlll}{                           
Experiment \tab LOOCV \tab CV(d=25) \tab APCV\cr  
1	\tab 0.484 \tab 0.934  \tab 0.501\cr 
2	\tab 0.641 \tab 0.947  \tab 0.651\cr 
3	\tab 0.801 \tab 0.965  \tab 0.818\cr 
4	\tab 0.985 \tab 0.948  \tab 0.999        
}

The CV(d=25) outperforms LOOCV in all cases and it also outforms APCV
by a large margin in Experiments 1, 2 and 3 but in case 4 APCV
is slightly better. 

}
\usage{data(Shao)}
\format{
  A data frame with 40 observations on the following 4 inputs.
  \describe{
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
    \item{\code{x5}}{a numeric vector}
  }
}

\source{
Shao, Jun (1993). Linear Model Selection by Cross-Validation. 
Journal of the American Statistical Assocation 88, 486-494.
}

\examples{
#In this example BICq(q=0.25) selects the correct model but BIC does not
data(Shao)
X<-as.matrix.data.frame(Shao)
b<-c(0,0,4,0)
set.seed(123321123)
#Note: matrix multiplication must be escaped in Rd file
y<-X\%*\%b+rnorm(40)
Xy<-data.frame(Shao, y=y)
bestglm(Xy)
bestglm(Xy, IC="BICq")

}

\keyword{datasets}



       
       
       
       

