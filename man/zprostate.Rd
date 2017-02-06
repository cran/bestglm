\name{zprostate}
\alias{zprostate}
\docType{data}
\title{ Prostate cancer data. Standardized.}
\description{
Data with 8 inputs and one output used to illustrate the prediction
problem and regression in the textbook of 
Hastie, Tibshirani and Freedman (2009).
}
\usage{data(zprostate)}
\format{
  A data frame with 97 observations, 9 inputs and 1 output.
  All input variables have been standardized.
  \describe{
    \item{\code{lcavol}}{log-cancer volume}
    \item{\code{lweight}}{log prostate weight}
    \item{\code{age}}{age in years}
    \item{\code{lbph}}{log benign prostatic hyperplasia}
    \item{\code{svi}}{seminal vesicle invasion}
    \item{\code{lcp}}{log of capsular penetration}
    \item{\code{gleason}}{Gleason score}
    \item{\code{pgg45}}{percent of Gleascores 4/5}
    \item{\code{lpsa}}{Outcome. Log of PSA}
    \item{\code{train}}{TRUE or FALSE}
  }
}
\details{
A study of 97 men with prostate cancer examined the correlation 
between PSA (prostate specific antigen) and a number of clinical measurements:
lcavol, lweight, lbph, svi, lcp, gleason, pgg45
}

\references{
Hastie, Tibshirani & Friedman. (2009).  
The Elements of Statistical Learning: Data Mining, Inference, and Prediction.  
2nd Ed. Springer.
}

\examples{
#Prostate data. Table 3.3 HTF.
data(zprostate)
#full dataset
trainQ<-zprostate[,10]
train <-zprostate[trainQ,-10]
test <-zprostate[!trainQ,-10]
ans<-lm(lpsa~., data=train)
sig<-summary(ans)$sigma
yHat<-predict(ans, newdata=test)
yTest<-zprostate$lpsa[!trainQ]
TE<-mean((yTest-yHat)^2)
#subset
ansSub<-bestglm(train, IC="BICq")$BestModel
sigSub<-summary(ansSub)$sigma
yHatSub<-predict(ansSub, newdata=test)
TESub<-mean((yTest-yHatSub)^2)
m<-matrix(c(TE,sig,TESub,sigSub), ncol=2)
dimnames(m)<-list(c("TestErr","Sd"),c("LS","Best"))
m

}

\keyword{datasets}
