\name{pcreg}
\alias{pcreg}

\title{
Principal Component and Partial Least Squares Regression
}
\description{
Regression using the principal components or latent variables as inputs.
The best model is selected using
components 1, 2, ..., r, where r, the number of components to use
is determined by the AIC or BIC.
}
\usage{
pcreg(Xy, scale = TRUE, method = c("PC", "LV"), ic = c("BIC", "AIC"))
}

\arguments{
  \item{Xy}{
dataframe with variable names in columns
}
  \item{scale}{
Whether or not to scale. Default is TRUE.
}
  \item{method}{
either principal components, "PC", or partial least squares latent variables,
"LV"
}
  \item{ic}{
"BIC" or "AIC"
}
}

\value{
An S3 class list "pcreg" with components
\item{lmfit}{lm model}
\item{PLSFit}{column sd}
\item{Z}{matrix of principal components or latent vector}
\item{method}{'pcr' or 'pls'}
}

\author{
A. I. McLeod
}

\seealso{
\code{\link{predict.pcreg}},
\code{\link{summary.pcreg}},
\code{\link{plot.pcreg}},
\code{\link{fitted.pcreg}},
\code{\link{residuals.pcreg}}
}
\examples{
pcreg(mcdonald, scale=TRUE, method="PC")
pcreg(mcdonald, scale=TRUE, method="LV")
}

\keyword{ models }
\keyword{ regression }

