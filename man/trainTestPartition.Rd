\name{trainTestPartition}
\alias{trainTestPartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Partition Dataframe into Train/Test Samples
}
\description{
Dataframe used to create training and test datasets using
specified fraction for the training sample.
The data matrix must be comprised of continuous variables
only (no factors).
}
\usage{
trainTestPartition(Xy, trainFrac = 2/3)
}

\arguments{
  \item{Xy}{
Dataframe with column names, last column is the response
variable and others are the regression input variables.
The data matrix must be comprised of continuous variables
only (no factors).
}
  \item{trainFrac}{
Fraction to be used for the training sample.
}
}

\value{
A list with components
  \item{XyTr }{Training dataframe.}
  \item{XTr }{Matrix, input training variables.}
  \item{yTr }{Vector, output training variable.}
  \item{XyTe}{Training dataframe.}
  \item{XTe }{Matrix, input test variables.}
  \item{yTe }{Vector, output test variable.}
  
  \item{XyTr }{Training dataframe.}
  \item{XyTr }{Training dataframe.}
  \item{XyTr }{Training dataframe.}
}

\author{
A. I. McLeod
}

\examples{
set.seed(7733551)
out <- trainTestPartition(mcdonald)
round(glmnetGridTable(out),4)
}

\keyword{ datagen }

