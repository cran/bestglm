#Source: grpregPredict.R
#
grpregPredict <- function(Xy, trainFrac=2/3, XyList=NULL) {
 rmse <- function(y, yh) sqrt(mean((y-yh)^2))
 if (is.null(XyList)) {
  XyList <- trainTestPartition(Xy, trainFrac=trainFrac)
 }
 stopifnot(length(XyList)==6)
 XTr <- XyList$XTr
 yTr <- XyList$yTr
 XTe <- XyList$XTe
 yTe <- XyList$yTe
 #LASSO
 ans <- grpreg(XTr, yTr, penalty="grLasso")
 best <- select(ans,"BIC")
 yh<-predict(ans, XTe, type="response", lambda=best$lambda)
 rmseLASSO <- rmse(yTe, yh)
 #SCAD
 ans <- grpreg(XTr, yTr, penalty="grSCAD")
 best <- select(ans,"BIC")
 yh<-predict(ans, XTe, type="response", lambda=best$lambda)
 rmseSCAD <- rmse(yTe, yh)
 #MCP
 ans <- grpreg(XTr, yTr, penalty="grMCP")
 best <- select(ans,"BIC")
 yh<-predict(ans, XTe, type="response", lambda=best$lambda)
 rmseMCP <- rmse(yTe, yh)
 #
 RMSE <- c(rmseLASSO, rmseSCAD, rmseMCP)
 names(RMSE) <- c("LASSO", "SCAD", "MCP")
 RMSE
}