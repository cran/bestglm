`BICqSimulation` <-
function(X, beta, NumSim, g=c(0.5,1),q=c(0.15,0.25,0.75)) 
{
### y=[1,X]*beta+sigma*N(0,1)
#Arguments: X - n*p design matrix for regression
#           beta - vector of p+1 coefficients (including intercept)
#           NumSim - number of simulations
#           g - adjustable parameters for EBIC
#           q - tuning parameters for BICq
#
stopifnot(ncol(X)==length(beta)-1)
n<- nrow(X)
K<- ncol(X)
b0<- beta[1]
b<- beta[-1]
s0<- (b!=0)
X1<- as.matrix(X[,b!=0]) 
b1<- b[b!=0]
y0<- b0+X1%*%b1
k0<- length(b1)
out<- 0
meMean<- 0
meSum2<- 0
for (i in 1:NumSim){
    y<- y0+rnorm(n)  
    Xy<- data.frame(X,y=y) 
#AIC
    m<- bestglm(Xy,IC="AIC")
    Subs<- m$Subsets[,-1]
    sAIC<- as.matrix(Subs[which.min(Subs[,ncol(Subs)]),][1:K])
    fit<- m$BestModel
    meAIC<- sum((y0-fit$fitted.values)^2)
    sAICst<- as.matrix(Subs[m$Bestq[1,3]+1,][1:K])
    sBICst<- as.matrix(Subs[m$Bestq[2,3]+1,][1:K])
    fit<- lm(y~., data=data.frame(X[,sAICst],y=y))
    meAICst<- sum((y0-fit$fitted.values)^2)
    fit<- lm(y~., data=data.frame(X[,sBICst],y=y))
    meBICst<- sum((y0-fit$fitted.values)^2)
#BIC
    m<- bestglm(Xy,IC="BIC")
    Subs<- m$Subsets[,-1]
    sBIC<- as.matrix(Subs[which.min(Subs[,ncol(Subs)]),][1:K])
    fit<- m$BestModel
    meBIC<- sum((y0-fit$fitted.values)^2)
#EBICs
    sBICg<- matrix(rep(NA,K*length(g)),ncol=K)
    meBICg<- rep(NA,length(g))
    for (j in 1:length(g)){
    m<- bestglm(Xy,IC="BICg",t=g[j])
    Subs<- m$Subsets[,-1]
    sBICg[j,]<- as.matrix(Subs[which.min(Subs[,ncol(Subs)]),][1:K])
    fit<- m$BestModel
    meBICg[j]<- sum((y0-fit$fitted.values)^2)
    }
#BICq's
    sBICq<- matrix(rep(NA,K*length(q)),ncol=K)
    meBICq<- rep(NA,length(q))
    for (j in 1:length(q)){
    m<- bestglm(Xy,IC="BICq",t=q[j])
    Subs<- m$Subsets[,-1]
    sBICq[j,]<- as.matrix(Subs[which.min(Subs[,ncol(Subs)]),][1:K])
    fit<- m$BestModel
    meBICq[j]<- sum((y0-fit$fitted.values)^2)
    }
#
    SS<- rbind(sAIC,sBIC,sBICg,sBICq,sAICst,sBICst)
    rownames(SS)<- c("AIC","BIC", paste("BICg(g=",g,")",sep=""), 
                paste("BICq(q=",q,")",sep=""), "BICq1","BICq2")
    meSS<- c(meAIC,meBIC,meBICg,meBICq,meAICst,meBICst) 
    Delta<- meSS-meMean
    meMean<- meMean+Delta/i
    meSum2<- meSum2+Delta*(meSS-meMean)
    SS_s0<- SS-matrix(rep(s0,nrow(SS)),nrow=nrow(SS),byrow=TRUE)
    SS_s0<- rowSums(SS_s0)
    kSS<- rowSums(SS)
    overfit <- kSS>k0
    underfit <- kSS<k0
    correct <- as.numeric(SS_s0==0)
    out <- out+cbind(overfit=(kSS>k0), underfit=(kSS<k0), correct=(SS_s0==0)) 
    }
overfit <- out[,"overfit"]/NumSim
underfit <- out[,"underfit"]/NumSim
correct <- out[,"correct"]/NumSim
cbind(overfit=overfit, underfit=underfit, correct=correct, me=meMean, se.me= sqrt(meSum2/(NumSim*(NumSim-1))))
}
