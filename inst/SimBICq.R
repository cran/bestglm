library(Rmpi)
library(bestglm)
data(Shao)
NumSim <- 10^2
SEED<-123123321
X <- Shao #Note: Shao is a 40-by-4 matrix
#intercept is 2 for all, ie. beta[1,]
BETA <- matrix(c(c(2,0,0,4,0),c(2,0,0,4,8),c(2,9,0,4,8),c(2,9,6,4,8)), ncol=4)
#
#Using apply. No significant timing difference.
Start <- proc.time()[3]
OutTable<-lapply(1:4, FUN=function(i){
            set.seed(SEED)
            BICqSimulation(X,b=BETA[,i],NumSim=NumSim)
            })
End <- proc.time()[3]
Total <- End-Start
for (i in 1:4)
    write.table(OutTable[[i]], file=paste("tb",i,".dat",sep=""))
TotalTime<-paste("Total elapsed time in seconds", Total)
write(TotalTime, file="TotalTime.txt")
