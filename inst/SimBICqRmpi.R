library(Rmpi)
library(bestglm)
data(Shao)
NumSim <- 10^5
SEED<-123123321
X <- Shao #Note: Shao is a 40-by-4 matrix
#intercept is 2 for all, ie. beta[1,]
BETA <- matrix(c(c(2,0,0,4,0),c(2,0,0,4,8),c(2,9,0,4,8),c(2,9,6,4,8)), ncol=4)
#mpi.apply doesn't understand anonymous functions, so define:
GetTable<-function(i){
            set.seed(SEED)
            BICqSimulation(X,b=BETA[,i],NumSim=NumSim)
            }
#
Start <- proc.time()[3]
StartDate <- date()
#
mpi.spawn.Rslaves(nslaves=4)
mpi.bcast.Robj2slave(SEED)
mpi.bcast.Robj2slave(X)
mpi.bcast.Robj2slave(BETA)
mpi.bcast.Robj2slave(NumSim)
mpi.bcast.Robj2slave(BICqSimulation)
mpi.bcast.Robj2slave(GetTable)
mpi.bcast.cmd(library(bestglm))
#
#note: argument name is 'fun' not 'FUN'
OutTable<-mpi.apply(1:4, fun=GetTable)
End <- proc.time()[3]
EndDate<-date()
Total <- End-Start
#
#output results
for (i in 1:4)
    write.table(OutTable[[i]], file=paste("tb",i,".dat",sep=""))
TotalTime<-paste("started:",StartDate,"\nended:",EndDate,"\nTotal elapsed time in seconds",Total)
write(TotalTime, file="TotalTime.txt")
#
#display files at console
dir()
file.show("TotalTime.txt")
#close and quit
mpi.close.Rslaves()
mpi.quit()
