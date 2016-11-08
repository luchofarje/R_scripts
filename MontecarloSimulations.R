#########################################
# Monte Carlo simulation of random walk
#########################################
StartingSet=c('N','E','S','W')
Steps=10000 #10,000
Steps=Steps+1
X = 1:Steps
XX = 2:Steps #Steps=Steps+1 # increment because 0,0 is stored as the first location
AllX = rep(0,Steps)
AllY = rep(0,Steps)
RunningX = rep(0,Steps)
RunningY = rep(0,Steps)

Walk=sample(StartingSet,Steps, replace=TRUE) 
Walk[1] = "Null"

for(i in X) {
  if (Walk[i] == "N") AllY[i] = 1
  if (Walk[i] == "S") AllY[i] = -1  
  if (Walk[i] == "E") AllX[i] = 1
  if (Walk[i] == "W") AllX[i] = -1
}
for (i in XX) {
  RunningX[i] = RunningX[i-1] + AllX[i] 
  RunningY[i] = RunningY[i-1] + AllY[i] 
}
plot(RunningX,RunningY,"l")

B = matrix(c(0, RunningX[Steps], 0, RunningY[Steps]), nrow=2,   ncol=2)
lines(B, col="red")
points(0,0, col="red") #Mark End

XCord = sum((Walk=="N")-(Walk=="S"))
YCord = sum((Walk=="E")-(Walk=="W"))
LinDist = sqrt(XCord^2 + YCord^2)

LinDist




#########################################
# Monte Carlo simulation of random walk
#########################################
StartingSet=c('N','E','S','W')
Steps=100
Steps=Steps+1
Runs=50000 # up to 50,000 takes about 3 seconds
X = 1:Runs
LinDist= rep(0,Runs)
#Steps=Steps+1 # increment because 0,0 is stored as the first location
for(i in X) {
  Walk=sample(StartingSet,Steps, replace=TRUE) 
  Walk[1] = "Null"
  XCord = sum((Walk=="N")-(Walk=="S"))
  YCord = sum((Walk=="E")-(Walk=="W"))
  LinDist[i] = sqrt(XCord^2 + YCord^2)
}
LinDistAvg = mean(LinDist)
LinDistAvg
