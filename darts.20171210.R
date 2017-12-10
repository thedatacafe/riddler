rm(list=ls())
#### setwd('C:/Users/')    ##### Set your directory, this is where the output file will be exported.

#### Define number of simulation rounds
simulation.rounds = 10000
exp.rows = 5

#### Defining functions: 
###(a) Boolean function (T or F) whether a point lies inside the circle or not (isPointInCircle)
isPointInCircle = function(x,y,r)  #### x and y are coordinates for the point, ### r = radius of cirlce, ### center assumed to be (0,0)
{
	d = sqrt(x^2+y^2)
	incircle = ifelse(d>r,F,T)
	return(incircle)
}

### (b) Numeric function to determine distance between two points (disPoints)
disPoints = function(x1,y1,x2,y2) ##### x1 and y1 are coordinates for point 1, x2 and y2 are coordinates for point 2
{
	d = sqrt((x1-x2)^2+(y1-y2)^2)
	return(d)
}

### Create a database of random points in the circle - will have approximately 3.93*(the number of simulation rounds) 
tmpPoints = as.data.frame(matrix(-9,simulation.rounds*exp.rows,2))
colnames(tmpPoints)= c('x','y')
tmpPoints$x = runif(n = simulation.rounds*exp.rows, min = -1, max = 1)
tmpPoints$y = runif(n = simulation.rounds*exp.rows, min = -1, max = 1)
r = rep(1,simulation.rounds*exp.rows)
inCirc  = isPointInCircle(tmpPoints$x,tmpPoints$y,r)
simPoints = subset(tmpPoints, inCirc)
dim(simPoints)

###  simulData is our database that will be updated through simulations

simulData = simPoints
simulData$x = simulData$y = simulData$sim.round = simulData$dart.throw = simulData$min.dist = rep(-9, nrow(simulData))

myRow = 0
for (mySims in 1:simulation.rounds)   ##### Iterate over number of simulation rounds
{
	myRow = myRow+1
	dartCounter = 0		      ###### Reset dart throw counter
	minDist = 1                   ###### default value of minimum distance between any two pair of points
	simulData$x[myRow]= simPoints$x[myRow]
	simulData$y[myRow]= simPoints$y[myRow]
	simulData$sim.round[myRow]=mySims
	simulData$dart.throw[myRow]=dartCounter
	simulData$min.dist[myRow]= minDist
	while (!(minDist<1))          ###### If all pairwise points are more than 1 feet apart only then continue, else next round of simulation. 
	{
		myRow = myRow+1
		dartCounter = dartCounter+1
		simulData$x[myRow]= simPoints$x[myRow]
		simulData$y[myRow]= simPoints$y[myRow]
		simulData$dart.throw[myRow]=dartCounter
		myRound = NULL
		myRound = subset(simulData,sim.round==mySims)
		myRound$x2 = rep(simPoints$x[myRow], nrow(myRound))
		myRound$y2 = rep(simPoints$y[myRow], nrow(myRound))
		myRound$min.dist = disPoints(myRound$x, myRound$y, myRound$x2, myRound$y2)
		minDist = min(myRound$min.dist)
		simulData$sim.round[myRow]=mySims
		simulData$min.dist[myRow]=minDist
	}


	if(floor(mySims/100)==mySims/100){print(mySims)}
}


simulData = subset(simulData, x > -2)
table(simulData$dart.throw)
write.table(simulData, "simulationResults.tsv", sep='\t', row.names=F, quote=F)



