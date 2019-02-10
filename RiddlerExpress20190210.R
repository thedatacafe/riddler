rm(list=ls())

# myNum is  a vector of 3 elements
divFun = function(myNum)  
{
	prod = myNum[1] * myNum[2]* myNum[3]
	myDiv = NULL
	myDiv = ifelse(floor(prod/100)== prod/100 , T,F)
	myDiv
}

simulations = 1000000
myWin = NULL
for (i in 1:simulations)
{
	a = NULL
	a = sample(1:1000,3,replace=T)
	myWin[i] = divFun(a)
}

table(myWin)



