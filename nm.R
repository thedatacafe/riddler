rm(list=ls())



n = 12
p = (2*n)-2
success = 0
while (success==0)
{
p=p+1
success = 1
m = rep(0,p)
m[1] = 1
m[p] =  1

	while(sum(m)<(n))
	{
		filled = which(m==1)
		myDist=NULL
		j = 0
		for(unfilled in which(m==0))
		{
			j=j+1
			myDist[j] = min(abs(unfilled-filled))
		}
		m[which(m==0)[which(myDist==max(myDist))[1]]]=1
		if (is.element(1,max(myDist))) {success=0} 
		
	print(m)
	}
print(paste(  "p is", p, "success is", success))

}







