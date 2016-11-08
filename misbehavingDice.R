start<-proc.time()#start timing the script execution

##### Script variables
dieCount=4 #number of Dice
sampleSize=100000 #number of dice rolls to test
diesuccess=0 # result if dice 1 beats dice 2 
success=1 # result if all dice beat each other A->B->C->D->A
Dice=matrix(nrow=dieCount,ncol=6) # matrix to hold all dice values
d=0 #loop counter
totalloops=0 #total loops of recalculation i.e after calculation of A,B,C & D how many times did D & A pair generated/tested
genCount = rep(0,dieCount) # number of times each Die was generated

#pthres: minimum probability with which dice A needs to beat dice B by and with the rest
#pthres can be set to 0 which means that dice 1 has to beat dice 2 just once (one roll out of the sampleSize) to be considered a solution
#pthres of 0.3 (arbitary selection) means each dice has to beat the next dice at least 30% of the time
#pthres of 0.5 was still running on my laptop after 8 or 9 hours, so keep this value low
pthres=0.3

##### functions

#returns a new Dice with 6 values between 0-6
newDie<-function(){
  return(sample(0:6,6,replace=TRUE)) 
}

#calculates total number of times dice 1 beats dice 2
calcDiff<-function(d1,d2){
  return(sum(d1>d2))
}

##### main execution loop
repeat{
  d=d+1 # increment counter
  if(d>dieCount ){ #exit loop if counter is greater than number of dice
    break
  }
  
  if(d==dieCount) totalloops=totalloops+1
  
  #set first dice
  #die is first dice out of a pair that is being generated/tested
  die=d
  if(d==dieCount) {nextdie=1} else {nextdie=d+1}
  
  #generate die 1 if it has not been generated
  if(is.na(Dice[die,1])){
    Dice[die,]=newDie()
	genCount[die]=genCount[die]+1
  }
  
  #generate die 2 if it has not been generated OR if the current test fails
  #test failure is if dice 1 does NOT beat dice 2, thus generate new dice 2 and keep dice 1 same
  if(success==0 || is.na(Dice[nextdie,1])){
    Dice[nextdie,]=newDie()
	genCount[nextdie]=genCount[nextdie]+1
  }
  
  #simulate dice rolls for both dice
  d1=sample(Dice[die,],sampleSize,replace=TRUE)
  d2=sample(Dice[nextdie,],sampleSize,replace=TRUE)
  
  #calculate if dice 1 beats dice 2 and compare the probability with desired pthres
  if((calcDiff(d1,d2)/sampleSize)>pthres){
    diesuccess=1
  } else {
    diesuccess=0
    d=d-1 #decrement counter so that the "nextdie" or dice 2 is regenerated
  }

  success= (diesuccess && success) #set that all pairs from beginning till now successfully beat each other

  #exit loop if all dice are generated and they beat the next die A->B->C->D->A
  if(d==dieCount && success==1){
      break
  }
}

#return results
View(Dice) # show the generated dice
##genCount and totalloops are interesting to see how many iterations the algo took to find a solution
View(genCount) # show generation count for each dice
View(totalloops) # show total loops 
proc.time()-start #show elapsed time