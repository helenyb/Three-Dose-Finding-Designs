#function to execute a single simulation of the 3+3 design

ThreePlusThree_Simulation<-function(seed, true_scenario){
  set.seed(seed)
dose.level<-cohort.index<-1
while(cohort.index<=10){
##simulate responses of one cohort
cohort.DLTs<-sum(runif(3)<true_scenario[dose.level])
#flowchart:
if(cohort.DLTs>1){
  MTD<-dose.level-1
  break
}else if(cohort.DLTs==1){ #additional cohort
  add.cohort.DLTs<-sum(runif(3)<true_scenario[dose.level])
  if((cohort.DLTs+add.cohort.DLTs)>1){
    MTD<-dose.level-1
    break
  }else{
    dose.level<-dose.level+1
  }
}else{
  dose.level<-dose.level+1
}

cohort.index<-cohort.index+1
#if we escalate above the dose range
if(dose.level>length(true_scenario)){
  MTD<-dose.level-1
}
}
return(MTD)
}



