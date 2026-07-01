#function to execute a single simulation of the BOIN design
library(BOIN)

BOIN_simulation<-function(seed,true_scenario){
 
  #set seed for replicability
  set.seed(seed)
  
   #initialize 
  DLTs<-Patients<-rep(0,5)
  dose.level<-cohort.index<-1
  
  #while loop to progress through the cohorts
  while(cohort.index<=10){
    
    ##simulate responses of one cohort
    cohort.DLTs<-sum(runif(3)<true_scenario[dose.level])
    #record responses
    DLTs[dose.level]<-DLTs[dose.level]+cohort.DLTs
    Patients[dose.level]<-Patients[dose.level]+3

    #BOIN recommends the next dose level
    dose.level<- next.comb(0.33,npts=as.matrix(Patients),ntox=as.matrix(DLTs),dose.curr =c(dose.level,1) ,
                           cutoff.eli=0.95)$next_dc[1]
   #if BOIN design recommends lowest dose is unsafe
     if(!is.numeric(dose.level)){
     #  browser()
      MTD<-0
      break
    }
    #updates the cohort index to the next cohort
    cohort.index<-cohort.index+1
    
    #sufficient information rule
    if(Patients[dose.level]==12){
      MTD<-dose.level
      break
    }
    #if we escalate above the dose range
    if(dose.level>length(true_scenario)){
      MTD<-dose.level-1
    }
  }
  #if we reach maximum sample size
  if(cohort.index==11){
    MTD<-select.mtd(0.33,npts=Patients,ntox=DLTs)$MTD
  }
  return(MTD)
  
}
