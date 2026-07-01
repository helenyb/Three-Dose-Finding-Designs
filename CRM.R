#function to execute a single simulation of the CRM design

library(dfcrm)

CRM_simulation<-function(seed,true_scenario){
  #set seed for replicability
  set.seed(seed)
  
  #initialize 
  DLTs<-Patients<-rep(0,5)
  DLT_vector<-PatientDoses<-c()
  dose.level<-cohort.index<-1
  
  #while loop to progress through the cohorts
  while(cohort.index<=10){
    
    ##simulate responses of one cohort
    new.cohort.DLTs<-runif(3)<true_scenario[dose.level]
    
    #record responses
    PatientDoses<-c(PatientDoses,rep(dose.level,3))
    DLT_vector<-c(DLT_vector,new.cohort.DLTs)
    DLTs[dose.level]<-DLTs[dose.level]+sum(new.cohort.DLTs)
    Patients[dose.level]<-Patients[dose.level]+3
    
    #CRM recommends the next dose level
    CRM.output<- crm(prior=c(0.33,	0.43,	0.53,	0.61,	0.69	),
                     target=0.33,
                     tox=DLT_vector,
                     level=PatientDoses,
                     n=length(PatientDoses),
                     scale=sqrt(0.89))
      
    dose.level<-CRM.output$mtd
    
    #if CRM design recommends lowest dose is unsafe
         if(CRM.output$ptoxL[1]>0.33){
      MTD<-0
      break
    }
    
    #restricts skipping doses (so we don't escalate too quickly)
    if(dose.level>PatientDoses[length(PatientDoses)]+1){
      dose.level<-PatientDoses[length(PatientDoses)]+1
      
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
    MTD<-dose.level
  }
  return(MTD)
  
}

sapply(c(1:100),CRM_simulation, true_scenario=c(1/6,2/6,3/6,4/6,5/6))
