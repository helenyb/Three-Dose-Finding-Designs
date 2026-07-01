source("ThreePlusThree.R")
source("BOIN.R")
source("CRM.R")


##running 1000 simulations for each design
ThreePlusThree_Results<-sapply(c(1:1000),ThreePlusThree_Simulation, true_scenario=c(1/6,2/6,3/6,4/6,5/6))
BOIN_Results<-sapply(c(1:1000),BOIN_simulation, true_scenario=c(1/6,2/6,3/6,4/6,5/6))
CRM_Results<-sapply(c(1:1000),CRM_simulation, true_scenario=c(1/6,2/6,3/6,4/6,5/6))

#tabulating results for each design
Selections_ThreePlusThree<-table(factor(ThreePlusThree_Results, levels = 0:5))
Selections_BOIN<-table(factor(BOIN_Results, levels = 0:5))
Selections_CRM<-table(factor(CRM_Results, levels = 0:5))

#collating results for all designs
All_selections<-data.frame(Design=rep(c("3+3","BOIN","CRM"),each=6),
                           Dose=as.factor(rep(c(0:5),times=3)),
                           Selection=c(Selections_ThreePlusThree,
                                       Selections_BOIN,
                                       Selections_CRM))


##plotting results
library(ggplot2)

g<-ggplot()
g<-g+geom_col(data=All_selections, aes(x=Dose, y=Selection, fill=Design), width=0.75, position="dodge")+
  ylim(c(0,600))+
  labs(y="Number of simulations", x="MTD Recommendation", title= "MTD Selections Comparison")

g