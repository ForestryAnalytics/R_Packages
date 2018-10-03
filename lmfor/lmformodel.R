
library(dplyr)
#library(reshape)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lattice)
library(scales)
library(readr)
library(lmfor)
#library(ggpubsr)



#setwd("~/home/github/psm2i_analysis")

TreeList <- grep("Tree.csv",list.files("FullDataset"),value=TRUE)

CombinedTreeDF <- data.frame()

for ( i in 1:length(TreeList)){
  thisDF <- read_csv(paste("FullDataset/",TreeList[i],sep=""))
  
  CombinedTreeDF <- rbind(CombinedTreeDF,thisDF) 
}

CombinedTreeDF <- CombinedTreeDF %>% mutate(
  SPP = factor(SPP),
  Windblown = factor(Windblown)
)


outputs <- ImputeHeights(d=CombinedTreeDF$DBH,
                         h=CombinedTreeDF$Height,
                         plot=CombinedTreeDF$PlotId,
                         modelName="wykoff")

CombinedTreeDF <- data.frame(CombinedTreeDF,Pred = outputs$imputed, H1 = outputs$h, H2 = outputs$hpred)

CombinedTreeDF <- CombinedTreeDF %>% mutate(diff = H1-H2 , sq.error = (H1-H2)^2)

RMSE <- CombinedTreeDF %>% filter(!Pred) %>% summarize(RMSE = sqrt(mean(sq.error)))

aov.test <- aov(diff ~ SPP, data = CombinedTreeDF)

CombinedTreeDF %<>%left_join(
     (CombinedTreeDF %>% group_by(SPP) %>% summarize(SpeciesOffset = mean(diff)) %>% as.data.frame()),
     by=("SPP"="SPP")
     ) %>% 
     mutate(H3 = H2 + SpeciesOffset, 
            adj.sq.error = (H1-H3)^2)


RMSE.adj <- CombinedTreeDF %>% filter(!Pred) %>% summarize(RMSE = sqrt(mean(adj.sq.error)))