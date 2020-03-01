

library(ggplot2)
library(grid)
library(gridExtra)
library(codyn)
library(dplyr)
library(tidyr)


Carapa<-read.csv("D:/Curriculum/02_ Articulos/00 In progress/220 Community stability/CommunityStability_TLA/codyCarapa.csv",h=T) # read the accompanying csv file
head(Carapa)


############ Run the turnover code #############
  
turnover <- turnover(df = Carapa,  
                         time.var = "time",  
                         species.var = "taxa", 
                         abundance.var = "abundance")

turnover

appearance <- turnover(df = Carapa,  
                           time.var = "time",  
                           species.var = "taxa", 
                           abundance.var = "abundance", 
                           metric = "appearance")
appearance

disappearance <- turnover(df = Carapa, 
                              time.var = "time",  
                              species.var = "taxa", 
                              abundance.var = "abundance", 
                              metric = "disappearance")

disappearance


#Format a compiled data frame
turnover$metric<-"total"
names(turnover)[1]="turnover"

appearance$metric<-"appearance"
names(appearance)[1]="turnover"

disappearance$metric<-"disappearance"
names(disappearance)[1]="turnover"

allturnover<-rbind(turnover, appearance, disappearance)

allturnover


#Create the graph
turn.graph <- ggplot(allturnover, aes(x=time, y=turnover, color=metric)) + 
  geom_line(size = 1) +  
  theme_bw() + 
  theme(legend.position="bottom")

turn.graph 


############ #Run the rank shift code ##########


rankshift <- rank_shift(df=Carapa, 
                        time.var = "time", 
                        species.var = "taxa",
                        abundance.var = "abundance")

rankshift

#Select the final time point from the returned time.var_pair
rankshift$samp_event <- seq(1, 108)
rankshift

# Create the graph
rankshift.graph <- ggplot(rankshift, aes(samp_event, MRS)) + 
  geom_line(size = 1) + 
  theme_bw() 


rankshift.graph


########### Run the rate change code ########

rateChanges <- rate_change(Carapa,   
                                   time.var= "time",    
                                   species.var= "taxa",  
                                   abundance.var= "abundance")
rateChanges


rateChange <- rate_change_interval(Carapa,   
                                 time.var= "time",    
                                 species.var= "taxa",  
                                 abundance.var= "abundance")
rateChange  

# Create the graph
rate.graph<-ggplot(rateChange, aes(interval, distance)) + 
  geom_point()+ 
  stat_smooth(method = "lm", se = F, size = 1) +
  theme_bw() 

rate.graph 


########### Calculate community stability ##############


stab <- community_stability(Carapa, 
                            time.var = "time",
                            abundance.var = "abundance")
stab


####### Calculate variance ratio, merge with stab ##########


# VARIANCE RATIO The

# If species vary independently, then the variance ratio will be
# close to 1. Avariance ratio <1 indicates predominately 
# negative species covariance, whereas a variance ratio 
# >1 indicates that species generally positively covary.

vRatio <- merge(variance_ratio(Carapa, time.var = "time",
                           species.var = "taxa",
                           abundance.var = "abundance",
                           bootnumber=1, 
                           average.replicates = F), stab)
vRatio


vr.graph <-ggplot(vRatio, aes(x=VR, y=stability)) + 
  geom_point(size=3) +
  theme_bw() +   
  theme(text= element_text(size = 14))

vr.graph
