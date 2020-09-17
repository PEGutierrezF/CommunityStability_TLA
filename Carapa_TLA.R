

# https://mran.microsoft.com/snapshot/2016-01-12/web/packages/codyn/vignettes/Temporal_Diversity_Indices.html
# https://scholar.google.com.pr/scholar?cites=17191641760846515355&as_sdt=2005&sciodt=0,5&hl=en&authuser=1

# section lable -----------------------------
# 28 Mar 2020
#PEGF
#--------------------------------------------
#



library(ggplot2)
library(grid)
library(gridExtra)
library(codyn)
library(dplyr)
library(tidyr)


Carapa<-read.csv("codyCarapa.csv") # read the accompanying csv file
head(Carapa)



# Turnover code -----------------------------------------------------------


turnover <- turnover(df = Carapa,  
                         time.var = "time",  
                         species.var = "taxa", 
                         abundance.var = "abundance",
                     replicate.var = NA)

turnover

min(turnover[,1])
max(turnover[,1])
mean(turnover[,1])


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

allturnoverCarapa <-rbind(turnover, appearance, disappearance)

allturnoverCarapa


#Create the graph
turn.graphCarapa <- ggplot(allturnoverCarapa, aes(x=time, y=turnover, color=metric)) + 
  geom_line(size = 1) +  
  theme_bw() + 
  theme(legend.position = "none")

turn.graphCarapa 


# Run the rank shift code -------------------------------------------------

rankshift <- rank_shift(df=Carapa, 
                        time.var = "time", 
                        species.var = "taxa",
                        abundance.var = "abundance")

rankshift

#Select the final time point from the returned time.var_pair
rankshift$samp_event <- seq(1, 122)
rankshift

# Create the graph
rankshift.graph <- ggplot(rankshift, aes(samp_event, MRS)) + 
  geom_line(size = 1) + 
  theme_bw() 
rankshift.graph


# Rate change code --------------------------------------------------------

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


###########################################################################
# Calculate community stability -------------------------------------------
##########################################################################

data("knz_001d")
head(collins08)
head(knz_001d)


# Synchrony ---------------------------------------------------------------
# Calculate synchrony via gross, merge with stab

## Calculate community stability
stab <- community_stability(df = Carapa, 
                            time.var = "time",
                            abundance.var = "abundance",
                            replicate.var= NA)
stab

# Calculate synchrony via loreau, merge with stab
synch_loreau <- synchrony(df= Carapa, 
                              time.var = "time",
                              species.var = "taxa",
                              abundance.var = "abundance",
                              replicate.var =NA,
                              metric="Loreau")
synch_loreau


# Calculate synchrony via gross, merge with stab
synch_gross<-synchrony(df= Carapa, 
                             time.var = "time",
                             species.var = "taxa",
                             abundance.var = "abundance",
                             replicate.var =NA,
                             metric="Gross")

synch_gross


# Calculate variance ratio, merge with stab
vr <- variance_ratio(df= Carapa,
                     time.var = "time",
                           species.var = "taxa",
                           abundance.var = "abundance",
                           replicate.var =NA,
                           bootnumber=1, 
                           average.replicates = F)
vr
