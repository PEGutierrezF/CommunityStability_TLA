

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
library(tsvr)


Carapa<-read.csv("codyCarapa.csv") # read the accompanying csv file
head(Carapa)



# Turnover code -----------------------------------------------------------
# Codyn v2
tableRAC <- RAC_change(df = Carapa, time.var = "time",  
           species.var = "taxa", abundance.var = "abundance",
            replicate.var = NULL,reference.time = NULL)
head(tableRAC)

ggplot(tableRAC, aes(x=time2, y=richness_change )) +
  geom_line()


RAC_difference(df = Carapa, time.var = "time", 
                species.var = "taxa", abundance.var = "abundance", 
                replicate.var= NULL, treatment.var = NULL, 
                pool = FALSE, block.var = NULL, 
                reference.treatment = NULL)

# Codyn v1
turnover <- turnover(df = Carapa,  
                         time.var = "time",  
                         species.var = "taxa", 
                         abundance.var = "abundance",
                     replicate.var = NA, metric = "total")

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
  labs(y="Turnover", x = "Time", colour = "metric") +
  geom_line(size = 2) +  
  guides(color=guide_legend("Metrics"), size=guide_legend("Density")) +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black", size = rel(1.25))) + #axis size 
  theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.5), angle = 0))+ # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  theme(legend.key = element_rect(fill = "white", colour = "white")) +
        theme(legend.title = element_text(size=18, color = "black"),legend.text = element_text(size=18))+
  scale_color_manual(name="Metrics",labels = c("Appearance","Disappearance","Total"),
        values = c("appearance"="#999999", "disappearance"="#E69F00", "total" ="#56B4E9"))
  
turn.graphCarapa 



###########################################################################
# Run the rank shift code -------------------------------------------------
###########################################################################

rankshift <- rank_shift(df=Carapa, 
                        time.var = "time", 
                        species.var = "taxa",
                        abundance.var = "abundance")

rankshift

min(rankshift[,2])
max(rankshift[,2])
mean(rankshift[,2])

#Select the final time point from the returned time.var_pair
rankshift$samp_event <- seq(1, 122)
rankshift

# Create the graph
rankshift.graph <- ggplot(rankshift, aes(samp_event, MRS)) + 
  geom_line(size = 1) + 
  theme_bw() 
rankshift.graph

###########################################################################
# Rate change code --------------------------------------------------------
###########################################################################

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

model <- lm(rateChange$distance~rateChange$interval)
summary(model)

model# Create the graph
rate.graph<-ggplot(rateChange, aes(interval, distance)) + 
  geom_point()+ 
  stat_smooth(method = "lm", se = F, size = 1) +
  theme_bw() 



rate.graph 


###########################################################################
# Calculate community stability -------------------------------------------
##########################################################################

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


# New functions -----------------------------------------------------------

community_structure(df= Carapa,
  time.var = "time",
  abundance.var= "abundance",
  replicate.var = NULL,
  metric = "SimpsonEvenness") #"SimpsonEvenness": Calculates Simpson's evenness

#Calculates changes in species richness, evenness, species' ranks, gains, 
# and losses for each replicate

RAC_change(df = Carapa,
           species.var = "taxa",
           abundance.var = "abundance",
           time.var = "time")

# For each species in a replicate, calculates changes in abundance
abundance_change(df = Carapa,
                 species.var = "taxa",
                 abundance.var = "abundance",
                 time.var = "time")

