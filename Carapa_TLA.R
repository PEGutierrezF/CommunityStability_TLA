



# ---------------------------------------------
# Community Stability Carapa (1997-2016)
# 07 Sep 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())




carapa <- read_xlsx("raw_data/data.xlsx", sheet = "carapa")
head(carapa)



turnover <- turnover(df = carapa,  
                     time.var = "time",  
                     species.var = "taxa", 
                     abundance.var = "abundance",
                     replicate.var = NA, metric = "total")

head(turnover)

min(turnover[,1])
turnover[which.min(turnover$total),]

max(turnover[,1])
turnover[which.max(turnover$total),]

mean(turnover[,1])


appearance <- turnover(df = carapa,  
                       time.var = "time",  
                       species.var = "taxa", 
                       abundance.var = "abundance", 
                       metric = "appearance")
appearance

disappearance <- turnover(df = carapa, 
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
  geom_line(linewidth = 1.2) +
  scale_color_manual(name="Metrics",labels = c("Appearance","Disappearance","Total"),
                     values = c("appearance"="#34e383", "disappearance"="#e3347a", "total" ="#34a0e3")) +
  guides(color=guide_legend("Metrics"), size=guide_legend("Density")) +
  
  labs(y="", x = "", colour = "metric") +
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_blank()) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  
  ylim(0, 1) +
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.9, 0.83)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  theme(legend.background = element_rect(fill = "transparent"))+  # get rid of legend bg
  #  theme(legend.box.background = element_rect(fill = "transparent")) +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

turn.graphCarapa 



###########################################################################
# Community synchrony ---------------------------------------------------------------
##########################################################################

# Calculate synchrony via loreau, merge with stab
synch_loreau <- synchrony(df= carapa, 
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




###########################################################################
# Rate change code --------------------------------------------------------
###########################################################################

rateChangesCarapa <- rate_change(carapa,   
                                   time.var= "time",    
                                   species.var= "taxa",  
                                   abundance.var= "abundance")
rateChangesCarapa


rateChCarapa <- rate_change_interval(carapa,   
                                 time.var= "time",    
                                 species.var= "taxa",  
                                 abundance.var= "abundance")
rateChCarapa  

model <- lm(rateChCarapa$distance~rateChCarapa$interval)
summary(model)

model# Create the graph

rate.Carapa<-ggplot(rateChCarapa, aes(interval, distance)) + 
  labs(y="Distance", x = "Intervals") +
  geom_point(shape=16, fill="gray10", color="gray10", size=1.5)+ 
  stat_smooth(method = "lm", se = F, size = 1) +
  theme_bw() + 
  theme(axis.text.y = element_text(colour = "black", size = rel(1))) + #subaxis size 
  theme(axis.text.x = element_text(colour = "black", size = rel(1))) + #subaxis size  
  theme(axis.title.y = element_text(size = rel(1.25), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.25), angle = 0)) + # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
rate.Carapa

 




