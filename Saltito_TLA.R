

# https://mran.microsoft.com/snapshot/2016-01-12/web/packages/codyn/vignettes/Temporal_Diversity_Indices.html
# https://scholar.google.com.pr/scholar?cites=17191641760846515355&as_sdt=2005&sciodt=0,5&hl=en&authuser=1

# section lable -----------------------------
# 28 Mar 2020
#PEGF
#--------------------------------------------
#



collins08
Saltito<-read.csv("codySaltito.csv") # read the accompanying csv file
head(Saltito)


###########################################################################
# Codyn V2 ----------------------------------------------------------------
###########################################################################

tableRAC_saltito <- RAC_change(df = Saltito, time.var = "time",  
                              species.var = "taxa", abundance.var = "abundance",
                              replicate.var = NULL,reference.time = NULL)
head(tableRAC_saltito)


min(tableRAC_saltito[,3])
max(tableRAC_saltito[,3])
mean(tableRAC_saltito[,3])


# Create the graph
richnesschanges_saltito_plot <- ggplot(tableRAC_saltito, aes(time2, richness_change)) + 
  labs(y="Species richness", x = "Time", colour = "") +
  geom_line(size = 1) + 
  ylim(-1,1) +
  theme_bw() + 
  theme(axis.text.y = element_text(colour = "black", size = rel(1))) + # axis size 
  theme(axis.text.x = element_text(colour = "black", size = rel(1))) + # axis and ticks 
  theme(axis.title.y = element_text(size = rel(1.25), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.25), angle = 0)) + # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

richnesschanges_saltito_plot


speciesrichness <- richnesschanges_carapa_plot / richnesschanges_saltito_plot + plot_annotation(tag_levels = 'A')
speciesrichness

speciesrichness + ggsave("Figure 4.JPEG",width=6, height=4,dpi=600)



RAC_difference(df = Carapa, time.var = "time", 
               species.var = "taxa", abundance.var = "abundance", 
               replicate.var= NULL, treatment.var = NULL, 
               pool = FALSE, block.var = NULL, 
               reference.treatment = NULL)

###########################################################################
# Codyn V1 ----------------------------------------------------------------
###########################################################################

turnoverSaltito <- turnover(df = Saltito,  
                         time.var = "time",  
                         species.var = "taxa", 
                         abundance.var = "abundance",
                     replicate.var = NA)

turnoverSaltito
min(turnoverSaltito[,1])
max(turnoverSaltito[,1])
mean(turnoverSaltito[,1])


appearanceSaltito <- turnover(df = Saltito,  
                           time.var = "time",  
                           species.var = "taxa", 
                           abundance.var = "abundance", 
                           metric = "appearance")
appearanceSaltito

disappearanceSaltito <- turnover(df = Saltito, 
                              time.var = "time",  
                              species.var = "taxa", 
                              abundance.var = "abundance", 
                              metric = "disappearance")

disappearanceSaltito


#Format a compiled data frame
turnoverSaltito$metric<-"total"
names(turnoverSaltito)[1]="turnover"

appearanceSaltito$metric<-"appearance"
names(appearanceSaltito)[1]="turnover"

disappearanceSaltito$metric<-"disappearance"
names(disappearanceSaltito)[1]="turnover"

allturnoverSaltito<-rbind(turnoverSaltito, appearanceSaltito, disappearanceSaltito)

allturnoverSaltito


#Create the graph
turn.graphSaltito <- ggplot(allturnoverSaltito, aes(x=time, y=turnover, color=metric)) + 
  labs(y="Turnover", x = "Time (consecutive month)", colour = "metric") +
  geom_line(size = 1) +  
  guides(color=guide_legend("Metrics"), size=guide_legend("Density")) +
  ylim(0, 1) +
  theme_bw() + 
  theme(axis.text = element_text(colour = "black", size = rel(1))) + #axis size 
  theme(axis.title.y = element_text(size = rel(1.25), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.25), angle = 0))+ # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position="bottom", legend.box = "horizontal")+
  theme(legend.key = element_rect(fill = "white", colour = "white")) +
  theme(legend.title = element_text(size=12, color = "black"),legend.text = element_text(size=12))+
  scale_color_manual(name="Metrics",labels = c("Appearance","Disappearance","Total"),
                     values = c("appearance"="#999999", "disappearance"="#E69F00", "total" ="#56B4E9"))

turn.graphSaltito 

T1 <- turn.graphCarapa / turn.graphSaltito + plot_annotation(tag_levels = 'A')
T1
T1 + ggsave("Figure 1.JPEG",width=6, height=4,dpi=600)

###########################################################################
# Run the rank shift code -------------------------------------------------
###########################################################################

rankshift_saltito <- rank_shift(df=Saltito, 
                        time.var = "time", 
                        species.var = "taxa",
                        abundance.var = "abundance")

rankshift_saltito

#Select the final time point from the returned time.var_pair
rankshift_saltito$samp_event <- seq(1, 122)
rankshift_saltito

# Create the graph
rankshift_saltito_plot <- ggplot(rankshift_saltito, aes(samp_event, MRS)) + 
  labs(y="Mean rank shift", x = "Sampling event", colour = "") +
  geom_line(size = 1) + 
  ylim(0, 4) +
  theme_bw() + 
  theme(axis.text.y = element_text(colour = "black", size = rel(1))) + # axis size 
  theme(axis.text.x = element_text(colour = "black", size = rel(1))) + # axis and ticks 
  theme(axis.title.y = element_text(size = rel(1.25), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.25), angle = 0)) + # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

rankshift_saltito_plot


rs <- rankshift_carapa_plot / rankshift_saltito_plot + plot_annotation(tag_levels = 'A')
rs

rs + ggsave("Figure 3.JPEG",width=6, height=4,dpi=600)



###########################################################################
# Rate change code --------------------------------------------------------
###########################################################################

rateChangesSaltito <- rate_change(Saltito,   
                                   time.var= "time",    
                                   species.var= "taxa",  
                                   abundance.var= "abundance")
rateChangesSaltito


rateChSaltito <- rate_change_interval(Saltito,   
                                 time.var= "time",    
                                 species.var= "taxa",  
                                 abundance.var= "abundance")
rateChSaltito  

# Create the graph
rate.Saltito<-ggplot(rateChSaltito, aes(interval, distance)) + 
  labs(y="", x = "Intervals") +
  geom_point(shape=16, fill="gray10", color="gray10", size=1.5)+ 
  stat_smooth(method = "lm", se = F, size = 1) +
  theme_bw() + 
  ylim(0, 30000) +
  theme(axis.text.y = element_text(colour = "black", size = rel(1))) + #axis size 
  theme(axis.text.x = element_text(colour = "black", size = rel(1))) + # axis and ticks 
  theme(axis.title.y = element_text(size = rel(1.25), angle = 90)) +  # axis title
  theme(axis.title.x = element_text(size = rel(1.25), angle = 0)) + # axis title
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

rate.Saltito

RC <- rate.Carapa + rate.Saltito + plot_annotation(tag_levels = 'A')
RC
RC + ggsave("Figure 2.JPEG",width=6, height=4,dpi=600)


# Calculate community stability -------------------------------------------



stab <- community_stability(Saltito, 
                            time.var = "time",
                            abundance.var = "abundance")
stab

# Calculate synchrony via loreau, merge with stab
synch_loreau_saltito <- synchrony(df= Saltito, 
                          time.var = "time",
                          species.var = "taxa",
                          abundance.var = "abundance",
                          replicate.var =NA,
                          metric="Loreau")
synch_loreau_saltito


# Calculate synchrony via gross, merge with stab
synch_gross_saltito <-synchrony(df= Saltito, 
                       time.var = "time",
                       species.var = "taxa",
                       abundance.var = "abundance",
                       replicate.var =NA,
                       metric="Gross")

synch_gross_saltito

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
