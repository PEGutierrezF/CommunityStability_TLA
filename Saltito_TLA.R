

# https://mran.microsoft.com/snapshot/2016-01-12/web/packages/codyn/vignettes/Temporal_Diversity_Indices.html
# https://scholar.google.com.pr/scholar?cites=17191641760846515355&as_sdt=2005&sciodt=0,5&hl=en&authuser=1

# section table -----------------------------
# 28 Mar 2020
#PEGF
#--------------------------------------------
#




saltito <- read_xlsx("raw_data/data.xlsx", sheet = "saltito")
head(saltito)


###########################################################################
# Codyn V1 ----------------------------------------------------------------
###########################################################################

turnoverSaltito <- turnover(df = saltito,  
                         time.var = "time",  
                         species.var = "taxa", 
                         abundance.var = "abundance",
                     replicate.var = NA)
head(turnoverSaltito)

min(turnoverSaltito[,1])
turnoverSaltito[which.min(turnoverSaltito$total),]

max(turnoverSaltito[,1])
turnoverSaltito[which.max(turnoverSaltito$total),]

mean(turnoverSaltito[,1])



# Plot --------------------------------------------------------------------
appearanceSaltito <- turnover(df = saltito,  
                           time.var = "time",  
                           species.var = "taxa", 
                           abundance.var = "abundance", 
                           metric = "appearance")
appearanceSaltito

disappearanceSaltito <- turnover(df = saltito, 
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
  geom_line(size = 1.2) +
  scale_color_manual(name="Metrics",labels = c("Appearance","Disappearance","Total"),
                     values = c("appearance"="#34e383", "disappearance"="#e3347a", "total" ="#34a0e3")) +
  guides(color=guide_legend("Metrics"), size=guide_legend("Density")) +
  
  labs(y="", x = "Time (consecutive month)", colour = "metric") +
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  
  ylim(0, 1) +
  theme(legend.position = "none") +
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5)) 

turn.graphSaltito 

T1. <- ggarrange(turn.graphCarapa , turn.graphSaltito, align = "v",
                        labels = c("(a)", "(b)"), font.label = list(size = 13, face= "plain", 
                      color = "black"), ncol = 1, nrow = 2, common.legend = TRUE,
                 label.x = 0.0,    # Adjust the x position of the labels
                 label.y = c(1.1, 1.1))  # Adjust the y position of the labels) 

T1.
T1.. <-annotate_figure(T1., left = text_grob("Turnover", rot = 90,
                  color = "Black", face = "plain", size = 12))

T1..

ggsave("Figure 1.tiff", plot=T1.., units="in", width=5, height=6, dpi=300, compression = 'lzw')






###########################################################################
# Community synchrony ---------------------------------------------------------------
##########################################################################

stab <- community_stability(saltito, 
                            time.var = "time",
                            abundance.var = "abundance")
stab

# Calculate synchrony via loreau, merge with stab
synch_loreau_saltito <- synchrony(df= saltito, 
                                  time.var = "time",
                                  species.var = "taxa",
                                  abundance.var = "abundance",
                                  replicate.var =NA,
                                  metric="Loreau")
synch_loreau_saltito


# Calculate synchrony via gross, merge with stab
synch_gross_saltito <-synchrony(df= saltito, 
                                time.var = "time",
                                species.var = "taxa",
                                abundance.var = "abundance",
                                replicate.var =NA,
                                metric="Gross")

synch_gross_saltito


###########################################################################
# Rate change code --------------------------------------------------------
###########################################################################

rateChangesSaltito <- rate_change(saltito,   
                                   time.var= "time",    
                                   species.var= "taxa",  
                                   abundance.var= "abundance")
rateChangesSaltito


rateChSaltito <- rate_change_interval(saltito,   
                                 time.var= "time",    
                                 species.var= "taxa",  
                                 abundance.var= "abundance")
rateChSaltito  

modelS <- lm(rateChSaltito$distance~rateChSaltito$interval)
summary(modelS)

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

