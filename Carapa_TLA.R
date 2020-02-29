

Carapa<-read.csv("D:/Curriculum/02_ Articulos/00 In progress/220 Community stability/Time Lag Analisys/codyCarapa.csv",h=T) # read the accompanying csv file
head(Carapa)

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

