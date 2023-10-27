



# ---------------------------------------------
# Temporal Stability
# 27 Oct 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())




data <- read_excel('raw_data/ts_data.xlsx', sheet = 'carapa')
head(data,6)


# Transform Species Richness Carapa
shapiro.test(data$Carapa_Sp_Rich)
Carapa_Sp_Rich_new <- sqrt(data$Carapa_Sp_Rich+1)
shapiro.test(Carapa_Sp_Rich_new)

# Transform Productivity Carapa
shapiro.test(data$Carapa_Biomass_TS)
Carapa_Biomass_TS_new <- 1/(data$Carapa_Biomass_TS)
shapiro.test(Carapa_Biomass_TS_new)


