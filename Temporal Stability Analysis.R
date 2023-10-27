



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
head(data,20)


# Transform Species Richness Carapa
shapiro.test(data$Carapa_Sp_Rich)
Carapa_Sp_Rich_new <- sqrt(data$Carapa_Sp_Rich+1)
shapiro.test(Carapa_Sp_Rich_new)

