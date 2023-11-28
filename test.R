


data_carapa <- read_excel('raw_data/rarefaction_curves.xlsx', sheet = 'Sheet1')
head(data_carapa,6)
datatable(data_carapa)

# Sum the columns to get the total abundance for each sampling period
total_abundance <- colSums(data_carapa[, -1])

# Round the total abundance values to the nearest integer
total_abundance <- round(total_abundance)

# Create a rarefaction curve
rare_curve <- rarecurve(total_abundance, step = 100)


install.packages('BiodiversityR')
library(BiodiversityR)
library(vegan)
library("DT") # Crea cuadros interactivos HTML 

data("BCI")
datatable(BCI)

curva <- specaccum(BCI)
plot(curva)
