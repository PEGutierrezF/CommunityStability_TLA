



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
# Square root transformation
shapiro.test(data$Carapa_Sp_Rich)
Carapa_Sp_Rich_new <- sqrt(data$Carapa_Sp_Rich+1)
shapiro.test(Carapa_Sp_Rich_new)

# Transform Productivity Carapa
# Reciprocal (inverse) transformation
shapiro.test(data$Carapa_Biomass_TS)
Carapa_Biomass_TS_new <- 1/(data$Carapa_Biomass_TS)
shapiro.test(Carapa_Biomass_TS_new)


# Linnear regression
model <- lm(Carapa_Biomass_TS_new ~ Carapa_Sp_Rich_new)
# Print the summary of the regression model
summary(model)



ggplot(data, aes(x = Carapa_Sp_Rich_new, y = Carapa_Biomass_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "Transformed Species Richness",
       y = "Transformed Biomass",
       title = "Scatter Plot with Regression Line")
