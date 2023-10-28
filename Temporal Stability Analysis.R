



# ---------------------------------------------
# Temporal Stability
# 27 Oct 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  


# cleans global environment
rm(list = ls())




data <- read_excel('raw_data/ts_data.xlsx', sheet = 'ts')
head(data.frm,6)



# Carapa-60 ---------------------------------------------------------------

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

# Transform Density Carapa
# Reciprocal (inverse) transformation
shapiro.test(data$Carapa_Density_TS)
Carapa_Density_TS_new <- 1/sqrt(data$Carapa_Density_TS)
shapiro.test(Carapa_Density_TS_new)



# Linnear regression
mod1 <- lm(Carapa_Biomass_TS_new ~ Carapa_Sp_Rich_new)
# Print the summary of the regression model
summary(mod1)


# Linnear regression
mod2 <- lm(Carapa_Density_TS_new ~ Carapa_Sp_Rich_new)
# Print the summary of the regression model
summary(mod2)


cP <- ggplot(data, aes(x = Carapa_Sp_Rich_new, y = Carapa_Biomass_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "Transformed Species Richness",
       y = "Transformed Biomass",
       title = "")

cP



cD <- ggplot(data, aes(x = Carapa_Sp_Rich_new, y = Carapa_Density_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "Transformed Species Richness",
       y = "Transformed Biomass",
       title = "")

cD



# Saltito -----------------------------------------------------------------


# Transform Species Richness Saltito
# Square root transformation
shapiro.test(data$Saltito_Sp_Rich)
Saltito_Sp_Rich_new <- sqrt(data$Saltito_Sp_Rich+2)
shapiro.test(Saltito_Sp_Rich_new)

# Transform Productivity Saltito
# Reciprocal (inverse) transformation
shapiro.test(data$Saltito_Biomass_TS)
Saltito_Biomass_TS_new <- 1/sqrt(data$Saltito_Biomass_TS)
shapiro.test(Saltito_Biomass_TS_new)



data_new <- data %>% 
  select(Saltito_Sp_Rich, Saltito_Density_TS)%>%
  filter(row_number() != 118)
# Convert Saltito_Density_TS to numeric
data_new$Saltito_Density_TS <- as.numeric(data_new$Saltito_Density_TS)

# Transform Species Richness Saltito
# Square root transformation
shapiro.test(data_new$Saltito_Sp_Rich)
Saltito_Sp_Rich_na_omit <- sqrt(data_new$Saltito_Sp_Rich+2)
shapiro.test(Saltito_Sp_Rich_na_omit)


# Check the Shapiro-Wilk test
shapiro.test(data_new$Saltito_Density_TS)
Saltito_Density_TS_na_omit <- 1/sqrt(data_new$Saltito_Density_TS)
shapiro.test(Saltito_Density_TS_na_omit)




# Linnear regression
mod3 <- lm(Saltito_Biomass_TS_new ~ Saltito_Sp_Rich_new)
# Print the summary of the regression model
summary(mod3)


# Linnear regression
mod4 <- lm(Saltito_Density_TS_na_omit ~ Saltito_Sp_Rich_na_omit)
# Print the summary of the regression model
summary(mod4)





