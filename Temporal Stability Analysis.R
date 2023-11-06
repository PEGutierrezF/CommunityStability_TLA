



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
head(data,6)



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


intercept <- coef(mod1)[1]
slope <- coef(mod1)[2]
# Now you can form the equation
equation <- paste("y =", round(intercept, 2), "+", round(slope, 2), "* x")
# Print the equation
cat(equation)

# Linnear regression
mod2 <- lm(Carapa_Density_TS_new ~ Carapa_Sp_Rich_new)
# Print the summary of the regression model
summary(mod2)

###########################################################################
# Saltito -----------------------------------------------------------------
###########################################################################

# Transform Species Richness Saltito
# Square root transformation
shapiro.test(data$Saltito_Sp_Rich)
Saltito_Sp_Rich_new <- log(data$Saltito_Sp_Rich+10)
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



# Figures -----------------------------------------------------------------
CP <- ggplot(data, aes(x = Carapa_Sp_Rich_new, y = Carapa_Biomass_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # Add regression line
  labs(x = "",
       y = "Temporal stability",
       title = "") +
  
  theme_bw() +
  
  theme(axis.title.x = element_blank()) + # axis x
  # theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90, color="black")) + # axis y
  # theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.x=element_blank()) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y

  geom_text(aes(x = 3.5, y = 2),
            label = "italic(y) == 1.52 - 0.15 * x * ',' ~italic(R)^2 ~'='~0.07~',' ~italic(p) ~'='~0.004",
            color = "black", size=5, parse = TRUE)

CP

CP + ggsave("Figure 1.jpeg",width = 18, height = 22, units = "cm", dpi = 600)

CD <- ggplot(data, aes(x = Carapa_Sp_Rich_new, y = Carapa_Density_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "Transformed Species Richness",
       y = "Temporal stability",
       title = "")

CD



SP <- ggplot(data, aes(x = Saltito_Sp_Rich_new, y = Saltito_Biomass_TS_new)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "",
       y = "",
       title = "")

SP



SD <- ggplot(data_new, aes(x = Saltito_Sp_Rich_na_omit, y = Saltito_Density_TS_na_omit)) +
  geom_point() +                   # Add scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(x = "Species Richness",
       y = "",
       title = "")

SD


(CP + SP) / (CD + SD)


