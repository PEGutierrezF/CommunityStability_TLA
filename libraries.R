



# ---------------------------------------------
# Packages and libraries
# 07 Sep 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



# cleans global environment
rm(list = ls())



libraries <- c("grid", "ggplot2", "plyr","dplyr", 'patchwork',
               'ggpubr', 'gridExtra', 'codyn','tidyr','readxl')

lapply(libraries, require, character.only = TRUE)







