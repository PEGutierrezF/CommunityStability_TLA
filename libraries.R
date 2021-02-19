

install.packages("ggpubr")
libraries <- c("grid", "ggplot2", "plyr","dplyr", 'patchwork',
               'ggpubr', 'gridExtra', 'codyn','tsvr')
lapply(libraries, require, character.only = TRUE)


install.packages("sos")
library(sos)
findFn("ggarrange")
