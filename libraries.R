

install.packages("tsvr")
libraries <- c("grid", "ggplot2", "plyr","dplyr", 'patchwork',
               'gridExtra', 'codyn','tsvr')
lapply(libraries, require, character.only = TRUE)
