if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,sjmisc,ggplot2,interplot,marginaleffects,sjlabelled,haven,stringr,ordinal,ggplot2)  
rm(list=ls())
load(here::here("input/data/proc/study1_country.RData"));df1 <- df2


df1$class11d


frq(df1$class10spo)
frq(df1$class10)

frq(df1$digclass10)

frq(df1$digclass11)
frq(df1$digclass11d)
