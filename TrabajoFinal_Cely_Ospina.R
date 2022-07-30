
###################################
## Big Data - Problem Set 3 #######
# Maria Camila Cely , Sara Ospina #
###### Julio 2022 #################
###################################

#"Prediction of housing values"

#####################
# 1. Data Acquisition
#####################

## clean environment
rm(list=ls())

## Llamar/instalar las librerias

require(pacman)
p_load(tidyverse,    #Para limpiar los datos
       caret,        #Para la clasificación y regresiones
       rio,          #Para importar datos
       modelsummary, # msummary
       gamlr,        
       class,
       ggplot2,
       skimr,
       rvest,
       dplyr,
       stargazer,
       gtsummary,
       expss,
       fastAdaboost,
       randomForest,
       xgboost,
       glmnet,
       pROC,
       class,
       sf,
       leaflet,
       tmaptools,
       osmdata, 
       skim, 
       readr) #por ahora llame todas las del problem set 3

predict<- stats::predict  #con esto soluciono el problema de que haya mas de una libreria con este comando


setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS")
#setwd("C:/Users/Camila Cely/Documents/GitHub/ProblemSet3_Cely_Ospina")

# input Stata file
library(haven)
BASE_TESIS_2907 <- read_dta("BASE TESIS 2907.dta")
View(BASE_TESIS_2907)


export(BASE_TESIS_2907,"C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/BASE_TESIS.rds")
