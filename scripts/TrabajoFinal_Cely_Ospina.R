
###################################
## Big Data - Problem Set 3 #######
# Maria Camila Cely , Sara Ospina #
###### Julio 2022 #################
###################################

# FINAL PROJECT #

#####################
# 1. Data Acquisition
#####################

## clean environment
rm(list=ls())


###############################
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


###############################
## Directorio

#setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS")

setwd("C:/Users/Camila Cely/Documents/GitHub/tesis_meca")


###############################
## Importar de Stata

#la base la veniamos trabajando en stata, por lo tanto la vamos a importar

# # input Stata file
 library(haven)
 BASE_TESIS_2907 <- read_dta("DATOS/BASE TESIS 2907.dta")
 View(BASE_TESIS_2907)
# 
# 
# export(BASE_TESIS_2907,"C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/BASE_TESIS.rds")



###############################
## Importar la base final a R

db <-readRDS("stores/BASE_TESIS.Rds") 

summary (db)

# ----------- pendiente realizar estadisticas descriptivas




#####################
# 2. Estimation
#####################

install.packages("hdm")
require("hdm")

###
#IDENTIFICACION DE VARIABLES


# y (subsidios) : SUB10MIL #57
# y (vis) : VIS10MIL       #56

# d (tratamiento) : proporcionareaexpansion #33

####
#las variables tienen que ser numeric o factor

#identificamos cuales no cumplen estas caracteristicas

###
#convertir las variables en as.factor cuando se necesite

lapply(db, class)

# $COD
# [1] "character" ##PERO ESTA VARIABLE NO ES NECESARIA, NO HAY QUE CORRERLA EN OLS, PORQUE ES COMO EL ID DEL MUNICIPIO #1
# 
# $Codigodane     "ESTA TAMPOCO HAY QUE CORRERLA, ES OTRO ID # 2
# [1] "numeric"
# 
# $DEPARTAMENTO
# [1] "character"

db <- db %>% mutate (DEPARTAMENTO= as.factor (db$DEPARTAMENTO))
class (db$DEPARTAMENTO) 

# $MUNICIPIO
# [1] "character"

db <- db %>% mutate (MUNICIPIO= as.factor (db$MUNICIPIO))
class (db$MUNICIPIO) 

# $Aglomeración
# [1] "character"

db <- db %>% mutate (Aglomeración= as.factor (db$Aglomeración))
class (db$Aglomeración) 

# 
# $Ejesregionales
# [1] "character"

db <- db %>% mutate (Ejesregionales= as.factor (db$Ejesregionales))
class (db$Ejesregionales)

# 
# $diskm
# [1] "numeric"
# 
# $disminutos
# [1] "numeric"
# 
# $AÑO_orig
# [1] "numeric" #estas de año no pueden ser numeric toca que sean factor

db <- db %>% mutate (AÑO_orig= as.factor (db$AÑO_orig))
class (db$AÑO_orig)

# 
# $AÑO_gen
# [1] "numeric"

db <- db %>% mutate (AÑO_gen= as.factor (db$AÑO_gen))
class (db$AÑO_gen)

# 
# $AÑO_mod
# [1] "numeric"

db <- db %>% mutate (AÑO_mod= as.factor (db$AÑO_mod))
class (db$AÑO_mod)

# 
# $AÑO_MVCT
# [1] "numeric"

db <- db %>% mutate (AÑO_MVCT= as.factor (db$AÑO_MVCT))
class (db$AÑO_MVCT)

# 
# $ULT_POT
# [1] "numeric"

db <- db %>% mutate (ULT_POT= as.factor (db$ULT_POT))
class (db$ULT_POT)

# 
# $HASTA2SMMLV
# [1] "numeric"
# 
# $SUPERIORESA2SMMLVYHASTA3S
# [1] "numeric"
# 
# $SUPERIORESA2SMMLVYHASTA4S
# [1] "numeric"
# 
# $SUPERIORESA3SMMLVYHASTA4S
# [1] "numeric"
# 
# $subtotal
# [1] "numeric"
# 
# $subvis
# [1] "numeric"
# 
# $Hogares2020
# [1] "numeric"
# 
# $Defhab2020
# [1] "numeric"
# 
# $Defcuant2020
# [1] "numeric"
# 
# $Defcuali2020
# [1] "numeric"
# 
# $Hogares2005
# [1] "numeric"
# 
# $Defhab2005
# [1] "numeric"
# 
# $Defcuant2005
# [1] "numeric"
# 
# $Defcuali2005
# [1] "numeric"
# 
# $VIS
# [1] "numeric"
# 
# $Indsub
# [1] "numeric"
# 
# $IndVIS
# [1] "numeric"
# 
# $Totalsueloexpansion
# [1] "numeric"
# 
# $areainicialurbana
# [1] "numeric"
# 
# $proporcionareaexpansion
# [1] "numeric"
# 
# $AREAURBANA
# [1] "numeric"
# 
# $AVALÚOURBANO
# [1] "numeric"
# 
# $Valorsuelo
# [1] "numeric"
# 
# $municipio
# [1] "character" #esta variable esta repetida, la voy a eliminar 

drop <- c("municipio")
db <- db[,!(names(db) %in% drop)]

# 
# $ano
# [1] "numeric"

db <- db %>% mutate (ano= as.factor (db$ano))
class (db$ano)

# 
# $retro_pobl_urb
# [1] "numeric"
# 
# $retro_pobl_tot
# [1] "numeric"
# 
# $pobl_urb
# [1] "numeric"
# 
# $pobl_tot
# [1] "numeric"
# 
# $indrural
# [1] "numeric"
# 
# $altura
# [1] "numeric"
# 
# $pib_percapita
# [1] "numeric"
# 
# $gpc
# [1] "numeric"
# 
# $gini
# [1] "numeric"
# 
# $pobreza
# [1] "numeric"
# 
# $nbi
# [1] "numeric"
# 
# $nbicabecera
# [1] "numeric"
# 
# $otras
# [1] "numeric"
# 
# $IPM
# [1] "numeric"
# 
# $IPM_urb
# [1] "numeric"
# 
# $Aglo
# [1] "haven_labelled" "vctrs_vctr"     "double"        


db <- db %>% mutate (Aglo= as.factor (db$Aglo))
class (db$Aglo)

# 
# $POB10mil
# [1] "numeric"
# 
# $VIS10MIL
# [1] "numeric"
# 
# $SUB10MIL
# [1] "numeric"


##Por ultimo hay que verificar que ninguna variable explicativa tenga valores unique (es decir que sea igualita en todas las observaciones)

sapply(lapply(db, unique), length)

which(sapply(db, function(x) length(unique(x))<2))

#vemos que ano es la variable problematica entonces la eliminamos

drop <- c("ano")
db <- db[,!(names(db) %in% drop)]


### Estimation in a linear model with many confounding factors


# ###
# #Dimensions
# 
# dim(db)
# 
# #[1] 61 56 -- 61 observaciones, 56 variables  ##THE NUMBER OF COVARIATES IS LARGE RELATIVE TO THE SAMPLE SIZE
# 
# 
# ###
# #Establecer nuestras variables
# 
# y_sub10m <- db [,55, drop=F] #variable y de subsidios por cada 10mil habitantes
# y_vis10m <- db [,54, drop=F] #variable y de vis por cada 10mil habitantes
# 
# d_exp <- db [,33, drop=F] #variable "tratamiento" (aumento suelo expansion)
# 
# x <- as.matrix(db)[,-c(33,55,56,1,2)] #matriz del resto de variables #le elimino las variables 1 y 2 porque son id = multicolinearidad con municipios
# 
# varnames <- colnames(db)
# 
# 
# #####################
# # First:  Estimate by OLS
# 
# xnames <- varnames [-c(33,54,55,1,2)]
# 
# dandxnames <- varnames [-c(55,54,1,2)]
# 
# fmla_sub <- as.formula (paste ("SUB10MIL ~ ", paste(dandxnames, collapse= "+")))
# 
# ls_effect_sub <- lm (fmla_sub, data= db) #pese a la limpieza de las variables, esto no ha querido correr


##########################################################################################################

###
# Voy a crear un subset de la base porque la base completa tiene muchas variables repetidas o innecesarias

colnames(db)

# [1] "COD"                       "Codigodane"                "DEPARTAMENTO"              "MUNICIPIO"                
# [5] "Aglomeración"              "Ejesregionales"            "diskm"                     "disminutos"               
# [9] "AÑO_orig"                  "AÑO_gen"                   "AÑO_mod"                   "AÑO_MVCT"                 
# [13] "ULT_POT"                   "HASTA2SMMLV"               "SUPERIORESA2SMMLVYHASTA3S" "SUPERIORESA2SMMLVYHASTA4S"
# [17] "SUPERIORESA3SMMLVYHASTA4S" "subtotal"                  "subvis"                    "Hogares2020"              
# [21] "Defhab2020"                "Defcuant2020"              "Defcuali2020"              "Hogares2005"              
# [25] "Defhab2005"                "Defcuant2005"              "Defcuali2005"              "VIS"                      
# [29] "Indsub"                    "IndVIS"                    "Totalsueloexpansion"       "areainicialurbana"        
# [33] "proporcionareaexpansion"   "AREAURBANA"                "AVALÚOURBANO"              "Valorsuelo"               
# [37] "retro_pobl_urb"            "retro_pobl_tot"            "pobl_urb"                  "pobl_tot"                 
# [41] "indrural"                  "altura"                    "pib_percapita"             "gpc"                      
# [45] "gini"                      "pobreza"                   "nbi"                       "nbicabecera"              
# [49] "otras"                     "IPM"                       "IPM_urb"                   "Aglo"                     
# [53] "POB10mil"                  "VIS10MIL"                  "SUB10MIL"                 

#diskm
#disminutos
#ULT_POT
#subtotal
#subvis
#Defhab2020
#Defcuant2020
#Defcuali2020
#Defhab2005
#Defcuant2005
#Defcuali2005
#VIS
#Indsub
#IndVIS
#proporcionareaexpansion
#Valorsuelo
#indrural
#altura
#pib_percapita
#gpc
#gini
#pobreza
#nbicabecera
#IPM_urb
#Aglo
#VIS10MIL
#SUB10MIL

dbs <- select(filter(db),c( diskm,  disminutos, ULT_POT, subtotal, subvis,  Defhab2020, Defcuant2020, Defcuali2020,  Defhab2005, 
                            Defcuant2005,Defcuali2005,  VIS, Indsub, IndVIS, proporcionareaexpansion,  Valorsuelo,  indrural, 
                            altura, pib_percapita, gpc, gini, pobreza, nbicabecera, IPM_urb, Aglo, VIS10MIL, SUB10MIL )) 


y_sub10 <- dbs [,27, drop=F] #variable y de subsidios por cada 10mil habitantes
y_vis10 <- dbs [,26, drop=F] #variable y de vis por cada 10mil habitantes

d_ex <- dbs [,15, drop=F] #variable "tratamiento" (aumento suelo expansion)

xs <- as.matrix(dbs)[,-c(27,26,15)] #matriz del resto de variables 

varnames <- colnames(dbs)


#####################
# First:  Estimate by OLS

xnames <- varnames [-c(27,26,15)]

dandxnames <- varnames [-c(27,26)]

fmla_sub <- as.formula (paste ("SUB10MIL ~ ", paste(dandxnames, collapse= "+")))

ls_effect_sub <- lm (fmla_sub, data= dbs) #AQUI SI CORRE, pero hay que solucionarle los NAs

summary(ls_effect_sub)



#####################
# Second:  Estimate the effect by the partialling out by Post-Lasso

lasso.effect <- rlassoEffect(x=xs, y=y_sub10, d=d_ex, method= "partialling out")





