
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
 BASE_TESIS_3107 <- read_dta("DATOS/BASE TESIS 3107.dta")
 View(BASE_TESIS_3107)
# 
# 
#export(BASE_TESIS_3107,"C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/stores/BASE_TESIS.rds")



###############################
## Importar la base final a R

db <-readRDS("stores/BASE_TESIS.Rds") 

summary (db)
#no tenemos NA

# ----------- pendiente realizar estadisticas descriptivas




#####################
# 2. Estimation
#####################

install.packages("hdm")
require("hdm")

###
#IDENTIFICACION DE VARIABLES


# y (subsidios) : SUB10MIL #57
# y (vis) : VIS10MIL       #56 Creo que dejar solo esta y centrarnos solo en el efecto sobre VIS, así todo es solo con las 52 obs

# d (tratamiento) : proporcionareaexpansion #33

####
#las variables tienen que ser numeric o factor

#identificamos cuales no cumplen estas caracteristicas

###
#convertir las variables en as.factor cuando se necesite

lapply(db, class)

# $COD
# [1] "character" ##PERO ESTA VARIABLE NO ES NECESARIA, NO HAY QUE CORRERLA EN OLS, PORQUE ES COMO EL ID DEL MUNICIPIO #1 #respuesta: no es necesaria, ya la toma codmpio es el mismo ID
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
# $Ejesregionales >> Esta creo que es mejor quitarla porque no tiene valores para todas las observaciones, no todos los municipios hacen parte de un eje
# las aglomeraciones igual capturan todo este efecto
# [1] "character"

#db <- db %>% mutate (Ejesregionales= as.factor (db$Ejesregionales))
#class (db$Ejesregionales)

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
# $ULT_POT
# [1] "numeric"

db <- db %>% mutate (ULT_POT= as.factor (db$ULT_POT))
class (db$ULT_POT)

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
# $AVALÚOURBANO
# [1] "numeric"
# 
# $Valorsuelo
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
# $dismdo
# [1] "numeric"
#
# $y_total
# [1] "numeric"
#
# $g_total
# [1] "numeric"
#
# $finan
# [1] "numeric"
#
# $DF_desemp_fisc
# [1] "numeric"
#
# $DI_desemp_int
# [1] "numeric"
#
# $indesarrollo_mun
# [1] "numeric"
#
# $indesarrollo_dep
# [1] "numeric"
#
# $inv_en_vivienda
# [1] "numeric"
#
# $inv_total
# [1] "numeric"
#
#$categoria
#[1] "character"

db <- db %>% mutate (categoria= as.factor (db$categoria))
class (db$categoria)
###
###


##Por ultimo hay que verificar que ninguna variable explicativa tenga valores unique (es decir que sea igualita en todas las observaciones)

sapply(lapply(db, unique), length)

which(sapply(db, function(x) length(unique(x))<2))


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

#[1] "codmpio"                 "DEPARTAMENTO"            "MUNICIPIO"               "Aglomeración"            "diskm"                  
#[6] "disminutos"              "AÑO_orig"                "ULT_POT"                 "Hogares2005"             "Defhab2005"             
#[11] "Defcuant2005"            "Defcuali2005"            "VIS"                     "IndVIS"                  "Totalsueloexpansion"    
#[16] "areainicialurbana"       "proporcionareaexpansion" "AVALÚOURBANO"            "Valorsuelo"              "pobl_urb"               
#[21] "pobl_tot"                "indrural"                "altura"                  "pib_percapita"           "gpc"                    
#[26] "gini"                    "pobreza"                 "nbicabecera"             "IPM_urb"                 "Aglo"                   
#[31] "POB10mil"                "VIS10MIL"                "dismdo"                  "y_total"                 "g_total"                
#[36] "finan"                   "DF_desemp_fisc"          "DI_desemp_int"           "indesarrollo_mun"        "indesarrollo_dep"       
#[41] "inv_en_vivienda"         "inv_total"               "categoria"  

#Aglomeración
#diskm
#disminutos
#AÑO_orig
#ULT_POT
#Defhab2005
#Defcuant2005
#Defcuali2005
#IndVIS
#proporcionareaexpansion
#Valorsuelo
#pobl_urb
#indrural
#altura
#pib_percapita
#gpc
#gini
#pobreza
#nbicabecera
#IPM_urb
#VIS10MIL
#dismdo
#y_total
#g_total
#finan
#DF_desemp_fisc
#DI_desemp_int
#indesarrollo_mun
#indesarrollo_dep
#inv_en_vivienda
#inv_total
#categoria



#dbs <- select(filter(db),c( diskm,  disminutos, ULT_POT, subtotal, subvis,  Defhab2020, Defcuant2020, Defcuali2020,  Defhab2005, 
#                            Defcuant2005,Defcuali2005,  VIS, Indsub, IndVIS, proporcionareaexpansion,  Valorsuelo,  indrural, 
#                            altura, pib_percapita, gpc, gini, pobreza, nbicabecera, IPM_urb, Aglo, VIS10MIL, SUB10MIL ))  #Sari meti muchas variables aca pero revisa si falta o sobra alguna 

dbs <- select(filter(db),c( Aglomeración , diskm , disminutos , AÑO_orig , ULT_POT , Defhab2005 , Defcuant2005 , Defcuali2005 , IndVIS , 
                            proporcionareaexpansion , Valorsuelo , pobl_urb , indrural , altura , pib_percapita , gpc , gini , pobreza , 
                            nbicabecera , IPM_urb , VIS10MIL , dismdo , y_total , g_total , finan , DF_desemp_fisc , DI_desemp_int , 
                            indesarrollo_mun , indesarrollo_dep , inv_en_vivienda , inv_total , categoria ))


#y_sub10 <- dbs [,27, drop=F] #variable y de subsidios por cada 10mil habitantes
y_vis10 <- dbs [,21, drop=F] #variable y de vis por cada 10mil habitantes

d_ex <- dbs [,10, drop=F] #variable "tratamiento" (aumento suelo expansion)

#xs <- as.matrix(dbs)[,-c(27,26,15)] #matriz del resto de variables 
xs <- as.matrix(dbs)[,-c(21,10)] #matriz del resto de variables 

varnames <- colnames(dbs)


#####################
# First:  Estimate by OLS

#xnames <- varnames [-c(27,26,15)]
xnames <- varnames [-c(21,10)]

#dandxnames <- varnames [-c(27,26,15)]
dandxnames <- varnames [-c(21)]

#fmla_sub <- as.formula (paste ("SUB10MIL ~ ", paste(dandxnames, collapse= "+")))
fmla_sub <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames, collapse= "+")))



ls_effect_sub <- lm (fmla_sub, data = dbs) #AQUI SI CORRE, pero hay que solucionarle los NAs

summary(ls_effect_sub)



#####################
# Second:  Estimate the effect by the partialling out by Post-Lasso



lasso.effect <- rlassoEffect(x=xs, y=y_sub10, d=d_ex, method= "double selection") #pendiente segun respuesta de ignacio




Eff = rlassoEffect(X[, -1], y, X[, 1], method = "partialling out")
summary(Eff)$coef[, 1:2]



##PENDIENTES

# 1 - terminar codigo (hoy) - Sara

# 2 - Sara - variable pablo querubin (2011)
# 2 - Camila - variable dummy pot           (mañana primera hora)

# 3 - correr (asumamos que sale bien todo) - Sara
# 3 - montar rapidamente borrador de paper - Camila

# 4 - hacer estadisticas descriptivas y nutrir el paper - Sara
# 4 - esto puede incluir mapas                          - Camila

# 5 - Consolidar y enviar


# Otros pendientes

# - preguntarle a Ignacio lo de aglomeracion



