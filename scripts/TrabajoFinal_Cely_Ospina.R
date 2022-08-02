
###################################
## Big Data - Final Project #######
# Maria Camila Cely , Sara Ospina #
###### Agosto 2022 ################
###################################

#####################
# 1. Data Acquisition
#####################

## clean environment
rm(list=ls())


###############################
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
       leaflet,
       haven,
       hdm)

##############################
##############################
## Directorio

#setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS")
#setwd("C:/Users/Camila Cely/Documents/GitHub/tesis_meca")


###############################
###############################
## Importacion de Stata

#la base la veniamos trabajando en stata, por lo tanto la vamos a importar

# # input Stata file
#library(haven)
#BASE_TESIS_3107 <- read_dta("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS/BASE TESIS 3107.dta")
# View(BASE_TESIS_3107)
# 
# 
#export(BASE_TESIS_3107,"C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/stores/BASE_TESIS.rds")


###############################
###############################
## Importar la base final a R

db <-readRDS("stores/BASE_TESIS.Rds") 
summary (db)
#no tenemos NA


#####################
# 2. Limpieza y estadisticas descriptivas
#####################


###############################
###############################
#IDENTIFICACION DE VARIABLES 

# y (vis) : VIS10MIL      
# d (tratamiento) : proporcionareaexpansion 



###############################
###############################
# CLASS DE LAS VARIABLES


####
#las variables tienen que ser numeric o factor
#identificamos cuales no cumplen estas caracteristicas

###
#convertir las variables en as.factor cuando se necesite

lapply(db, class)

# $COD
# [1] "character" ##PERO ESTA VARIABLE NO ES NECESARIA, NO HAY QUE CORRERLA EN OLS, codmpio es el mismo ID
# 
# $Codigodane     #ESTA TAMPOCO HAY QUE CORRERLA, ES OTRO ID # 2
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
# [1] "numeric" #esta no la voy a volver factor porque la necesito para crear la dummy de incorporacion automatica

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

db <- db %>% mutate (Aglo= as.numeric (db$Aglo))
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



###############################
###############################
# VERIFICAR VALORES UNIQUE

##Por ultimo hay que verificar que ninguna variable explicativa tenga valores unique (es decir que sea igualita en todas las observaciones)

sapply(lapply(db, unique), length) #aqui vemos que ya no hay ninguna con el valor de 1

which(sapply(db, function(x) length(unique(x))<2)) #0



###############################
###############################
# MODIFICACIONES A VARIABLES EXISTENTES

#########
#VARIABLE: dummy POT (proxy): toma valor de 1 si el pot fue modificado cuando el decreto de incorporacion automatica estuvo vigente
# 0 de lo contrario


db <- db %>% mutate (pot_exc = ULT_POT)
class (db$pot_exc)
summary(db$pot_exc)

db <- db %>% mutate (pot_exc = if_else (pot_exc >= 2015 & pot_exc <= 2020 , 1, 0))


#########
#VARIABLE: Dummies por Aglomeracion


#Volver variable de aglomeracion dummies

db <- db %>% 
  mutate(A_Apartado = if_else(db$Aglo==1, 1, 0))

db <- db %>% 
  mutate(A_Armenia = if_else(db$Aglo==2, 1, 0))

db <- db %>% 
  mutate(A_Barrancabermenja = if_else(db$Aglo==3, 1, 0))

db <- db %>% 
  mutate(A_Barranquilla = if_else(db$Aglo==4, 1, 0))

db <- db %>% 
  mutate(A_Bogota = if_else(db$Aglo==5, 1, 0))

db <- db %>% 
  mutate(A_Bucaramanga = if_else(db$Aglo==6, 1, 0))

db <- db %>% 
  mutate(A_Buenaventura = if_else(db$Aglo==7, 1, 0))

db <- db %>% 
  mutate(A_Cali = if_else(db$Aglo==8, 1, 0))

db <- db %>% 
  mutate(A_Cartagena = if_else(db$Aglo==9, 1, 0))

db <- db %>% 
  mutate(A_Cartago = if_else(db$Aglo==10, 1, 0))

db <- db %>% 
  mutate(A_Cucuta = if_else(db$Aglo==12, 1, 0))

db <- db %>% 
  mutate(A_Duitama = if_else(db$Aglo==13, 1, 0))

db <- db %>% 
  mutate(A_Fusagasuga = if_else(db$Aglo==15, 1, 0))

db <- db %>% 
  mutate(A_Girardot = if_else(db$Aglo==16, 1, 0))

db <- db %>% 
  mutate(A_Buga = if_else(db$Aglo==17, 1, 0))

db <- db %>% 
  mutate(A_Ibague = if_else(db$Aglo==18, 1, 0))

db <- db %>% 
  mutate(A_Manizales = if_else(db$Aglo==20, 1, 0))

db <- db %>% 
  mutate(A_Medellin = if_else(db$Aglo==21, 1, 0))

db <- db %>% 
  mutate(A_Monteria = if_else(db$Aglo==22, 1, 0))

db <- db %>% 
  mutate(A_Neiva = if_else(db$Aglo==23, 1, 0))

db <- db %>% 
  mutate(A_Pasto = if_else(db$Aglo==25, 1, 0))

db <- db %>% 
  mutate(A_Pereira = if_else(db$Aglo==26, 1, 0))

db <- db %>% 
  mutate(A_Popayan = if_else(db$Aglo==27, 1, 0))

db <- db %>% 
  mutate(A_Rionegro = if_else(db$Aglo==29, 1, 0))

db <- db %>% 
  mutate(A_SantaMarta = if_else(db$Aglo==31, 1, 0))

db <- db %>% 
  mutate(A_Sincelejo = if_else(db$Aglo==32, 1, 0))

db <- db %>% 
  mutate(A_Sogamoso = if_else(db$Aglo==33, 1, 0))

db <- db %>% 
  mutate(A_Tulua = if_else(db$Aglo==34, 1, 0))

db <- db %>% 
  mutate(A_Tunja = if_else(db$Aglo==35, 1, 0))

db <- db %>% 
  mutate(A_Valledupar = if_else(db$Aglo==37, 1, 0))

db <- db %>% 
  mutate(A_Villavicencio = if_else(db$Aglo==38, 1, 0))





#########
#VARIABLE: Dummies por Categoria

db <- db %>% 
  mutate(Cat_1 = if_else(db$categoria==1, 1, 0))

db <- db %>% 
  mutate(Cat_2 = if_else(db$categoria==2, 1, 0))

db <- db %>% 
  mutate(Cat_3 = if_else(db$categoria==3, 1, 0))

db <- db %>% 
  mutate(Cat_4 = if_else(db$categoria==4, 1, 0))

db <- db %>% 
  mutate(Cat_5 = if_else(db$categoria==5, 1, 0))

db <- db %>% 
  mutate(Cat_6 = if_else(db$categoria==6, 1, 0))

db <- db %>% 
  mutate(Cat_ESP = if_else(db$categoria=="ESP", 1, 0))



###############################
###############################
## Estadisticas Descriptivas

# ----------- pendiente realizar estadisticas descriptivas

### se incluyen controles de acuerdo con 3 criterios: de la politica de vivienda, de construccion, caracteristicas de municipios y capacidades municipales
db %>%
  select(VIS10MIL, proporcionareaexpansion) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"))

db %>%
  select(Defcuant2005 , IPM_urb , pobl_urb , pib_percapita , gpc , gini , pobreza , 
         nbicabecera , IPM_urb) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)")) 

db %>%
  select(IndVIS , Valorsuelo , ) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"))

db %>%
  select(diskm , disminutos , indrural , altura , dismdo, Aglomeración) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)")) 

db %>%
  select(y_total , g_total , finan , DF_desemp_fisc , DI_desemp_int , 
         indesarrollo_mun , indesarrollo_dep , inv_en_vivienda , inv_total, categoria) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)")) 


#variables y y d
ggplot () + geom_boxplot(data=db, aes(x=VIS10MIL), fill ="tomato", alpha=0.5)
ggplot () + geom_boxplot(data=db, aes(x=proporcionareaexpansion), fill ="tomato", alpha=0.5)

#
d <- ggplot(db, aes(x=VIS10MIL)) + 
  geom_density()
d+ geom_vline(aes(xintercept=mean(VIS10MIL)),
              color="steelblue", linetype="dashed", size=1.25)

d <- ggplot(db, aes(x=proporcionareaexpansion)) + 
  geom_density()
d+ geom_vline(aes(xintercept=mean(proporcionareaexpansion)),
              color="steelblue", linetype="dashed", size=1.25)
#
#y contra d
g <- ggplot(data = db , mapping = aes(x = proporcionareaexpansion , y = VIS10MIL))+
  geom_point(col = "tomato" , size = 3)
g

box_plot <- ggplot(data=db , mapping = aes(x=VIS10MIL , y=proporcionareaexpansion)) + 
  geom_boxplot()
box_plot

#habilitacion de suelo contra capacidad de los municipios
box_plot <- ggplot(data=db , mapping = aes(as.factor(categoria) , proporcionareaexpansion)) + 
  geom_boxplot() 
box_plot

#construccion de vivienda contra capacidad de los municipios
box_plot <- ggplot(data=db , mapping = aes(as.factor(categoria) , VIS10MIL)) + 
  geom_boxplot() 
box_plot

#####################
# 3. Estimacion
#####################

#Verifico los nombres de todas las variables disponibles
colnames(db)

#(actualizar)
#[1] "codmpio"                 "DEPARTAMENTO"            "MUNICIPIO"               "Aglomeración"            "diskm"                  
#[6] "disminutos"              "AÑO_orig"                "ULT_POT"                 "Hogares2005"             "Defhab2005"             
#[11] "Defcuant2005"            "Defcuali2005"            "VIS"                     "IndVIS"                  "Totalsueloexpansion"    
#[16] "areainicialurbana"       "proporcionareaexpansion" "AVALÚOURBANO"            "Valorsuelo"              "pobl_urb"               
#[21] "pobl_tot"                "indrural"                "altura"                  "pib_percapita"           "gpc"                    
#[26] "gini"                    "pobreza"                 "nbicabecera"             "IPM_urb"                 "Aglo"                   
#[31] "POB10mil"                "VIS10MIL"                "dismdo"                  "y_total"                 "g_total"                
#[36] "finan"                   "DF_desemp_fisc"          "DI_desemp_int"           "indesarrollo_mun"        "indesarrollo_dep"       
#[41] "inv_en_vivienda"         "inv_total"               "categoria"  



###############Seleccionamos las mas relevantes para nuestro analisis

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



########## LO QUE VAMOS A HACER ES PLANTEAR VARIAS ESPECIFICACIONES PARA VER COMO SE COMPORTA LA METODOLOGIA


############################################################################################################

### A) VERSION ORIGINAL DE LO ANALIZADO PREVIAMENTE (POCAS VARIABLES)


db_a <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, IndVIS, Valorsuelo ))
cor(db_a)






dbs_2 <- select(filter(db),c(diskm , disminutos , Defhab2005 , Defcuant2005 , Defcuali2005 , IndVIS , 
                            proporcionareaexpansion , Valorsuelo , pobl_urb , indrural , altura , pib_percapita , gpc , gini , pobreza , 
                            nbicabecera , IPM_urb , VIS10MIL , dismdo , y_total , g_total , finan , DF_desemp_fisc , DI_desemp_int , 
                            indesarrollo_mun , indesarrollo_dep , inv_en_vivienda , inv_total ))

cor(dbs_2) #matriz de correlaciones









#################### 

#y_sub10 <- dbs [,27, drop=F] #variable y de subsidios por cada 10mil habitantes

#y_vis10 <- dbs [,21, drop=F] #variable y de vis por cada 10mil habitantes

#d_ex <- dbs [,10, drop=F] #variable "tratamiento" (aumento suelo expansion)

#xs <- as.matrix(dbs)[,-c(27,26,15)] #matriz del resto de variables 
#xs <- as.matrix(dbs)[,-c(21,10)] #matriz del resto de variables 

#varnames <- colnames(dbs)






############################
############################

#y_sub10 <- dbs [,27, drop=F] #variable y de subsidios por cada 10mil habitantes
y_vis10_2 <- dbs_2 [,18, drop=F] #variable y de vis por cada 10mil habitantes

d_ex_2 <- dbs_2 [,7, drop=F] #variable "tratamiento" (aumento suelo expansion)

#xs <- as.matrix(dbs)[,-c(27,26,15)] #matriz del resto de variables 
xs_2 <- as.matrix(dbs_2)[,-c(18,7)] #matriz del resto de variables 

varnames_2 <- colnames(dbs_2)

#####################
# First:  Estimate by OLS

#xnames <- varnames [-c(27,26,15)]
#xnames <- varnames [-c(21,10)]

#dandxnames <- varnames [-c(27,26,15)]
#dandxnames <- varnames [-c(21)]

#fmla_sub <- as.formula (paste ("SUB10MIL ~ ", paste(dandxnames, collapse= "+")))
#fmla_sub <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames, collapse= "+")))



#ls_effect_sub <- lm (fmla_sub, data = dbs) #AQUI SI CORRE, pero hay que solucionarle los NAs

#summary(ls_effect_sub)

##################################################
##################################################
##################################################
##################################################

#xnames <- varnames [-c(27,26,15)]
xnames_2 <- varnames_2 [-c(18,7)]

#dandxnames <- varnames [-c(27,26,15)]
dandxnames_2 <- varnames_2 [-c(18)]

#fmla_sub <- as.formula (paste ("SUB10MIL ~ ", paste(dandxnames, collapse= "+")))
fmla_sub_2 <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_2, collapse= "+")))

ls_effect_sub_2 <- lm (fmla_sub_2, data = dbs_2) #AQUI SI CORRE

summary(ls_effect_sub_2) #recordar que esto es por OLS

#####################
# Second:  Estimate the effect by the partialling out by Post-Lasso

lasso.effect <- rlassoEffect(x=xs_2, y=y_vis10_2, d=d_ex_2, method= "partialling out") #pendiente segun respuesta de ignacio
summary(lasso.effect)

####################
# Third:  Estimate the effect by the double selection method

doublesel.effect <- rlassoEffect(x=xs_2, y=y_vis10_2, d=d_ex_2, method= "double selection") #pendiente segun respuesta de ignacio
summary(doublesel.effect)


####################
# Collect results

install.packages("xtable")
library(xtable)
table = rbind(summary(ls_effect_sub_2)$coef["proporcionareaexpansion", 1:2], summary(lasso.effect)$coef[, 1:2], 
              summary(doublesel.effect)$coef[, 1:2]) 
colnames(table) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab = xtable(table, digits = c(2, 2, 5))

tab

##PENDIENTES

# 1 - terminar codigo (hoy) - Sara >> buenas noticias: corre el codigo, malas noticias: nada nos da significativo, no se si correr solo con un par mas, no tantas ##ahorita vemos 
#_______ Ya, pendiente cacharrearle y ver que pasa con las significancias


# 2 - Sara - variable pablo querubin (2011)  
# 2 - Camila - variable dummy pot           (mañana primera hora)
#________ Ya

# 2.5 dummies de aglomeracion y categoria



# 3 - correr (asumamos que sale bien todo) - Sara
# 3 - montar rapidamente borrador de paper - Camila

# 4 - hacer estadisticas descriptivas y nutrir el paper - Sara
# 4 - esto puede incluir mapas                          - Camila (para anexos)

# 5 - Consolidar y enviar


# Otros pendientes

# - preguntarle a Ignacio lo de aglomeracion

# si nos sobra el tiempo hacer causal trees




