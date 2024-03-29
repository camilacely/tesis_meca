
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
       hdm,
       xtable,
       sf,
       gtsummary,
       ggpubr)

##############################
##############################
## Directorio

setwd("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS")
#setwd("C:/Users/Camila Cely/Documents/GitHub/tesis_meca")


###############################
###############################
## Importacion de Stata

#la base la veniamos trabajando en stata, por lo tanto la vamos a importar

# # input Stata file
#library(haven)
BASE_TESIS_1409 <- read_dta("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/DATOS/BASE TESIS 1409.dta")
View(BASE_TESIS_1409)
# 
# 
export(BASE_TESIS_1409,"C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/stores/BASE_TESIS2.rds")

###############################
###############################
## Importar la base final a R

db <-readRDS("C:/Users/SARA/Documents/ESPECIALIZACIÓN/BIG DATA/GITHUB/tesis_meca/stores/BASE_TESIS2.Rds") 
summary (db)
#no tenemos NA


# install.packages("writexl")
# library("writexl")
# write_xlsx(db,"stores/db_prueba.xlsx")


#####################
# 2. Limpieza y estadisticas descriptivas
#####################

db %>%
  select(VIS10MIL) %>%
  tbl_summary()

summary(db$VIS10MIL)

db %>%
  select(VIS10MIL, Aglo, proporcionareaexpansion, Defhab2005, IPM_urb ) %>%
  tbl_summary()

### ANALISIS DE CORRELACIONES

colnames(db)

ggplot(db, aes(x=Defcuant2005, y=VIS10MIL)) + geom_point()

ggplot(db, aes(x=Valorsuelo, y=VIS10MIL)) + geom_point()

ggplot(db, aes(x=IPM_urb, y=VIS10MIL)) + geom_point()

ggplot(db, aes(x=proporcionareaexpansion, y=VIS10MIL)) + geom_point() 

# install.packages("ggpubr")
# library("ggpubr")
ggscatter(db, x = "Defhab2005", y = "VIS10MIL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Deficit habitacional", ylab = "N° VIS por cada 10.000 hab")

ggscatter(db, x = "Defcuant2005", y = "VIS10MIL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Deficit cuantitativo", ylab = "N° VIS por cada 10.000 hab")


ggscatter(db, x = "IPM_urb", y = "VIS10MIL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Indice de Pobreza Multidimensional Urbano", ylab = "N° VIS por cada 10.000 hab")


ggscatter(db, x = "Valorsuelo", y = "VIS10MIL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Valor del suelo", ylab = "N° VIS por cada 10.000 hab")


ggscatter(db, x = "proporcionareaexpansion", y = "VIS10MIL", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Proporcion area expansion", ylab = "N° VIS por cada 10.000 hab")



####
ggplot(db, aes(x=Defcuant2005, y=VIS10MIL)) + geom_point()

ggplot(db, aes(x=Defcuant2005, y=VIS10MIL)) + geom_point() +
  geom_text(data = db, aes(Defcuant2005,VIS10MIL, label = MUNICIPIO), hjust = 0.2)

ggplot(db, aes(x=Defcuali2005, y=VIS10MIL)) + geom_point()




### HISTOGRAMAS

head(db$Aglo)

db_cali <- db %>% subset(Aglo == 8) #4 obs  ##pendiente

class(db_cali$Defcuant2005)

ggplot(db_cali, aes(x=Defcuant2005)) + 
  geom_histogram()

plot(x = db_cali$Defcuant2005)



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


var_lab(db$y_total) = "Ingresos Totales"
var_lab(db$g_total) = "gastos Totales"
var_lab(db$finan) = "Financiamiento - creditos"
var_lab(db$DF_desemp_fisc) = "Indicador de desempeño fiscal"
var_lab(db$DI_desemp_int) = "Indicador de desempeño integral"
var_lab(db$inv_en_vivienda) = "Inversión en vivienda"
var_lab(db$inv_total) = "Inversión total"
var_lab(db$diskm) = "Distancia lineal a la capital del departamento"
var_lab(db$disminutos) = "Distancia en minutos a la capital del departamento"
var_lab(db$altura) = "Altura del municipio - MSNM"
var_lab(db$IndVIS) = "Indicador de concentración de constructores de proyectos VIS"
var_lab(db$Defcuant2005) = "Déficit cuantitativo en 2005"
var_lab(db$pobl_urb) = "Población Urbana en 2005"
var_lab(db$gini) = "Índice de gini municipal"
var_lab(db$nbicabecera) = "Necesidades Básicas Insatisfechas 2005 cabecera municipal"
var_lab(db$VIS10MIL) = "Viviendas de Interés Social cada 10mil habitantes"
var_lab(db$proporcionareaexpansion) = "Proporción area habilitada para suelo de expansión entre 2005 y 2020"




###############################
###############################
## Estadisticas Descriptivas



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
box_plot <- ggplot(data=db , mapping = aes(categoria , proporcionareaexpansion)) + 
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

# [1] "codmpio"                 "DEPARTAMENTO"            "MUNICIPIO"               "Aglomeración"            "diskm"                  
# [6] "disminutos"              "AÑO_orig"                "ULT_POT"                 "Hogares2005"             "Defhab2005"             
# [11] "Defcuant2005"            "Defcuali2005"            "VIS"                     "IndVIS"                  "Totalsueloexpansion"    
# [16] "areainicialurbana"       "proporcionareaexpansion" "AVALÚOURBANO"            "Valorsuelo"              "pobl_urb"               
# [21] "pobl_tot"                "indrural"                "altura"                  "pib_percapita"           "gpc"                    
# [26] "gini"                    "pobreza"                 "nbicabecera"             "IPM_urb"                 "Aglo"                   
# [31] "POB10mil"                "VIS10MIL"                "dismdo"                  "y_total"                 "g_total"                
# [36] "finan"                   "DF_desemp_fisc"          "DI_desemp_int"           "indesarrollo_mun"        "indesarrollo_dep"       
# [41] "inv_en_vivienda"         "inv_total"               "categoria"               "pot_exc"                 "A_Apartado"             
# [46] "A_Armenia"               "A_Barrancabermenja"      "A_Barranquilla"          "A_Bogota"                "A_Bucaramanga"          
# [51] "A_Buenaventura"          "A_Cali"                  "A_Cartagena"             "A_Cartago"               "A_Cucuta"               
# [56] "A_Duitama"               "A_Fusagasuga"            "A_Girardot"              "A_Buga"                  "A_Ibague"               
# [61] "A_Manizales"             "A_Medellin"              "A_Monteria"              "A_Neiva"                 "A_Pasto"                
# [66] "A_Pereira"               "A_Popayan"               "A_Rionegro"              "A_SantaMarta"            "A_Sincelejo"            
# [71] "A_Sogamoso"              "A_Tulua"                 "A_Tunja"                 "A_Valledupar"            "A_Villavicencio"        
# [76] "Cat_1"                   "Cat_2"                   "Cat_3"                   "Cat_4"                   "Cat_5"                  
# [81] "Cat_6"                   "Cat_ESP"  



###########Analizar cuales pueden ser intuitivamente las mas relevantes para nuestro analisis

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

### A) VERSION ORIGINAL DE LO ANALIZADO PREVIAMENTE (POCAS VARIABLES) -- esta intuicion ya la habiamos trabajado previamente en stata 


db_a <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, IndVIS, Valorsuelo ))
cor(db_a)

y_a <- db_a [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_a <- db_a [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_a <- as.matrix(db_a)[,-c(1,2)] #matriz del resto de variables 

varnames_a <- colnames(db_a)

##
# First:  Estimate by OLS
xnames_a <- varnames_a [-c(1,2)]
dandxnames_a <- varnames_a [-c(1)]

fmla_a <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_a, collapse= "+")))

ls_effect_a <- lm (fmla_a, data = db_a)

summary(ls_effect_a) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_a <- rlassoEffect(x=x_a, y=y_a, d=d_a, method= "partialling out") 
summary(lasso_effect_a)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_a <- rlassoEffect(x=x_a, y=y_a, d=d_a, method= "double selection") 
summary(doublesel_effect_a)


##
# Collect results

table_a = rbind(summary(ls_effect_a)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_a)$coef[, 1:2], 
              summary(doublesel_effect_a)$coef[, 1:2]) 
colnames(table_a) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_a) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_a = xtable(table_a, digits = c(2, 2, 5))
tab_a


############################################################################################################

### B) QUE PASA SI CORREMOS LO MISMO PERO SIN INDVIS

db_b <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, Valorsuelo ))
cor(db_b)

y_b <- db_b [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_b <- db_b [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_b <- as.matrix(db_b)[,-c(1,2)] #matriz del resto de variables 

varnames_b <- colnames(db_b)

##
# First:  Estimate by OLS
xnames_b <- varnames_b [-c(1,2)]
dandxnames_b <- varnames_b [-c(1)]

fmla_b <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_b, collapse= "+")))

ls_effect_b <- lm (fmla_b, data = db_b)

summary(ls_effect_b) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_b <- rlassoEffect(x=x_b, y=y_b, d=d_b, method= "partialling out") 
summary(lasso_effect_b)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_b <- rlassoEffect(x=x_b, y=y_b, d=d_b, method= "double selection") 
summary(doublesel_effect_b)


##
# Collect results

table_b = rbind(summary(ls_effect_b)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_b)$coef[, 1:2], 
                summary(doublesel_effect_b)$coef[, 1:2]) 
colnames(table_b) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_b) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_b = xtable(table_b, digits = c(2, 2, 5))
tab_b



############################################################################################################

### C) IGUAL A VERSION A, PERO CONTROLANDO POR AGLOMERACION (EFECTO FIJO = DUMMY)

db_c <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, IndVIS, Valorsuelo,
                            A_Apartado, A_Armenia  , A_Barrancabermenja,  A_Barranquilla, A_Bogota, A_Bucaramanga,
                            A_Buenaventura, A_Cali, A_Cartagena, A_Cartago,  A_Cucuta, A_Duitama, A_Fusagasuga, A_Girardot,
                            A_Buga,  A_Ibague, A_Manizales, A_Medellin,  A_Monteria,  A_Neiva, A_Pasto, A_Pereira, A_Popayan,
                            A_Rionegro, A_SantaMarta,  A_Sincelejo, A_Sogamoso, A_Tulua  , A_Tunja, A_Valledupar, A_Villavicencio  ))
cor(db_c)


y_c <- db_c [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_c <- db_c [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_c <- as.matrix(db_c)[,-c(1,2)] #matriz del resto de variables 

varnames_c <- colnames(db_c)

##
# First:  Estimate by OLS
xnames_c <- varnames_c [-c(1,2)]
dandxnames_c <- varnames_c [-c(1)]

fmla_c <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_c, collapse= "+")))

ls_effect_c <- lm (fmla_c, data = db_c)

summary(ls_effect_c) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_c <- rlassoEffect(x=x_c, y=y_c, d=d_c, method= "partialling out") 
summary(lasso_effect_c)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_c <- rlassoEffect(x=x_c, y=y_c, d=d_c, method= "double selection") 
summary(doublesel_effect_c)


##
# Collect results

table_c = rbind(summary(ls_effect_c)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_c)$coef[, 1:2], 
                summary(doublesel_effect_c)$coef[, 1:2]) 
colnames(table_c) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_c) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_c = xtable(table_c, digits = c(2, 2, 5))
tab_c


############################################################################################################

### D) IGUAL A VERSION A, PERO CONTROLANDO POR CATEGORIA (EFECTO FIJO = DUMMY)

db_d <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, IndVIS, Valorsuelo,
                            Cat_1, Cat_2, Cat_3,  Cat_4, Cat_5, Cat_6, Cat_ESP))
cor(db_d)


y_d <- db_d [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_d <- db_d [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_d <- as.matrix(db_d)[,-c(1,2)] #matriz del resto de variables 

varnames_d <- colnames(db_d)

##
# First:  Estimate by OLS
xnames_d <- varnames_d [-c(1,2)]
dandxnames_d <- varnames_d [-c(1)]

fmla_d <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_d, collapse= "+")))

ls_effect_d <- lm (fmla_d, data = db_d)

summary(ls_effect_d) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_d <- rlassoEffect(x=x_d, y=y_d, d=d_d, method= "partialling out") 
summary(lasso_effect_d)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_d <- rlassoEffect(x=x_d, y=y_d, d=d_d, method= "double selection") 
summary(doublesel_effect_d)


##
# Collect results

table_d = rbind(summary(ls_effect_d)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_d)$coef[, 1:2], 
                summary(doublesel_effect_d)$coef[, 1:2]) 
colnames(table_d) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_d) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_d = xtable(table_d, digits = c(2, 2, 5))
tab_d


############################################################################################################

### E) SIN EFECTOS FIJOS, PERO CON TODAS LAS VARIABLES DE LA BASE

db_e <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, diskm , disminutos , Defhab2005 , Defcuant2005 , Defcuali2005 , IndVIS , 
                             Valorsuelo , pobl_urb  , altura , pib_percapita , gpc , gini , pobreza , 
                             nbicabecera , IPM_urb , dismdo , y_total , g_total , finan , DF_desemp_fisc , DI_desemp_int , 
                             indesarrollo_mun , indesarrollo_dep , inv_en_vivienda , inv_total , pot_exc )) #indrural
cor(db_e)




y_e <- db_e [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_e <- db_e [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_e <- as.matrix(db_e)[,-c(1,2)] #matriz del resto de variables 

varnames_e <- colnames(db_e)

##
# First:  Estimate by OLS
xnames_e <- varnames_e [-c(1,2)]
dandxnames_e <- varnames_e [-c(1)]

fmla_e <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_e, collapse= "+")))

ls_effect_e <- lm (fmla_e, data = db_e)

summary(ls_effect_e) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_e <- rlassoEffect(x=x_e, y=y_e, d=d_e, method= "partialling out") 
summary(lasso_effect_e)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_e <- rlassoEffect(x=x_e, y=y_e, d=d_e, method= "double selection") 
summary(doublesel_effect_e)


##
# Collect results

table_e = rbind(summary(ls_effect_e)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_e)$coef[, 1:2], 
                summary(doublesel_effect_e)$coef[, 1:2]) 
colnames(table_e) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_e) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_e = xtable(table_e, digits = c(2, 2, 5))
tab_e


############################################################################################################

### F) CON "UNA SOLA" DE LAS VARIABLES QUE MIDEN COSAS PARECIDAS (APROXIMACION INTUITIVA)

db_f <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, diskm , Defcuant2005 , Defcuali2005 , IndVIS , 
                          Valorsuelo , pobl_urb  , altura , pib_percapita , IPM_urb , dismdo , y_total  , 
                            indesarrollo_mun ,  inv_en_vivienda , pot_exc, indrural )) 

cor(db_f)

lapply(db_f, class)



y_f <- db_f [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_f <- db_f [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_f <- as.matrix(db_f)[,-c(1,2)] #matriz del resto de variables 

varnames_f <- colnames(db_f)

##
# First:  Estimate by OLS
xnames_f <- varnames_f [-c(1,2)]
dandxnames_f <- varnames_f [-c(1)]

fmla_f <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_f, collapse= "+")))

ls_effect_f <- lm (fmla_f, data = db_f)

summary(ls_effect_f) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_f <- rlassoEffect(x=x_f, y=y_f, d=d_f, method= "partialling out") 
summary(lasso_effect_f)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_f <- rlassoEffect(x=x_f, y=y_f, d=d_f, method= "double selection") 
summary(doublesel_effect_f)


##
# Collect results

table_f = rbind(summary(ls_effect_f)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_f)$coef[, 1:2], 
                summary(doublesel_effect_f)$coef[, 1:2]) 
colnames(table_f) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_f) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_f = xtable(table_f, digits = c(2, 2, 5))
tab_f

############################################################################################################

### G) CON LAS VARIABLES ACTUALIZADAS

db_g <- select(filter(db),c(VIS10MIL, proporcionareaexpansion, IPM_urb, Defcuant2005, IndVIS, Valorsuelo, 
                            A_Barranquilla, A_Bogota, A_Bucaramanga, A_Cali, A_Cartagena,  A_Medellin))

cor(db_g)

lapply(db_g, class)



y_g <- db_g [,1, drop=F] #variable y de vis por cada 10mil habitantes
d_g <- db_g [,2, drop=F] #variable "tratamiento" (aumento suelo expansion)
x_g <- as.matrix(db_g)[,-c(1,2)] #matriz del resto de variables 

varnames_g <- colnames(db_g)

##
# First:  Estimate by OLS
xnames_g <- varnames_g [-c(1,2)]
dandxnames_g <- varnames_g [-c(1)]

fmla_g <- as.formula (paste ("VIS10MIL ~ ", paste(dandxnames_g, collapse= "+")))

ls_effect_g <- lm (fmla_g, data = db_g)

summary(ls_effect_g) 

##
# Second:  Estimate the effect by the partialling out by Post-Lasso
lasso_effect_g <- rlassoEffect(x=x_g, y=y_g, d=d_g, method= "partialling out") 
summary(lasso_effect_g)


##
# Third:  Estimate the effect by the double selection method
doublesel_effect_g <- rlassoEffect(x=x_g, y=y_g, d=d_g, method= "double selection") 
summary(doublesel_effect_g)


##
# Collect results

table_g = rbind(summary(ls_effect_g)$coef["proporcionareaexpansion", 1:2], summary(lasso_effect_g)$coef[, 1:2], 
                summary(doublesel_effect_g)$coef[, 1:2]) 
colnames(table_g) = c("Estimate", "Std. Error") #names(summary(full.fit)£coef)[1:2]
rownames(table_g) = c("full reg via ols", "partial reg
via post-lasso ", "partial reg via double selection")
tab_g = xtable(table_g, digits = c(2, 2, 5))
tab_g


#####################
# 4. Visualizacion
#####################

# Adicionalmente, vamos a ver como se comportan los proyectos VIS en aglomeraciones especificas de acuerdo con algunas de las variables vistas


#base municipios colombia

mun <-read_sf("stores/Municipios/Municipios.shp")
mun <- st_transform(mun, 4326)



leaflet() %>% addTiles() %>% addPolygons(data=mun) #se demora un poco en cargar porque son todos los municipios del pais


#base proyectos VIS

coord_vis<-haven::read_dta("stores/coord_proyectos_vis.dta")
coord_vis<-st_as_sf(coord_vis, coords=c("Longitud", "Latitud"))
coord_vis <- st_set_crs(coord_vis, "+proj=longlat +datum=WGS84")
coord_vis <- st_transform(coord_vis, 4326)

leaflet() %>% addTiles() %>% addPolygons(data=mun, fill=NA)  %>% addCircleMarkers(data=coord_vis, col="red")


db_mun <- select(filter(db),c(codmpio, VIS10MIL, proporcionareaexpansion )) 

db_mun <- db_mun %>% mutate (COD_MUNIC = codmpio )

mun <- mun %>% mutate (COD_MUNIC= as.numeric (mun$COD_MUNIC))

db_mun <- left_join(db_mun, mun)

summary(db_mun)
summary(mun)

class(db_mun)
class(mun)

db_mun <- st_as_sf (db_mun)

plot(db_mun)



leaflet() %>% addTiles() %>% addPolygons(
    data= db_mun, color= "blue", fillOpacity=0.5, opacity = 0.5)

#######
# vamos a intentar ver la relacion entre suelo habilitado y vis por cada 10mil habitantes en mapas paralelos


#### En este mapa 1 veremos los municipios con mayor proporcion de suelo habilitado

summary (db_mun$proporcionareaexpansion)

upper_bound_cp1<-0.2  
upper_bound_cp2<-0.4
upper_bound_cp3<-0.6
upper_bound_cp4<-0.8
upper_bound_cp5<-1


upper_bound_cp1 <- quantile(db_mun$proporcionareaexpansion, upper_bound_cp1, na.rm=T)  
upper_bound_cp1 

upper_bound_cp2 <- quantile(db_mun$proporcionareaexpansion, upper_bound_cp2, na.rm=T)  
upper_bound_cp2 

upper_bound_cp3 <- quantile(db_mun$proporcionareaexpansion, upper_bound_cp3, na.rm=T)  
upper_bound_cp3

upper_bound_cp4 <- quantile(db_mun$proporcionareaexpansion, upper_bound_cp4, na.rm=T)  
upper_bound_cp4

upper_bound_cp5 <- quantile(db_mun$proporcionareaexpansion, upper_bound_cp5, na.rm=T)  
upper_bound_cp5


db_mun <- db_mun %>% 
  mutate(cp1 = if_else( proporcionareaexpansion <= upper_bound_cp1, 1, 0))

db_mun <- db_mun %>% 
  mutate(cp2 = if_else( proporcionareaexpansion <= upper_bound_cp2, 1, 0))
db_mun <- db_mun %>% 
  mutate(cp2 = if_else( proporcionareaexpansion > upper_bound_cp1, 1, 0))

db_mun <- db_mun %>% 
  mutate(cp3 = if_else( proporcionareaexpansion <= upper_bound_cp3, 1, 0))
db_mun <- db_mun %>% 
  mutate(cp3 = if_else( proporcionareaexpansion > upper_bound_cp2, 1, 0))

db_mun <- db_mun %>% 
  mutate(cp4 = if_else( proporcionareaexpansion <= upper_bound_cp4, 1, 0))
db_mun <- db_mun %>% 
  mutate(cp4 = if_else( proporcionareaexpansion > upper_bound_cp3, 1, 0))

db_mun <- db_mun %>% 
  mutate(cp5= if_else( proporcionareaexpansion <= upper_bound_cp5, 1, 0))
db_mun <- db_mun %>% 
  mutate(cp5 = if_else( proporcionareaexpansion > upper_bound_cp4, 1, 0))


colorcp <- rep(NA, nrow(db_mun))

colorcp[db_mun$cp1 == 1] <- "#CAF0F8"   
colorcp[db_mun$cp2 == 1] <- "#90E0EF"
colorcp[db_mun$cp3 == 1] <- "#00B4D8"
colorcp[db_mun$cp4 == 1] <- "#0077B6"
colorcp[db_mun$cp5 == 1] <- "#03045E"  #el mas oscuro es el que mas crecio en area de expansion

#MAPA 1
leaflet() %>% addTiles() %>% addCircleMarkers(
  data=coord_vis, color= "black", fillOpacity=1 , opacity=1, radius=1) %>% addPolygons(
    data= db_mun, color= colorcp, fillOpacity=0.5, opacity = 0.5)


#### Ahora en el mapa 2 veremos los municipios con mayor proporcion de VIS por cada 10mil habitantes

summary (db_mun$VIS10MIL)

upper_bound_c1<-0.2  
upper_bound_c2<-0.4
upper_bound_c3<-0.6
upper_bound_c4<-0.8
upper_bound_c5<-1


upper_bound_c1 <- quantile(db_mun$VIS10MIL, upper_bound_c1, na.rm=T)  
upper_bound_c1 

upper_bound_c2 <- quantile(db_mun$VIS10MIL, upper_bound_c2, na.rm=T)  
upper_bound_c2 

upper_bound_c3 <- quantile(db_mun$VIS10MIL, upper_bound_c3, na.rm=T)  
upper_bound_c3

upper_bound_c4 <- quantile(db_mun$VIS10MIL, upper_bound_c4, na.rm=T)  
upper_bound_c4

upper_bound_c5 <- quantile(db_mun$VIS10MIL, upper_bound_c5, na.rm=T)  
upper_bound_c5


db_mun <- db_mun %>% 
  mutate(c1 = if_else( VIS10MIL <= upper_bound_c1, 1, 0))

db_mun <- db_mun %>% 
  mutate(c2 = if_else( VIS10MIL <= upper_bound_c2, 1, 0))
db_mun <- db_mun %>% 
  mutate(c2 = if_else( VIS10MIL > upper_bound_c1, 1, 0))

db_mun <- db_mun %>% 
  mutate(c3 = if_else( VIS10MIL <= upper_bound_c3, 1, 0))
db_mun <- db_mun %>% 
  mutate(c3 = if_else( VIS10MIL > upper_bound_c2, 1, 0))

db_mun <- db_mun %>% 
  mutate(c4 = if_else( VIS10MIL <= upper_bound_c4, 1, 0))
db_mun <- db_mun %>% 
  mutate(c4 = if_else( VIS10MIL > upper_bound_c3, 1, 0))

db_mun <- db_mun %>% 
  mutate(c5= if_else( VIS10MIL <= upper_bound_c5, 1, 0))
db_mun <- db_mun %>% 
  mutate(c5 = if_else( VIS10MIL > upper_bound_c4, 1, 0))


colorc <- rep(NA, nrow(db_mun))

colorc[db_mun$c1 == 1] <- "#FF9B54"   
colorc[db_mun$c2 == 1] <- "#FF7F51"
colorc[db_mun$c3 == 1] <- "#CE4257"
colorc[db_mun$c4 == 1] <- "#720026"
colorc[db_mun$c5 == 1] <- "#4F000B"  #el mas oscuro es el que mas tiene VIS por cada 10 mil hab

#MAPA 2
leaflet() %>% addTiles() %>% addCircleMarkers(
  data=coord_vis, color= "black", fillOpacity=1 , opacity=1, radius=1) %>% addPolygons(
    data= db_mun, color= colorc, fillOpacity=0.5, opacity = 0.5)


########################################################################################################




