**BASE

clear
clear all

*GUARDAR LA BASE DE AGLOMERACIONES EN STATA
import excel "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\Aglomeraciones F.xlsx", sheet("Consulta1") firstrow

save "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\AGLOMERACIONES 2.dta", replace

*TRAER LA DE PROPORCIONES
import excel "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\PROPORCION 2.xlsx", sheet("Sheet1") firstrow clear

*MERGE
rename COD codigo_dane

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2"

merge m:m codigo_dane using "AGLOMERACIONES 2"
*Como resultado del merge se puede ver que Magangue, San vicente del caguan, santander de quilichao, aguachica, cerete, lorica y pitalito no están en el sistema de ciudades. 

*mas grave: para 90 de los municipios en el sistema de ciudades no existen en la base de crecimiento. 

**OBSERVACIONES TOTALES=61

*DROPEAR LAS OBSERVACIONES QUE NO TIENEN VALORES
drop if _merge!=3

drop _merge
drop Municipio

merge m:m MUNICIPIO DEPARTAMENTO using "MINVIVIENDA POR MUNICIPIO"

*Hay 3 de los municipios que no se asignaron subsidios: Cienaga (Magdalena), Puerto Colombia(Atlántico), Tumaco (Nariño)

*mas grave: hay 241 municipios donde se asignaron subsidios que no está en la base de crecimiento

*OBSERVACIONES TOTALES=58

drop if _merge!=3
drop _merge

*CREAR LA VARIABLE DE TODOS LOS SUBSIDIOS SUMADOS
egen sub=rowtotal(HASTA2SMMLV SUPERIORESA2SMMLVYHASTA3S SUPERIORESA2SMMLVYHASTA4S SUPERIORESA3SMMLVYHASTA4S)

*JUNTAR CON DEFICIT HAB
rename codigo_dane Codigodane
merge m:m Codigodane using "DEFICIT"
drop if _merge!=3

*POR AHORA VOY A QUITAR LOS MUNICIPIOS PARA LOS QUE PROPORCION=0 PORQUE CREO QUE ESE VALOR DE 0 PUEDE GENERAR UN SESGO GRANDE: Manizales, sogamoso y Riohacha
drop if PROPORCIONCRECIMIENTO==0

*JUNTAR BASE DE VIS POR MUNICIPIO 
drop _merge
merge m:m MUNICIPIO using "VIS"
drop _merge

////////////////////////////////////////////
*MERGE CON INDICE CONSTUCTORES, TENGO EL DO DE COMO LO HICE PARA CUANDO TENGAMOS MAS VALORES
merge m:m MUNICIPIO using "INDICE CONSTRUCTORES"
label var sub "total subsidios por municipio"
label var ind1 "Índice constructores por municipio"
drop _merge
drop if _merge==2
rename ind1 Indsub

**MERGE CON INDICE CONSTRUCTORES VIVIENDAS VIS
merge m:m MUNICIPIO using "INDICE CONSTRUCTORES VIS"
drop _merge
drop if _merge==2

**MERGE CON INFO DEL PANEL DEL CEDE
merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge==2
drop _merge

**MERGE CON PARTIDOS POLÍTICOS
merge m:m codmpio using "GOB"
drop if _merge==2

**GENERAR PER 10.000 
gen POB10mil= pobl_urb/10000
gen POB10mil= pobl_urb

**REGRESIONES 
gen VIS10MIL= VIS/POB10mil
gen VISpob= VIS/pobl_urb

reg VIS AREAEXPT2T4
estimates store modelos_1

reg VISpob AREAEXPT2T4
estimates store modelos_2

reg VIS PROPORCIONCRECIMIENTO
estimates store modelos_3
reg VIS10MIL PROPORCIONCRECIMIENTO
estimates store modelos_4

outreg2 [modelos_1 modelos_2 modelos_3 modelos_4] using "estiniciales.doc", replace






