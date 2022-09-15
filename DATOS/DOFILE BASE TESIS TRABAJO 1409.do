**BASE SOLO VIS

clear
clear all

import excel "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\Aglomeraciones F.xlsx", sheet("Consulta1") firstrow
cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2"

rename Municipio MUNICIPIO

merge m:m MUNICIPIO DEPARTAMENTO using "MINVIVIENDA POR MUNICIPIO"

drop _merge

egen subtotal=rowtotal(HASTA2SMMLV SUPERIORESA2SMMLVYHASTA3S SUPERIORESA2SMMLVYHASTA4S SUPERIORESA3SMMLVYHASTA4S)

egen subvis=rowtotal(SUPERIORESA3SMMLVYHASTA4S SUPERIORESA2SMMLVYHASTA4S SUPERIORESA2SMMLVYHASTA3S)

rename codigo_dane Codigodane

merge m:m Codigodane using "DEFICIT"
 
drop if _merge!=3

drop _merge

merge m:m MUNICIPIO using "VIS"

drop _merge

drop if missing(Codigodane)

merge m:m MUNICIPIO using "INDICE CONSTRUCTORES"
label var ind1 "Índice constructores por municipio"
drop if _merge==2
drop _merge
rename ind1 Indsub

**MERGE CON INDICE CONSTRUCTORES VIVIENDAS VIS
merge m:m MUNICIPIO using "INDICE CONSTRUCTORES VIS"
drop if _merge==2
drop _merge

**CRECIMIENTO
rename Codigodane COD
merge m:m COD using "AREAEXPANSION"
drop if _merge!=3
drop _merge

destring COD, gen(Codigodane)

**VALOR DE SUELO
merge m:m Codigodane using "VALORSUELO"
drop if _merge==2
drop _merge
replace Valorsuelo = 875064.8683218 in 1
**aqui ajusté el valor de medellin 
gen Valorsuelo_e = Valorsuelo/100

**MERGE CON INFO DEL PANEL DEL CEDE
merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge==2
drop _merge


**SOLO VIS
drop if VIS==0
drop if VIS==.
drop AÑO_orig AÑO_gen AÑO_mod AÑO_MVCT ULT_POT

**MERGE CON INFO POT ACTUALIZADA
merge m:m MUNICIPIO using "POT A"
 drop in 53
 replace ULTIMOPOT = 2013 in 52
 replace MODEXCEPCIONAL = 1 in 52
drop _merge

**MERGE CON INFO PARTIDOS
merge m:m Codigodane using "PARTIDOSP"
drop _merge


****Estadística descriptiva
outreg2 using estdesc.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo)

outreg2 using estdescsub.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 Indsub Valorsuelo)

encode Aglomeración, gen(Aglo)

*****regresiones con VIS

gen POB10mil= pobl_urb/10000
gen VIS10MIL= VIS/POB10mil
gen SUB10MIL = subvis/POB10mil

**** Tope 150 SMMLV
gen Tope150=1 if Codigodane==5001
replace Tope150 = 0 in 2
replace Tope150 = 1 in 3
replace Tope150 = 1 in 4
replace Tope150 = 1 in 5
replace Tope150 = 0 in 6
replace Tope150 = 1 in 7
replace Tope150 = 1 in 8
replace Tope150 = 0 in 9
replace Tope150 = 1 in 10
replace Tope150 = 1 in 11
replace Tope150 = 1 in 12
replace Tope150 = 1 in 13
replace Tope150 = 0 in 14
replace Tope150 = 0 in 15
replace Tope150 = 0 in 16
replace Tope150 = 0 in 17
replace Tope150 = 0 in 18
replace Tope150 = 0 in 19
replace Tope150 = 0 in 20
replace Tope150 = 0 in 21
replace Tope150 = 1 in 21
replace Tope150 = 1 in 22
replace Tope150 = 0 in 23
replace Tope150 = 0 in 24
replace Tope150 = 0 in 25
replace Tope150 = 1 in 26
replace Tope150 = 1 in 27
replace Tope150 = 1 in 28
replace Tope150 = 0 in 29
replace Tope150 = 0 in 30
replace Tope150 = 0 in 31
replace Tope150 = 0 in 32
replace Tope150 = 0 in 33
replace Tope150 = 0 in 34
replace Tope150 = 0 in 35
replace Tope150 = 0 in 36
replace Tope150 = 0 in 37
replace Tope150 = 0 in 39
replace Tope150 = 1 in 38
replace Tope150 = 1 in 40
replace Tope150 = 1 in 41
replace Tope150 = 1 in 42
replace Tope150 = 0 in 43
replace Tope150 = 0 in 44
replace Tope150 = 1 in 45
replace Tope150 = 0 in 46
replace Tope150 = 0 in 47
replace Tope150 = 0 in 48
replace Tope150 = 1 in 49
replace Tope150 = 0 in 50
replace Tope150 = 0 in 51
replace Tope150 = 1 in 52
br if Tope150==.
drop if Codigodane==.

*numero de viviendas VIS per cápita

*solo incluyendo EF por aglomeración al final
reg VIS10MIL proporcionareaexpansion
estimates store modelo_1
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005
estimates store modelo_2
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo_e 
estimates store modelo_3
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo_e ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor
estimates store modelo_4
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo_e ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo
estimates store modelo_5

*Incluyendo EF por aglomeracion en todos 
reg VIS10MIL proporcionareaexpansion i.Aglo
estimates store modelo_6
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 i.Aglo
estimates store modelo_7
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo_e i.Aglo
estimates store modelo_8
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo_e ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo
estimates store modelo_9

**Doble selection lasso
dsregress VIS10MIL proporcionareaexpansion, controls(IPM_urb Defcuant2005 IndVIS Valorsuelo ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo)
ereturn list

reg VIS10MIL proporcionareaexpansion Defcuant2005


outreg2 [modelo_1 modelo_2 modelo_3 modelo_4 modelo_5] using "estimaciones1608.doc", replace
outreg2 [modelo_6 modelo_7 modelo_8 modelo_9] using "estimaciones16082.doc", replace


