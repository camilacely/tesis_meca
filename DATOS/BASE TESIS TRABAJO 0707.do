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

**MERGE CON INFO DEL PANEL DEL CEDE
merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge==2
drop _merge

****Estadística descriptiva
outreg2 using estdesc.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo)

outreg2 using estdescsub.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 Indsub Valorsuelo)

encode Aglomeración, gen(Aglo)

*****regresiones con VIS

gen POB10mil= pobl_urb/10000
gen VIS10MIL= VIS/POB10mil
gen SUB10MIL = subvis/POB10mil

*numero de viviendas VIS per cápita

reg VIS10MIL proporcionareaexpansion
estimates store modelo_1
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005
estimates store modelo_2
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo
estimates store modelo_3
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo i.Aglo
estimates store modelo_4

reg SUB10MIL proporcionareaexpansion if Indsub!=.
estimates store modelo_4
reg SUB10MIL proporcionareaexpansion IPM_urb Defcuant2005 if Indsub!=.
estimates store modelo_5
reg SUB10MIL proporcionareaexpansion IPM_urb Defcuant2005 Indsub Valorsuelo if Indsub!=.
estimates store modelo_6
reg SUB10MIL proporcionareaexpansion IPM_urb Defcuant2005 Indsub Valorsuelo i.Aglo if Indsub!=.
estimates store modelo_7

outreg2 [modelo_1 modelo_2 modelo_3 modelo_4] using "estimaciones0807.doc", replace
outreg2 [modelo_4 modelo_5 modelo_6 modelo_7] using "estimaciones08072.doc", replace


