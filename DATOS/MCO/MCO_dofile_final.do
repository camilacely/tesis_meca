

clear
clear all

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\DATOS\MCO"

use "Aglo"

rename codigo_dane Codigodane

merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "indHHI"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "Valor2"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "Deficit"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "Afinidad"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "AREAEXPANSION"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "POT A"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "VIS"
drop _merge
drop if missing(Codigodane)
drop if missing(pobl_urb)

encode Aglomeración, gen(Aglo)
encode Ejesregionales, gen(eje)

gen POB10mil= pobl_urb/10000
gen VIS10MIL= VIS/POB10mil

****Estadística descriptiva
outreg2 using estdesc.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 IndVIS Valorsuelo)

outreg2 using estdescsub.doc, replace sum(log) keep(proporcionareaexpansion IPM_urb Defcuant2005 Indsub Valorsuelo)

*****regresiones con VIS

reg VIS10MIL proporcionareaexpansion
estimates store modelo_1
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005
estimates store modelo_2
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 valorsuelourb 
estimates store modelo_3
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 valorsuelourb ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor
estimates store modelo_4
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 valorsuelourb ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo
estimates store modelo_5

*Incluyendo EF por aglomeracion en todos 
reg VIS10MIL proporcionareaexpansion i.Aglo
estimates store modelo_6
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 i.Aglo
estimates store modelo_7
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 valorsuelourb i.Aglo
estimates store modelo_8
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 valorsuelourb ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo
estimates store modelo_9

**Doble selection lasso
dsregress VIS10MIL proporcionareaexpansion, controls(IPM_urb Defcuant2005 HHI2013 valorsuelourb ULTIMOPOT MODEXCEPCIONAL left_mayor right_mayor other_mayor unknown_mayor i.Aglo)
ereturn list

reg VIS10MIL proporcionareaexpansion Defcuant2005














