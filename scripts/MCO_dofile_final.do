

clear
clear all

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\stores\MCO - Stata"
*cd "C:\Users\Camila Cely\Documents\GitHub\tesis_meca\stores\MCO - Stata"

use "BaseAglo"

rename codigo_dane Codigodane

merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "indHHI"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "VALORSUELO"
drop if _merge!=3
drop _merge
gen Valorsuelo_e = Valorsuelo/100

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

*save MCOED.dta


****Estadística descriptiva
outreg2 using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstDescriptivaMCO.doc", replace sum(log) keep(VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo ULTIMOPOT MODEXCEPCIONAL Alineadoalc_con)
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstDescriptivaMCO.txt"


*****regresiones con VIS

reg VIS10MIL proporcionareaexpansion
estimates store modelo_1
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005
estimates store modelo_2
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e 
estimates store modelo_3
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e ULT_POT MODEXCEPCIONAL Alineadoalc_con 
estimates store modelo_4
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e ULT_POT MODEXCEPCIONAL Alineadoalc_con i.Aglo
estimates store modelo_5

*Incluyendo EF por aglomeracion en todos 
reg VIS10MIL proporcionareaexpansion i.Aglo
estimates store modelo_6
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 i.Aglo
estimates store modelo_7
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e i.Aglo
estimates store modelo_8
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e ULT_POT MODEXCEPCIONAL i.Aglo
estimates store modelo_9
reg VIS10MIL proporcionareaexpansion IPM_urb Defcuant2005 HHI2013 Valorsuelo_e ULT_POT MODEXCEPCIONAL Alineadoalc_con i.Aglo
estimates store modelo_10

**Doble selection lasso
dsregress VIS10MIL proporcionareaexpansion, controls(IPM_urb Defcuant2005 HHI2013 Valorsuelo_e ULT_POT MODEXCEPCIONAL Alineadoalc_con i.Aglo) 
estimates store modelo_11
ereturn list

reg VIS10MIL proporcionareaexpansion Defcuant2005
estimates store modelo_12


outreg2 [modelo_1 modelo_2 modelo_3 modelo_4 modelo_5] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesMCO.doc", replace
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesMCO.txt"


outreg2 [modelo_6 modelo_7 modelo_8 modelo_9 modelo_10] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesMCO2.doc", replace
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesMCO2.txt"

outreg2 [modelo_11 modelo_12 modelo_10] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\Doblelasso.doc", replace
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\Doblelasso.txt"








