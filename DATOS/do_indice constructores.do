**CARGAR LA BASE QUE TENGA MUNICIPIO, CONSTRUCTOR Y # DE VIVIENDASXCONSTRUCTOR

import excel "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\ind.xlsx", sheet("Sheet1") firstrow clear

merge m:m MUNICIPIO using IND

drop if _merge==2

drop _merge

**GENERAR NUEVA VARIABLE

gen ind1=(VIVIENDAS/TOTAL)^2

**COLAPSAR LA BASE POR MUNICIPIO

collapse (sum) ind1, by (MUNICIPIO)

save "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2\INDICE CONSTRUCTORES.dta", replace

