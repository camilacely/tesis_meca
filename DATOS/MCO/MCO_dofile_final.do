

clear
clear all

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\DATOS\MCO"

use "Aglo"

*Construimos la base
merge m:m MUNICIPIO DEPARTAMENTO using "MINVIVIENDA POR MUNICIPIO"
drop _merge

rename codigo_dane Codigodane

merge m:m Codigodane using "Deficit"
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

merge m:m MUNICIPIO using "indHHI"
drop if _merge==2
drop _merge

rename Codigodane COD
merge m:m COD using "Valor2"
drop if _merge!=3
drop _merge

merge m:m Codigodane using "GRAL PANEL CEDE"
drop if _merge==2
drop _merge

*Solo tomamos municipios donde tenemos registros de VIS
drop if VIS==0
drop if VIS==.



*Creamos variables de subsidio MCY
egen subtotal=rowtotal(HASTA2SMMLV SUPERIORESA2SMMLVYHASTA3S SUPERIORESA2SMMLVYHASTA4S SUPERIORESA3SMMLVYHASTA4S)
egen subvis=rowtotal(SUPERIORESA3SMMLVYHASTA4S SUPERIORESA2SMMLVYHASTA4S SUPERIORESA2SMMLVYHASTA3S)
















