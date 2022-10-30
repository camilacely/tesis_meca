********************************************
***********DIF EN DIF***********************
********************************************

*Limpiar
clear
clear all 

*Establecer directorio
cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\stores\DiD - Stata"

*Traigo primera base
use "numeroVIS",replace

merge m:m CODDANE using "Poburb"
drop if _merge!=3
drop _merge
*Se descartan los municipios que no se tiene VIS

merge m:m CODDANE using "aglomeracion"
drop if _merge!=3
drop _merge

merge m:m CODDANE ano using "indice HHI"
drop _merge

merge m:m CODDANE ano using "valorsuelo"
drop _merge

merge m:m CODDANE using "modificacion"
drop _merge
*Aqui traemos el tratamiento

merge m:m CODDANE ano using "afiliacion"
drop if _merge!=3
drop _merge
*Se descartan 2009 y 2010 que no se tienen datos

merge m:m CODDANE ano using "Deficit"
drop if _merge!=3
drop _merge
*Se descartan años 2009 y 2022 que no se tiene el déficit

codebook MUNICIPIO 
*Se cuenta con información para 79 municipios sin missing values


order CODDANE MUNICIPIO Aglomeración Ejesregionales ano VIS Tratamiento pobl_urb DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb PartidoAlcalde Alineadoalc_nal Mismoalc_nal PartidoConcejo Alineadoalc_con Mismocon_nal Alineado3periodos

*Revisar los años que son, por ahora entre 2013 a 2021
drop if ano<=2009
drop if ano>2021

*generar variables 
gen POB10mil= pobl_urb/10000
gen VIS10MIL= VIS/POB10mil

gen cod=CODDANE
destring cod, replace

gen CODDEPT=substr(CODDANE,1,2) 

encode Aglomeración, gen(Aglo)
encode MUNICIPIO, gen(mun)
encode CODDEPT, gen(dept)
encode Ejesregionales, gen(eje)

save "BASE PARA DID",replace

