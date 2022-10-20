********************************************
***********DIF EN DIF***********************
********************************************

*Limpiar
clear
clear all 

*Establecer directorio
cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\DATOS\Dif en Dif"

*Traigo primera base
use "numeroVIS",replace

merge m:m CODDANE using "Poburb"
drop if _merge!=3
drop _merge
*Se descartan los municipios que no se tiene VIS

merge m:m CODDANE ano using "indice HHI"
drop if _merge==2
drop _merge
*Se descarta Apulo

merge m:m CODDANE ano using "Deficit"
drop if _merge!=3
drop _merge
*Se descartan años 2009 y 2022 que no se tiene el déficit

merge m:m CODDANE ano using "valorsuelo"
drop _merge

merge m:m CODDANE ano using "afiliacion"
drop if ano==2022
drop if _merge!=3
drop _merge
*Se descartan 2009 y 2010 que no se tienen datos

merge m:m CODDANE using "modificacion"
drop _merge
*Aqui traemos el tratamiento

merge m:m CODDANE using "aglomeracion"
drop if _merge!=3
drop _merge

codebook MUNICIPIO 
*Se cuenta con información para 79 municipios sin missing values


drop Ciudad Departamento Municipio  
order CODDANE MUNICIPIO DEPARTAMENTO Aglomeración Ejesregionales ano VIS Tratamiento pobl_urb DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb PartidoAlcalde Alineadoalc_nal Mismoalc_nal PartidoConcejo Alineadoalc_con Mismocon_nal Alineado3periodos

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

sort MUNICIPIO ano

save "BASE PARA DID",replace
use "BASE PARA DID"
*******************************************************
*EJERCICIO CON SEBASTIAN
********************************************************

*Estimacion
gen Post2015=0 if ano<2015
replace Post2015=1 if ano>=2015 
drop if ano>2018

*Tendencias paralelas 
sort ano Tratamiento
by ano Tratamiento: egen promedio_periodo=mean(VIS10MIL)

tw (connected promedio_periodo ano if Tratamiento==1, msymbol(D) color(black)) 			///
		(connected promedio_periodo ano if Tratamiento==0, msymbol(T) color(gray)), 	///
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2015, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		

*Estadistica descriptiva en la linea base
asdoc tabstat VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos if ano==2015, by(Tratamiento) stat(mean sd min max)

*Prueba de medias
foreach x in VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos {

	asdoc ttest `x', by(Tratamiento)
}

*pruebas chi para las variables categoricas
tab DEPARTAMENTO Tratamiento if ano==2015, chi2 

*Modelo
collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos dept eje (sum) VIS10MIL, by (MUNICIPIO Post2015)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos Tratamiento dept eje, i(MUNICIPIO) j(Post2015)
gen deltaVIS=VIS10MIL1-VIS10MIL0

reg deltaVIS Tratamiento1 i.dept1
estimates store modelo_1
reg deltaVIS Tratamiento1 DeficitHabitacional0 i.dept1
estimates store modelo_2
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 i.dept1
estimates store modelo_3
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 valorsuelourb0 i.dept1
estimates store modelo_4
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 valorsuelourb0 Alineadoalc_con0 Alineadoalc_nal0 i.dept1
estimates store modelo_5

outreg2 [modelo_1 modelo_2 modelo_3 modelo_4 modelo_5] using "estimaciones1110.doc", replace
drop _est_modelo_2 _est_modelo_3 _est_modelo_4 _est_modelo_5 _est_modelo_1


*************************************
*MODELO SEBASTIAN AJUSTANDO AÑOS 
*************************************


*************************************
*MODELO IGNACIO
*************************************

use "BASE PARA DID"

gen Post2012=0
replace Post2012=1 if ano>= 2012

sort MUNICIPIO ano

by ano DeficitHabitacional: egen promedio_periodo=mean(VIS)

by ano Tratamiento: egen promedio_periodo=mean(VIS10MIL)

tw (connected promedio_periodo ano if Tratamiento==1, msymbol(D) color(black)) 			///
		(connected promedio_periodo ano if Tratamiento==0, msymbol(T) color(gray)), 	///
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2013, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		
		
gen TratxPost2012=Tratamiento*Post2012







