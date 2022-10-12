********************************************
***********DIF EN DIF***********************
********************************************

*Limpiar
clear
clear all 

*Establecer directorio
cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\DATOS\Dif en Dif"

*Traigo primera base
use "numeroVIS.dta",replace

merge m:m CODDANE using "pob.dta"
drop if _merge!=3
drop _merge

merge m:m CODDANE ano using "indice HHI.dta"
drop _merge

merge m:m CODDANE ano using "Deficit.dta"
drop if _merge==2
drop _merge

merge m:m CODDANE ano using "valorsu.dta"
drop _merge

merge m:m CODDANE ano using "afiliacion.dta"
drop if _merge!=3
drop _merge
*aqui se nos están eliminando 28 municipios

merge m:m CODDANE ano using "Mod.dta"
drop if _merge!=3
drop _merge

drop Codigodane Ciudad Departamento
order CODDANE MUNICIPIO DEPARTAMENTO Aglomeración ano VIS Tratamiento pobl_urb DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb PartidoAlcalde Alineadoalc_nal Mismoalc_nal PartidoConcejo Alineadoalc_con Mismocon_nal Alineado3periodos


*Revisar los años que son, por ahora entre 2013 a 2021
drop if ano<2013
drop if ano>2021

*generar variables 
gen POB10mil= pobl_urb/10000
gen VIS10MIL= VIS/POB10mil

gen cod=CODDANE
destring cod, replace
xtset cod ano

gen CODDEPT=substr(CODDANE,1,2) 

encode Aglomeración, gen(Aglo)
encode MUNICIPIO, gen(mun)
encode CODDEPT, gen(dept)

sort MUNICIPIO ano

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
collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos dept (sum) VIS10MIL, by (MUNICIPIO Post2015)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos Tratamiento dept, i(MUNICIPIO) j(Post2015)
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






