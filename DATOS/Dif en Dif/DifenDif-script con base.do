********************************************
***********DIF EN DIF***********************
********************************************

*Limpiar
clear
clear all 

*Establecer directorio

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\DATOS\Dif en Dif"

*cd "C:\Users\Camila Cely\Documents\GitHub\tesis_meca\DATOS\Dif en Dif"

use "BASE 2009"

*************************************
*MODELO ORIGINAL
*************************************

use "BASE PARA DID"

*Primero 2013 
gen Post2013=0
replace Post2013=1 if ano>= 2013

sort ano Tratamiento

by ano Tratamiento: egen promedio_periodo=mean(VIS10MIL)

tw (connected promedio_periodo ano if Tratamiento==1, msymbol(D) color(black)) 			///
		(connected promedio_periodo ano if Tratamiento==0, msymbol(T) color(gray)), 	///
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2013 2016, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		
		
gen TratxPost2013=Tratamiento*Post2013

xtset cod ano
*Panel balanceado

*CON REGRESIONES NORMALES FUNCIONA UN POQUITO
*Corremos regresion con efectos fijos por aglomeracion y año
reghdfe VIS10MIL TratxPost2013 , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2013 DeficitCuantitativo , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI valorsuelourb, absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con , absorb(ano Aglo) 

*Con errores robustos por clusters no es significativo. Pero el indice HHI y el valor del suelo si continúan siendo significativos
reghdfe VIS10MIL TratxPost2013 , absorb(ano Aglo) vce (cluster Aglo) 
reghdfe VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con , absorb(ano Aglo) vce (cluster Aglo) 

*con xtreg funciona con efectos fijos por region no por aglomeracion
xtreg VIS10MIL TratxPost2013 i.ano i.eje
xtreg VIS10MIL TratxPost2013 DeficitCuantitativo  i.ano i.eje
xtreg VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI  i.ano i.eje
xtreg VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI valorsuelourb i.ano i.eje
xtreg VIS10MIL TratxPost2013 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con i.ano i.eje

**tendencias paralelas?
didregress (VIS10MIL DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con) (TratxPost2013), group(mun) time(ano)
estat trendplots
estat ptrends
estat granger
estat grangerplot
********************************************
*PARA 2014 pensando que depronto el lag no es de 1 año sino de 2 
********************************************
gen Post2014=0
replace Post2014=1 if ano>= 2014

tw (connected promedio_periodo ano if Tratamiento==1, msymbol(D) color(black)) 			///
		(connected promedio_periodo ano if Tratamiento==0, msymbol(T) color(gray)), 	///
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2014 2017, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		
		
gen TratxPost2014=Tratamiento*Post2014

reg VIS10MIL TratxPost2014 i.ano i.Aglo
reg VIS10MIL TratxPost2014 DeficitCuantitativo i.Aglo i.ano
reg VIS10MIL TratxPost2014 DeficitCuantitativo indiceHHI i.Aglo i.ano
reg VIS10MIL TratxPost2014 DeficitCuantitativo indiceHHI valorsuelourb i.Aglo i.ano
reg VIS10MIL TratxPost2014 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con i.ano i.Aglo

********************************************
*PARA 2015 de acuerdo con lo que vimos en el modelo con sebastian
**********************************************
gen Post2015=0
replace Post2015=1 if ano>= 2015

sort ano Tratamiento

gen TratxPost2015=Tratamiento*Post2015

reghdfe VIS10MIL TratxPost2015 , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2015 DeficitCuantitativo , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI , absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI valorsuelourb, absorb(ano Aglo)
reghdfe VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con , absorb(ano Aglo) 


*Con vce no es significativo. 
reghdfe VIS10MIL TratxPost2015 , absorb(ano Aglo) vce (cluster Aglo) 
reghdfe VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con , absorb(ano Aglo) vce (cluster Aglo) 

*con xtger funciona con efectos fijos por region no por aglomeracion
xtreg VIS10MIL TratxPost2015 i.ano i.eje
xtreg VIS10MIL TratxPost2015 DeficitCuantitativo  i.ano i.eje
xtreg VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI  i.ano i.eje
xtreg VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI valorsuelourb i.ano i.eje
xtreg VIS10MIL TratxPost2015 DeficitCuantitativo indiceHHI valorsuelourb Alineadoalc_con i.ano i.eje

****podríamos correr el ejercicio inicial con efectos fijos por eje, pensando que tenga una logica similar a controlar por aglomeracion 



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
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2015 2012, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		

*Estadistica descriptiva en la linea base
asdoc tabstat VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos if ano==2015, by(Tratamiento) stat(mean sd min max)

*Prueba de medias
foreach x in VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos {

	asdoc ttest `x', by(Tratamiento)
}

*pruebas chi para las variables categoricas
tab eje Tratamiento if ano==2015, chi2 

*Modelo
collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos dept eje (sum) VIS10MIL, by (MUNICIPIO Post2015)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos Tratamiento dept eje, i(MUNICIPIO) j(Post2015)

gen deltaVIS=VIS10MIL1-VIS10MIL0

reg deltaVIS Tratamiento1 i.Aglo1
reg deltaVIS Tratamiento1 i.eje1

reg deltaVIS Tratamiento1 DeficitHabitacional0 i.eje1
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 i.eje1
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 valorsuelourb0 i.eje1
reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 valorsuelourb0 Alineadoalc_con0 Alineadoalc_nal0 i.eje1


*************************************
*MISMO MODELO AJUSTANDO AÑOS 
*************************************

*Estimacion
*Año de tratamiento 2013 teniendo en cuenta que la ley se aprobó en junio de 2012 y necesita tiempo de implementacion
gen Post2013=0 if ano<2013
replace Post2013=1 if ano>=2013
drop if ano>2021


*Tendencias paralelas 
sort ano Tratamiento
by ano Tratamiento: egen promedio_periodo=mean(VIS10MIL)

tw (connected promedio_periodo ano if Tratamiento==1, msymbol(D) color(black)) 			///
		(connected promedio_periodo ano if Tratamiento==0, msymbol(T) color(gray)), 	///
		legend(lab(1 "Tratados") lab(2 "Controles")) xline(2013 2016, lcolor(red))		///
		ytitle("Promedio") xtitle("Periodo") graphregion(fcolor(white)) 
		

*Estadistica descriptiva en la linea base
asdoc tabstat VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos if ano==2013, by(Tratamiento) stat(mean sd min max)

*Prueba de medias
foreach x in VIS10MIL DeficitHabitacional DeficitCuantitativo DeficitCualitativo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos {

	asdoc ttest `x', by(Tratamiento)
}

*pruebas chi para las variables categoricas
tab DEPARTAMENTO Tratamiento if ano==2013, chi2 

*Modelo
collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_con  dept eje (sum) VIS10MIL, by (MUNICIPIO Post2013)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb   Alineadoalc_con  Tratamiento dept eje, i(MUNICIPIO) j(Post2013)
gen deltaVIS=VIS10MIL1-VIS10MIL0

reg deltaVIS Tratamiento1 i.eje1
reg deltaVIS Tratamiento1 DeficitCuantitativo1 i.eje1
reg deltaVIS Tratamiento1 DeficitCuantitativo1 indiceHHI1 i.eje1
reg deltaVIS Tratamiento1 DeficitCuantitativo1 indiceHHI1 valorsuelourb1 i.eje1
reg deltaVIS Tratamiento1 DeficitCuantitativo1 indiceHHI1 valorsuelourb1 Alineadoalc_con1 i.eje1



*************************************
*EJERCICIO SABADO 22 DE OCTUBRE SEBASTIAN
*************************************

*Estimacion
gen Post2015=0 if ano<2015
replace Post2015=1 if ano>=2015 
drop if ano>2018

collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos dept eje (sum) VIS10MIL, by (MUNICIPIO Post2015)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos Tratamiento dept eje, i(MUNICIPIO) j(Post2015)

gen deltaVIS=VIS10MIL1-VIS10MIL0

reg deltaVIS Tratamiento1 DeficitHabitacional0 indiceHHI0 valorsuelourb0 Alineadoalc_con0 Alineadoalc_nal0, vce (cluster eje1) 



*************************************
*EJERCICIO SUGERENCIAS IGNACIO
*************************************


***********
*Trends
***********
tab ano, gen(year)

forvalues i=1(1)14{
	loc j=2008+`i'
	gen year_treat`j'=year`i'*Tratamiento
}

encode Aglomeración, generate(aglo)
encode MUNICIPIO, generate(muni)

generate newaglo = aglo
generate neweje=eje

xtset muni ano
set seed 10101
reghdfe VIS10MIL year_treat2009-year_treat2021 , absorb(ano muni)  cl(aglo)
*bootstrap, reps(300) cl(aglo) idcl(newaglo) group(muni): reghdfe VIS10MIL year_treat2012-year_treat2021 , absorb(ano muni) 

reghdfe VIS10MIL year_treat2012-year_treat2021 , absorb(ano muni )  cl(aglo)
*Beware of bad controls
*No tenemos deficit ni afiliacion politica desde 2009
*reghdfe VIS10MIL year_treat2012-year_treat2021 DeficitCuantitativo indiceHHI  Alineadoalc_con , absorb(ano muni)  cl(aglo)
*reghdfe VIS10MIL year_treat2012-year_treat2021  DeficitHabitacional DeficitCuantitativo, absorb(ano muni ano##aglo)  cl(aglo)

*generar VIS acumuladas
bysort muni (ano) : gen cum_vis = sum(VIS)
bysort muni (ano) : gen cum_vis10 = sum(VIS10MIL)

reghdfe VIS10MIL year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)
reghdfe cum_vis year_treat2009-year_treat2020 , absorb(ano muni )  cl(aglo)
reghdfe cum_vis10 year_treat2009-year_treat2020 , absorb(ano muni )  cl(aglo)

*generar el logaritmo de VIS acumuladas
gen logcumvis=log(cum_vis +1)
gen logcumvis10=log(cum_vis10 +1)

reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni ano##eje)  cl(aglo)
reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)
reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(eje)
*El logaritmo funciona mucho mejor

*Si quisieramos correrlo con eje tendriamos que correr bootstrap (menos de 30 clusters) pero tiene resultados similares a usar aglomeracion

bootstrap, reps(300) cl(eje) idcl(neweje) group(muni): reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni) 

reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)

*Gráfico tendencias paralelas
coefplot, vert drop(_cons) yline(0)

coefplot, keep(year*)vertical base mcolor("106 208 200") ciopts(lcolor("118 152 160")) ///
	rename(year_treat2009="2009" year_treat2010="2010" year_treat2011="2011" year_treat2012="2012" year_treat2013=		"2013" year_treat2014="2014" year_treat2015="2015" year_treat2016="2016"year_treat2017="2017" year_treat2018=		"2018" year_treat2019="2019"year_treat2020="2020" year_treat2021="2021") ///
	yline(0,lcolor("106 208 200") lpattern(dash)) xline(4 7, lcolor("236 196 77")) ///
	graphregion(fcolor(white) ifcolor(white) ilcolor(white)) ///
	xscale(lcolor("0 51 102")) yscale(lcolor("0 51 102")) ///
	xlabel(, labcolor("0 51 102") noticks) ylabel(, labcolor("0 51 102") noticks nogrid)