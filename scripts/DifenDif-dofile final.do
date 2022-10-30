********************************************
***********DIF EN DIF***********************
********************************************

*Limpiar
clear
clear all 

*Establecer directorio

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\stores\DiD - Stata"

*cd "C:\Users\Camila Cely\Documents\GitHub\tesis_meca\stores\MCO y DiD"

use "BASE PARA DID"



*************************************
*EJERCICIO INTUITIVO
*************************************

*Estimacion
gen Post2015=0 if ano<2015
replace Post2015=1 if ano>=2015 
drop if ano>2018

collapse (mean) DeficitHabitacional DeficitCuantitativo DeficitCualitativo Tratamiento Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos dept eje (sum) VIS10MIL, by (MUNICIPIO Post2015)

reshape wide VIS DeficitHabitacional DeficitCuantitativo DeficitCualitativo Aglo indiceHHI valorsuelourb Alineadoalc_nal Mismoalc_nal Alineadoalc_con Alineado3periodos Tratamiento dept eje, i(MUNICIPIO) j(Post2015)

gen deltaVIS=VIS10MIL1-VIS10MIL0

reg deltaVIS Tratamiento1 , vce (cluster Aglo1) 
estimates store modelo_i1
reg deltaVIS Tratamiento1 DeficitHabitacional0 DeficitCuantitativo0 , vce (cluster Aglo1) 
estimates store modelo_i2
reg deltaVIS Tratamiento1 DeficitHabitacional0 DeficitCuantitativo0 indiceHHI0 valorsuelourb0 , vce (cluster Aglo1) 
estimates store modelo_i3
reg deltaVIS Tratamiento1 DeficitHabitacional0 DeficitCuantitativo0 indiceHHI0 valorsuelourb0 Alineadoalc_con0, vce (cluster Aglo1) 
estimates store modelo_i4

outreg2 [modelo_i1 modelo_i2 modelo_i3 modelo_i4] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesDIDmodeloinicial.doc",  replace 
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesDIDmodeloinicial.txt"

*************************************
*EJERCICIO CON TENDENCIAS PARALELAS
*************************************


***********
*Trends
***********

clear
clear all
use "BASE 2009"


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
reghdfe cum_vis year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)
reghdfe cum_vis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)

*generar el logaritmo de VIS acumuladas
gen logcumvis=log(cum_vis +1)
gen logcumvis10=log(cum_vis10 +1)

reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni ano##eje)  cl(aglo)
reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)
reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(eje)
*El logaritmo funciona mucho mejor

*Si quisieramos correrlo con eje tendriamos que correr bootstrap (menos de 30 clusters) pero tiene resultados similares a usar aglomeracion

bootstrap, reps(300) cl(eje) idcl(neweje) group(muni): reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni) 
*vs
reghdfe logcumvis10 year_treat2009-year_treat2021 , absorb(ano muni )  cl(aglo)
estimates store modelo_test


outreg2 [modelo_test] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\TendenciasparalelasDID.doc", replace
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\TendenciasparalelasDID.txt"



*Gráfico tendencias paralelas
coefplot, vert drop(_cons) yline(0)

coefplot, keep(year*)vertical base mcolor("0 111 150") ciopts(lcolor("118 152 160")) ///
	rename(year_treat2009="2009" year_treat2010="2010" year_treat2011="2011" year_treat2012="2012"year_treat2013=		"2013" year_treat2014="2014" year_treat2015="2015" year_treat2016="2016"year_treat2017="2017" year_treat2018=		"2018" year_treat2019="2019"year_treat2020="2020" year_treat2021="2021") ///
	yline(0,lcolor("0 111 150") lpattern(dash)) xline(4 7, lcolor("255 191 128")) ///
	graphregion(fcolor(white) ifcolor(white) ilcolor(white)) ///
	xscale(lcolor("0 51 102")) yscale(lcolor("0 51 102")) ///
	xlabel(,  noticks) ylabel( ,noticks nogrid) 

graph export "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\TendenciasparalelasDID.png", replace
	
*Corremos con controles 
clear
clear all 
use "BASE PARA DID"
tab ano, gen(year)



forvalues i=1(1)11{
	loc j=2010+`i'
	gen year_treat`j'=year`i'*Tratamiento
}

encode Aglomeración, generate(aglo)
encode MUNICIPIO, generate(muni)

generate newaglo = aglo
generate neweje=eje

xtset muni ano
set seed 10101

bysort muni (ano) : gen cum_vis10 = sum(VIS10MIL)
gen logcumvis10=log(cum_vis10 +1)

*Estadistica descriptiva en la linea base
asdoc tabstat logcumvis10  DeficitCuantitativo indiceHHI  Alineadoalc_con  if ano==2011, by(Tratamiento) stat(mean sd min max) save(C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstDescriptivaDID.doc) replace

*Prueba de medias
foreach x in logcumvis10 DeficitHabitacional DeficitCuantitativo indiceHHI  Alineadoalc_con {

	asdoc ttest `x', by(Tratamiento) save(C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\TtestDID.doc)

}
*pruebas chi para las variables categoricas
tab aglo Tratamiento if ano==2013, chi2 


reghdfe logcumvis10 year_treat2012-year_treat2020  , absorb(ano muni)  cl(aglo)
estimates store modelo_1 

reghdfe logcumvis10 year_treat2012-year_treat2020 DeficitCuantitativo, absorb(ano muni )  cl(aglo)
estimates store modelo_2

reghdfe logcumvis10 year_treat2012-year_treat2020  DeficitCuantitativo indiceHHI , absorb(ano muni )  cl(aglo)
estimates store modelo_3 

reghdfe logcumvis10 year_treat2012-year_treat2020  DeficitCuantitativo indiceHHI  Alineadoalc_con  , absorb(ano muni )  cl(aglo)
estimates store modelo_4

outreg2 [modelo_1 modelo_2 modelo_3  modelo_4] using "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesmodeloDID.doc", replace addtext (Year Fixed Effect, YES, Aglomeracion Fixed Effects, YES)
erase "C:\Users\SARA\Documents\ESPECIALIZACIÓN\BIG DATA\GITHUB\tesis_meca\views\EstimacionesmodeloDID.txt"


