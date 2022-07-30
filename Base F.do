************************************

clear
clear all

cd "C:\Users\SARA\Documents\ESPECIALIZACIÓN\TESIS\DATOS 2"

*abrir base
use "MINVIVIENDA POR MUNICIPIO", replace

*unirla con caja de herramientas
merge m:m codmpio using ""