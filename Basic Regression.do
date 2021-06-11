
insheet using (enter your excel location here), clear

describe

tab country


* US time-series consumption and income data
clear

insheet using "(enter your excel location here)", clear

import excel "(enter your excel location here)", sheet("consumption_us") firstrow clear


global econometrics2019 (enter your excel location here)

import excel $(enter your excel file name here), sheet("consumption_us") firstrow clear

twoway (scatter y x)
twoway (scatter Y X)


reg y x

reg Y X

*****************************************
*net from (enter your excel location here)
*net install estout, replace

estimate store m1, title (model 1)
estout m1
estout m1, cells(b(star fmt(3)) se(par fmt(2))) ///
   legend label varlabels(_cons Constant)
   
estout m1, cells(b(star fmt(3)) se(par fmt(2)))   ///
   legend label varlabels(_cons constant)               ///
   starlevels (* 0.10 ** 0.05 *** 0.01)  ///
   stats(r2 N, fmt(3 0 1) label(R-sq Obs.))   

esttab using test.xls
******************************************
   
   
predict yhat, xb
twoway (scatter y x)(line yhat x)

twoway (scatter Y X)(line yhat X)

****************************************
* Intermediate Econometrics Final Exam
*****************************************
clear

import excel "(enter your excel name here)", sheet("Sheet1") firstrow clear

import excel $(enter your excel name here), sheet("Sheet1") firstrow clear


* change format
gen final2=real(final)

gen mid2=real(mid)

sum final2

sum homework mid2 final2 if quit==0

sort class
by class: sum final2

bysort class: summarize homework mid2 final2 if quit==0

bysort class: tab final2


tab final2 if quit==0 & class==6
tab final2 if quit==0


histogram final2 if quit==0 , percent normal

kdensity final2 if quit==0, gaussian normal


twoway scatter final2 homework if quit==0 
twoway scatter final2 mid2 if quit==0, ylabel(0(10)100) xlabel(0(10)100)

* add a 45 degree reference line
twoway scatter final2 mid2 if quit==0|| line mid2 mid2, ylabel(0(10)100) xlabel(0(10)100)


* regression
reg final2 homework mid2 if quit==0


* For years 2010-2011
* add att variable but no mid variable
reg final2 homework att mid2 if quit==0


reg final2 homework att if quit==0

reg final2 homework mid2 if quit==0

* logit model only 
logit quit att homework



