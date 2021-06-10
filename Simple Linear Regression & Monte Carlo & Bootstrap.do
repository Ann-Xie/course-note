
clear
set mem 800m

* anscombe data (outliers)
infile Y1 X1 Y2 X2 Y3 X3 Y4 X4 using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\anscombe.txt"
sum
corr Y1 X1 
corr Y2 X2
corr Y3 X3
corr Y4 X4

twoway(scatter Y1 X1)
twoway(scatter Y2 X2)
twoway(scatter Y3 X3)
twoway(scatter Y4 X4)


* Husband and wife's heights
clear
infile husband wife using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\husband_wife.txt", clear

twoway (scatter husband wife)(line wife wife)

correlate husband wife, covariance
correlate husband wife
corr
pwcorr husband wife,sig 



* lifeline.txt
* age = age at which person died (in years)
* length = length of lifeline on person's left hand

clear

infile age length using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\lifeline.txt"

twoway (scatter age length)

correlate age length, covariance

correlate age length
corr
pwcorr age length,sig 



********************
* Simple regression
********************
* China consumption data
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\consumption_china.csv"

twoway (scatter consumption income )

reg consumption income
predict yhat, xb

twoway (scatter consumption income )(line yhat income)


********
* wages
*******
* wage data
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\WAGE1.RAW", clear

twoway(scatter wage educ)

reg wage educ
predict wagehat,xb
order wage wagehat
predict ehat, residual

gen ehat2=wage-wagehat

order wage wagehat ehat ehat2

order wage wagehat ehat2


twoway(scatter wage educ)(line wagehat educ)

twoway(scatter ehat educ)


* regression through origin

reg wage educ, noconstant

gen index=_n

reg wage index

, noconstant



***********************
*Phillips Curve example
************************
clear

use F:\Econometrics_undergraduate\class_dataset\Chapter2_data\phillipsearly.dta

gen unemploy100 = unemploy/100  
twoway (scatter unemploy100 infl)

reg unemploy100 infl 


predict unemploy100_hat, xb

twoway (scatter unemploy100 infl)(line unemploy100_hat infl)

* use log 
gen logunemploy = ln(unemploy100)

* or log(unemploy100)

gen loginfl = ln(infl)

reg logunemploy loginfl
predict logunemploy_hat, xb

gen unemploy100_hat2 = exp(logunemploy_hat)
sort unemploy100_hat2
twoway (scatter unemploy100 infl)(line unemploy100_hat2 infl)


help reg

*************************
* Monte Carlo Simulation
************************
*set mem 500m
*set more off 
*set matsize 800

clear

scalar beta0 = 1.5
scalar beta1 = 5.6
scalar sigma = 2

`'
forvalues i = 1(1)50 {
    set obs 100
    gen x`i' =_n
    gen u`i' = invnorm(uniform())*sigma
    gen y`i'= beta0 + beta1*x`i'+u`i'
    reg y`i' x`i'
}

* Quietly regression
clear
scalar beta0 = 1.5
scalar beta1 = 5.6
scalar sigma = 2

forvalues i = 1(1)50 {
    set obs 100
    gen x`i' =_n
    gen u`i' = invnorm(uniform())*sigma
    gen y`i'= beta0 + beta1*x`i'+u`i'       
    qui reg y`i' x`i'
    matrix b`i' = e(b)
    matrix list b`i'
    matrix rmse`i'= e(rmse) 
    matrix list rmse`i'
}

matrix b0 = (0,0)
matrix rmse0 = (0)

help reg


forvalues j = 1(1)50 {
   matrix b0 = b0\b`j'
   matrix b`j' = b0
   matrix rmse0 = rmse0\rmse`j'
   matrix rmse`j' = rmse0
   matrix list b`j'
   matrix list rmse`j'
}

matrix list b50
matrix list rmse50

matrix U = J(1,51,1)
matrix list U

* compute sum statistics
matrix E_betaHat = U*b50/50
matrix list E_betaHat

matrix E_sigmaHat = U*rmse50/50
matrix list E_sigmaHat

* matrix to data
svmat double b50, name(b_hat)

svmat double rmse50, name(rmse)

save F:\Econometrics_undergraduate\class_dataset\Chapter2_data\MC_Sim, replace

drop if rmse1==.|b_hat1==.|b_hat2==.

drop if rmse1==0|b_hat1==0|b_hat2==0

rename b_hat2 b_hat0

sum b_hat0 b_hat1 rmse1

* Histogram & kdensity(distribution plot)
histogram b_hat0
kdensity b_hat0


histogram b_hat1
kdensity b_hat1

histogram rmse1
kdensity rmse1


* QQ plot
qnorm b_hat0

qnorm b_hat1

qnorm rmse1



*****************************
* Bootstrap standard error
*****************************
* wage data
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics_undergraduate\class_dataset\Chapter2_data\WAGE1.RAW", clear

reg wage educ expersq exper tenure

bootstrap,  seed(100) reps(50): reg wage educ exper expersq  tenure

bootstrap diff=(_b[educ]-_b[exper]),  seed(100) reps(50): reg wage educ exper expersq  tenure

bootstrap multiply=(_b[educ]*_b[exper]),  seed(100) reps(50): reg wage educ exper expersq  tenure

bootstrap division=(_b[educ]/_b[exper]),  seed(100) reps(50): reg wage educ exper expersq  tenure




******************
* One more example
******************
* Engel's law: Households with higher incomes spend a smaller fraction of their income on food
use F:\Econometrics_undergraduate\class_dataset\Chapter2_data\ducfood.dta, clear


twoway (scatter food total)
reg food total

reg lfood ltot


