
clear
set memory 100m
set more off


******************
*Multicollinearity
*******************
* wage1 data- perfect multicollinearity
****************************************
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics_undergraduate\class_dataset\Chapter4_data\WAGE1.RAW", clear

gen educ2 = 2*educ
reg wage educ educ2


**************************************************
* comsumption2 data - imperfect multicollinearity
**************************************************
clear
insheet using "F:\Econometrics\consumption2.csv"

corr income wealth

reg consumption income wealth

* auxiliary regression
reg income wealth

*************************
* Longley Data
*************************
Y = # of people employed
X1 = GNP implicit price deflator
X2 = GNP
X3 = # of people unemployed
X4 = # of people in the armed forces
X5 = noninstitutionalized population over 14 years of age
X6 = time
*********************************************************
clear
insheet using "F:\Econometrics\longley_data.csv"
reg y x1 x2 x3 x4 x5 time

correlate x1 x2 x3 x4 x5 time

* partial & semipartial correlation coefficients
pcorr x1 x2 x3 x4 x5 time

pcorr x2 x1 x3 x4 x5 time
pcorr x3 x1 x2 x4 x5 time
pcorr x4 x1 x2 x3 x5 time
pcorr x5 x1 x2 x3 x4 time
pcorr time x1 x2 x3 x4 x5

* VIF_j=1/(1-R2_j)
estat vif

* Tolerance = 1/VIF

* Auxiliary regressions
reg x1 x2 x3 x4 x5 time

reg x2 x1 x3 x4 x5 time

reg x3 x1 x2 x4 x5 time

reg x4 x1 x2 x3 x5 time

reg x5 x1 x2 x3 x4 time

reg time x1 x2 x3 x4 x5

* OLS estimators and their standard errors are 
* sensitive to small changes in the data
reg y x1 x2 x3 x4 x5 time
est store model1

reg y x1 x2 x3 x4 x5 time if time <16
est store model2

estout model1 model2, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) legend label varlabels(_cons Constant) stats(r2 r2_a F N, labels(R-squared "Adj R-squared" "Sum of Sq. Residuals" F-stat "Number of Obs."))


* remedial actions
* 1. express GNP(x2) in real terms
* 2. keep only one of x5 and time, since x5 grows over time
* 3. drop x3, unemployed # of people, (maybe unemployment rate is better)

gen GNP_real = x2/x1

reg y GNP_real x4 x5 


*******************************
*  Heteroskedasticity 
********************************
* consumption3 data - heteroscedasticity
clear 
insheet using "F:\Econometrics\consumption3.csv"

reg consumption income

predict con_hat, xb

* two methods to compute residuals
predict e, residuals

gen ehat = consumption - con_hat

gen ehatsq = ehat^2

* plot tests
twoway (scatter ehatsq con_hat)

twoway (scatter ehatsq income)

* BP test
reg ehatsq income

* white test
reg consumption income

imtest, white

* enhanced white test
gen con_hatsq = con_hat^2

reg ehatsq con_hat con_hatsq
reg ehatsq con_hat


* robust stadard error
reg consumption income
reg consumption income, robust

reg consumption income, vce(robust)


clear

infile str8 country year lgaspcar lincomep lrpmg lcarpcap using "F:\Econometrics\TableF9.2_Gasoline.dat" in 2/343

tab country, gen (country)

* reg lgaspcar lgaspcar lincomep lrpmg lcarpcap country1-country18, noconstant

reg lgaspcar lincomep lrpmg lcarpcap country1-country18, noconstant
imtest, white

estimate store OLS


test country1=country2=country3=country4=country5=country6=country7=country8=country9=country10=country11=country12= country13=country14=country15=country16=country17=country18

predict ehat, residual

twoway(scatter ehat country)

egen country_n = group(country)
twoway(scatter ehat country_n)

encode country, gen(country_n2)
twoway(scatter ehat country_n2), saving(fig1)

reg lgaspcar lincomep lrpmg lcarpcap country1-country18, noconstant vce(robust)
estimate store White_robust


gen var_e = ehat^2

egen var_e2 = mean(var_e), by (country_n2)

* FGLS-WLS
reg lgaspcar lincomep lrpmg lcarpcap country1-country18 [aweight=1/var_e2], noconstant 
predict ehat2, residual

*twoway(scatter ehat2 country_n), saving(fig2)

*graph combine fig1.gph fig2.gph, col(2)

estimate store FGLS

* smaller standard error of FGLS
estout OLS White_robust FGLS, cells(b(star fmt(3)) se(par fmt(2))) legend label varlabels(_cons Constant) 


clear

infile price assess bdrms lotsize sqrft colonial lprice lassess llotsize lsqrft using "F:\Econometrics\HPRICE1.RAW", clear

reg price lotsize sqrft bdrms
imtest, white

* log transform data can reduce heteroscedasticity
reg lprice llotsize lsqrft bdrms
imtest, white




****************
* Autocorrelation
****************
***********************************************************
* Y: index of real compensation per hour (base year = 1992)
* X: index of output per hour (base year = 1992)
************************************************************
clear

insheet using "F:\Econometrics\product.csv"

tsset year

twoway (scatter y x)

reg y x

predict e, residuals

twoway (line e year)

gen le=L1.e

twoway (scatter e l.e)


* report d-w statistics
dwstat
estat dwatson

* Breusch-Godfrey (BG) test
***************************************************************************
 estat bgodfrey [, bgodfrey_options]

bgodfrey_options     Description
----------------------------------------------------------------------
lags(numlist)        test numlist lag orders
nomiss0              do not use Davidson and MacKinnon's approach
small                obtain p-values using the F or t distribution
----------------------------------------------------------------------

Options for estat bgodfrey

lags(numlist) specifies a list of numbers, indicating the lag orders to be tested. 
The test will be performed separately for each order.  The default is order one.

nomiss0 specifies that Davidson and MacKinnon's approach (1993, 358), 
which replaces the missing values in the  initial observations on the lagged residuals
 in the auxiliary regression with zeros, not be used.

small specifies that the p-values of the test statistics be obtained using 
the F or t distribution instead of the  default chi-squared or normal distribution.
**********************************************************************************
reg y x

predict e, residuals

estat bgodfrey, nomiss0 lag(1)

reg e x L.e
display e(N)*e(r2)

* test for higher-order serial correlation
reg y x
estat bgodfrey, nomiss0 lag(1/2)

reg e x L.e L2.e

display e(N)*e(r2)

*********************************
* FGLS: deal with autocorrelation
*********************************
*reg e L.e, noconstant

reg e x l.e
* rouhat = 0.9043024

gen ystar = y - _b[L.e]*L.y
gen xstar = x - _b[L.e]*L.x 

replace ystar = (1-_b[L.e]^2)^0.5*y if year==1959
replace xstar = (1-_b[L.e]^2)^0.5*x if year==1959

reg ystar xstar

reg y x
est store m1

predict e2, residuals

twoway (line e2 year)

twoway (scatter e2 L.e2)

dwstat


*******************************************
* Regression with Newey-West standard errors
********************************************
newey y x, lag(1)
est store Newey_West

estout m1 Newey_West, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01)


***********************************
* C-D production function
***********************************
clear 
insheet using "F:\Econometrics\China_C_D_data.csv"

gen lnGDP = ln(rgdp)
gen lnK = ln(k)
gen lnL = ln(l)

gen lnGDP_L = ln(rgdp/l)
gen lnK_L = ln(k/l)

tsset year

reg lnGDP lnK lnL t if year<1958|year>1969
test _b[lnK] + _b[lnL] = 1

dwstat

predict yhat, xb

reg lnGDP_L lnK_L t if year<1958 | year>1969

dwstat

* deal with autocorrlation using FGLS
reg lnGDP_L lnK_L t if year<1958 | year>1969
predict ehat, residual

reg ehat lnK_L t L.ehat if year<1958 | year>1969 
*rou = 0.518859

reg ehat lnK_L t L.ehat
* rou = 0.7433032
* paper reported rou = 0.6234

gen lnGDP_Lstar = lnGDP_L - 0.6234*L.lnGDP_L
gen lnK_Lstar = lnK_L - 0.6234*L.lnK_L

replace lnGDP_Lstar = (1-0.6234^2)^0.5*lnGDP_L if year==1952
replace lnK_Lstar = (1-0.6234^2)^0.5*lnK_L if year==1952

reg lnGDP_Lstar lnK_Lstar t if year<1958 | year>1969
dwstat




*********************************
* Endogeneity
 ********************************************************************
* install estout
net from F:\estout
net install estout, replace
 
 
* Card 1995 data
clear 
infile id  nearc2 nearc4 educ age fatheduc motheduc weight momdad14 sinmom14 step14 reg661 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669 south66 black smsa south smsa66 wage enroll KWW IQ married libcrd14 exper lwage expersq using "F:\Econometrics_undergraduate\class_dataset\Chapter4_data\CARD.RAW"
                   
* OLS
reg lwage educ exper expersq black smsa south smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669   
estimates store m1, title(OLS)


* Proxy variable (IQ for ability)
reg lwage educ exper expersq black smsa south smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669 IQ  
estimates store m2, title(Proxy Variable)

* IV (nearc4)
pwcorr nearc4 educ, sig
                   
ivreg lwage (educ=nearc4) exper expersq black smsa south smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669               

estimates store m3, title(IV)

estout m1 m2 m3, cells(b(star fmt(3)) se(par fmt(2)))  starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) legend label varlabels(_cons Constant) stats(r2 r2_a F N, labels(R-squared "Adj R-squared" "Sum of Sq. Residuals" F-stat "Number of Obs."))

reg educ nearc4 exper expersq black smsa south smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669               

predict educ_hat, xb

reg lwage educ_hat exper expersq black smsa south smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669               
estimates store m4, title(two_step)

estout m3 m4, cells(b(star fmt(3)) se(par fmt(2)))  starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) legend label varlabels(_cons Constant) stats(r2 r2_a F N, labels(R-squared "Adj R-squared" "Sum of Sq. Residuals" F-stat "Number of Obs."))


ereturn list

*******
* 2SLS
*******
* return to education for working women
* note:  inlf =1 if in labor force, 1975
clear
infile inlf hours kidslt6 kidage6 age educ wage repwage hushrs husage huseduc huswage faminc mtr motheduc fatheduc unem city exper nwifeinc lwage expersq using "F:\Econometrics\MROZ.RAW", clear

* 2SLS
corr educ motheduc fatheduc

ivreg lwage (educ = motheduc fatheduc) exper expersq if inlf ==1
estimates store m2, title(2SLS)

* compare OLS with 2SLS
reg lwage educ exper expersq if inlf==1
estimates store m1, title(OLS)

estout m1 m2, cells(b(star fmt(3)) se(par fmt(2)))  starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) legend label varlabels(_cons Constant) stats(r2 r2_a F N, labels(R-squared "Adj R-squared" "Sum of Sq. Residuals" F-stat "Number of Obs."))
 
************ 
* 2-step OLS
************
* first-step OLS
reg educ exper expersq motheduc fatheduc if inlf==1

drop educ_hat
predict educ_hat,xb

* second-step OLS
reg lwage educ_hat exper expersq if inlf==1
estimates store m3, title(two-step OLS)

estout m2 m3, cells(b(star fmt(5)) se(par fmt(4)))  starlevels(+ 0.10 * 0.05 ** 0.01 *** 0.001) legend label varlabels(_cons Constant) stats(r2 r2_a F N, labels(R-squared "Adj R-squared" "Sum of Sq. Residuals" F-stat "Number of Obs."))

* adjusted for estimated standard errors
rename educ_hat educ_hat_keep
rename educ educ_hat

predict ehat, residual

gen ehat_sq=ehat^2

sum ehat_sq

help summarize
scalar realmse=r(mean)*r(N)/e(df_r)

ereturn list

matrix bmatrix=e(b)
matrix Vmatrix=e(V)*realmse/e(rmse)^2

ereturn post bmatrix Vmatrix

ereturn display

rename educ_hat educ
ivreg lwage (educ = motheduc fatheduc) exper expersq if inlf ==1



* Endogeneity Test
reg lwage educ exper expersq if inlf==1
estimates store OLS

ivreg lwage (educ = motheduc fatheduc) exper expersq if inlf ==1
estimates store TSLS

hausman TSLS OLS


ivregress 2sls lwage (educ = motheduc fatheduc) exper expersq if inlf ==1

estat endogenous educ


reg educ motheduc fatheduc exper expersq if inlf==1
predict v, residual

reg lwage educ exper expersq v if inlf==1
* t = 1.67, moderate evidence of endogeneity

