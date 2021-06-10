
clear

insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\randomwalk.csv"

tsset time

corr y1 L.y1, covariance

corr y1 L.y1

corr y1 L2.y1

corrgram y1

ac y1

pac y1

ac d.y1, blwidth(vthick) yscale(range(-1 1))
pac d.y1, blwidth(vthick) yscale(range(-1 1))


****************************************
* Two nonstationary stochastic series
****************************************
* 1. random walk without drift 
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\randomwalk.csv"

tsset time

twoway(line y1 time)

* 2. random walk with a drift
twoway(line y2 time)


* white noise: stationary stochastic series
* yt = yt-1 + e1
* xt = xt-1 + e2
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\whitenoise.csv"

gen time = _n

tsset time

twoway(line e1 time)
twoway(line e2 time)

**********************
* spurious regression
* yt = L.yt + e1
* xt = L.xt + e2
************************
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\whitenoise.csv"

gen time = _n
tsset time

corr e1 e2
pwcorr e1 e2, sig


reg yt xt
dwstat

gen dy = yt - L.yt
gen dx = xt - L.xt

reg dy dx
dwstat


***********************
* stationary tests 
***********************

* ACF plot
ac e1, blwidth(vthick) yscale(range(-1 1))

ac yt, blwidth(vthick) yscale(range(-1 1))


* US/UK exchange rate data
clear
infile time exchange using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\exchange.txt"

tsset time
gen lny = ln(exchange)
gen dlny = lny - L.lny

twoway (line exchange time, clwidth(medthick))
twoway (line lny time, clwidth(medthick))
twoway (line dlny time, clwidth(medthick))

ac lny, blwidth(vthick) yscale(range(-1 1))

ac dlny, blwidth(vthick) yscale(range(-1 1))


* Dickey-Fuller unit root test 
dfuller lny
dfuller dlny

dfuller lny, noconstant
dfuller dlny, noconstant

* Augmented Dickey-Fuller (ADF) test
dfuller lny, lags(4) noconstant reg
dfuller lny, lags(4) reg
dfuller lny, lags(4) trend reg

dfuller dlny, lags(4) noconstant reg
dfuller dlny, lags(4) reg
dfuller dlny, lags(4) trend reg


* simulated data
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\whitenoise.csv"

gen time=_n

tsset time

gen w1= e1

gen w2 = yt

wntestq w1

wntestq w2


dfuller e1, noconstant reg
dfuller e2, noconstant reg
dfuller yt, noconstant reg
dfuller xt, noconstant reg

* The Augmented Dickey-Fuller (ADF) test
dfuller e1, lags(4) noconstant reg
dfuller e1, lags(4) reg
dfuller e1, lags(4) trend reg

dfuller yt, lags(4) noconstant reg
dfuller yt, lags(4) reg
dfuller yt, lags(4) trend reg



***************
* ARIMA model
***************
 * identifying AR model 

clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\whitenoise.csv"

gen time=_n
tsset time

* yt has a unit root
ac yt, blwidth(vthick) yscale(range(-1 1))

ac D.yt, blwidth(vthick) yscale(range(-1 1))

pac yt, blwidth(vthick) yscale(range(-1 1))


* white noise ACF and PACF
ac e1, blwidth(vthick) yscale(range(-1 1))
pac e2, blwidth(vthick) yscale(range(-1 1))

* zt = 2+0.7*zt-1 +e
ac zt, blwidth(vthick) yscale(range(-1 1))
dfuller zt

ac zt, blwidth(vthick) yscale(range(-1 1))
pac zt, blwidth(vthick) yscale(range(-1 1))

arima zt, arima(1,0,0)
estat ic

arima zt, arima(2,0,0)
estat ic

* one-step ahead forecasting
arima zt if time<151, arima(1,0,0)

predict zthat, y
twoway (line zt time, clcolor(black)) (line zthat time, clcolor(blue) clwidth(thick))

* Dynamic forecasting: 
predict zthat3, dynamic(150) y
twoway (line zt time, clcolor(black)) (line zthat3 time, clcolor(blue) clwidth(thick))

predict et1, residuals
twoway (line et1 time, clcolor(blue) clwidth(thick))

predict et3, dynamic(50) residuals
twoway (line et3 time, clcolor(blue) clwidth(thick))


* sunspots data (from http://www.stanford.edu/~clint/bench/#ar1)
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\sunspot.csv"
gen time = _n

tsset time

twoway (line e time)

dfuller e

ac e, blwidth(vthick) yscale(range(-1 1))
pac e, blwidth(vthick) yscale(range(-1 1))

arima e, arima(2,0,0)
estat ic

arima e, arima(3,0,0)
estat ic


***********
* MA model
************
clear all
set seed 339487731
* standard error = 2
scalar sigma = 2

set obs 500
gen time =_n
gen et = invnorm(uniform())*sigma

tsset time

* white noise ACF and PACF
ac et, blwidth(vthick) yscale(range(-1 1))
pac et, blwidth(vthick) yscale(range(-1 1))

gen et_1 = L1.et
gen et_2 = L2.et

gen wt1 = et +0.9*et_1
gen wt2 = et+0.8*et_1
gen wt3 = et+0.5*et_1
gen wt4 = et+0.3*et_1
gen wt5 = et+0.1*et_1

* ARIMA(0,0,2)
gen wt6 = et+0.8*et_1-0.5*et_2

ac wt1, blwidth(vthick) yscale(range(-1 1))
pac wt1, blwidth(vthick) yscale(range(-1 1))

* MA(1)
arima wt1, arima(0,0,1)  
arima wt1, arima(0,0,1)  noconstant 

* MA(2)
ac wt6, blwidth(vthick) yscale(range(-1 1))
pac wt6, blwidth(vthick) yscale(range(-1 1))

arima wt6, arima(0,0,2)
arima wt6, arima(0,0,2) noconstant


* how about arima(2,0,4)
arima wt6, arima(2,0,3)

ac wt3, blwidth(vthick) yscale(range(-1 1))
pac wt3, blwidth(vthick) yscale(range(-1 1))

ac wt4, blwidth(vthick) yscale(range(-1 1))
pac wt4, blwidth(vthick) yscale(range(-1 1))

ac wt7, blwidth(vthick) yscale(range(-1 1))
pac wt7, blwidth(vthick) yscale(range(-1 1))


*************
* ARIMA(1,0,1)
*************
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\bjaarma11.csv"

gen time=_n

tsset time
twoway (line a time)

ac a, lags(60) blwidth(vthick) yscale(range(-1 1))

pac a, lags(60) blwidth(vthick) yscale(range(-1 1))

dfuller a

arima a, arima(1,0,1)
estat ic


* get residuals
predict et, residuals
ac et, blwidth(vthick) yscale(range(-1 1))
pac et, blwidth(vthick) yscale(range(-1 1))

arima a, arima(2,0,1)
estat ic


arima a, arima(1,0,2)
estat ic


arima a, arima(4,0,4)
* note: we need 18 interations to get converge


******************
* real examples
******************
* Monthly simple returns of 3M stock 
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\3M_return.csv"

tsset time

twoway (line r time, clcolor(blue)) 
dfuller r

ac r, blwidth(vthick)  yscale(range(-1 1))
pac r, blwidth(vthick) yscale(range(-1 1))

* ARIMA(0,0,0)
arima r, arima(0,0,0)

************************************************************
* US macro data
Macroeconomic Data, United States, 1970.1 to 1991.4
  GDP      = Gross Domestic Product, Billions of 1987 $
  PDI      = Personal Disposable Income, Billions of 1987 $
  PCE      = Personal Consumption Expenditure, Billions of 1987 $
  PROFITS  = Corporate Profits After Tax, Billions of 1987 $
  DIVIDEND = Net Corporate Dividends Payments, Billions of 1987 $
******************************************************************8
clear
infile  year  gdp  pdi   pce  profits  dividend using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\USGDPstata.txt"
gen time=_n
tsset time

gen lngdp = ln(gdp)
gen dlngdp = lngdp - L.lngdp

* test for stationary
twoway (line gdp time, clcolor(blue)) 
twoway (line lngdp time, clcolor(blue)) 
twoway (line dlngdp time, clcolor(blue)) 

dfuller gdp, trend
dfuller gdp, lags(4) trend

dfuller lngdp, trend
dfuller lngdp, lags(4) trend

dfuller dlngdp
dfuller dlngdp, lags(4)
ac lngdp, blwidth(vthick)  yscale(range(-1 1))

* identify ARMA order
* use ACF to identify MA order
*MA(1), MA(8)
 ac dlngdp, blwidth(vthick)  yscale(range(-1 1))

* use PACF to identify AR order
* AR(1), AR(8),AR(12)
pac dlngdp, blwidth(vthick) yscale(range(-1 1))

corrgram dlngdp, lags(25)
 
* ARIMA
arima dlngdp, ar(1,8,12) ma(1, 8)

arima dlngdp, arima(1,0,1)
arima lngdp, arima(1,1,1)

predict et, residuals

twoway (line et time, clcolor(blue)) 
ac et, blwidth(vthick)  yscale(range(-1 1))
pac et, blwidth(vthick) yscale(range(-1 1))

* forecast to original data
arima D.lngdp if time <60, ar(1,8,12) ma(1, 8)

* one step ahead in-sample forecast
predict lngdp_hat, y
twoway (line lngdp_hat time, clcolor(blue)) (line lngdp time, clcolor(black))

* dynamic forecast
* out of sample forecast
predict lngdp_hat3, dynamic(60) y


twoway (line lngdp_hat3 time, clcolor(blue)) (line lngdp time, clcolor(black))

* forecast error
gen error = lngdp - lngdp_hat3

twoway (line error time, clcolor(blue))


* GDP 1947 I - 1986 III ARIMA(2,1,2)
* may skip this example
clear 
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\gdp.csv"

tsset time
gen lngdp = ln(gdp)
twoway (line lngdp time, clcolor(blue)) 
ac lngdp, blwidth(vthick)  yscale(range(-1 1))
pac lngdp, blwidth(vthick) yscale(range(-1 1))

ac D.lngdp, blwidth(vthick)  yscale(range(-1 1))
pac D.lngdp, blwidth(vthick)  yscale(range(-1 1))
corrgram D.lngdp, lags(25)


arima lngdp, arima(2,1,2)
estat ic

* predict y option vs. xb option
predict lngdp_hat,y

twoway (line lngdp_hat time, clcolor(blue)) (line lngdp time, clcolor(black))
 
predict d_lngdp_hat
twoway (line d_lngdp_hat time, clcolor(blue)) (line D.lngdp time, clcolor(black))


predict e1, residuals
ac e1, blwidth(vthick)  yscale(range(-1 1))
pac e1, blwidth(vthick)  yscale(range(-1 1))

arima lngdp, arima(3,1,2)
estat ic


predict e2, residuals
ac e2, blwidth(vthick)  yscale(range(-1 1))
pac e2, blwidth(vthick)  yscale(range(-1 1))



*************************************
* cointegration and ECM models
*************************************
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\foodvsfuel_data.csv"

gen T = _n
tsset T

gen lnpg = ln(gasoline)
gen lnpo= ln(oil)


gen Yt=lnpo
gen Xt=lnpg

dfuller Yt, lags(4) trend reg
dfuller Xt, lags(4) trend reg

dfuller D.Yt, lags(4) trend reg
dfuller D.Xt, lags(4) trend reg


reg Yt Xt

predict ehat, residual

twoway (line ehat T)

dfuller ehat, noconstant

twoway (line lnpg T, clwidth(medthick))(line lnpo T, clwidth(medthick))

ac lnpg, blwidth(vthick) yscale(range(-1 1))
ac lnpo, blwidth(vthick) yscale(range(-1 1))

ac D.lnpg, blwidth(vthick) yscale(range(-1 1))
ac D.lnpo, blwidth(vthick) yscale(range(-1 1))

reg lnpo lnpg
predict et, residual
twoway (line et T, clwidth(medthick))

ac et, blwidth(vthick) yscale(range(-1 1))
dfuller et

var D.lnpo D.lnpg, exog(L.et) lags(1/4)

vargranger
D.lnpo ->D.lnpg
D.lnpg ->D.lnpo

* 12.0 version vec 
vec lnpo lnpg, lags(5)

predict ce, ce

line ce T

reg lnpo lnpg


* long-run Granger test results
* t-test
*  lnpo -> lnpg

* short-run Granger test results




*********************************
* ARCH and GARCH models
*********************************
clear
infile time exchange using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\exchange.txt"

tsset time
gen lny = ln(exchange)
gen dlny = lny - L.lny

twoway (line exchange time, clwidth(medthick))
twoway (line lny time, clwidth(medthick))
twoway (line dlny time, clwidth(medthick))

* ACF graph
ac lny, blwidth(vthick)

* ARIMA(1,1,1) or ARIMA(2,1,1)
ac dlny, blwidth(vthick) yscale(range(-1 1))
pac dlny, blwidth(vthick) yscale(range(-1 1))
corrgram dlny, lags(25)

* Dickey-Fuller unit root test 
dfuller lny, noconstant
dfuller dlny, noconstant

dfuller lny, lags(4) noconstant reg
dfuller lny, lags(4) reg
dfuller lny, lags(4) trend reg

dfuller dlny, lags(4) noconstant reg
dfuller dlny, lags(4) reg
dfuller dlny, lags(4) trend reg


* ARCH(1) model
arch lny, arch(1/1) arima(1,1,1)

arch lny, arch(1/1) arima(0,1,1)

* GARCH(1,1) model
arch lny, arch(1/1) garch(1/1) arima(0,1,1)


* intel return
clear
insheet using "F:\Econometrics_undergraduate\class_dataset\Chapter7_data\intel_return.csv"
tsset time

twoway (line r time, clwidth(medthick))

ac r, blwidth(vthick) yscale(range(-1 1))
pac r, blwidth(vthick) yscale(range(-1 1))

* ARCH(1) model

* test for arch effect
arima r, arima(0,0,0)
predict et, residuals

gen et_sq = et^2

reg et_sq L.et_sq L2.et_sq 

arch r, arch(1,2/2) arima(0,0,0)

arch r, arch(2/2) arima(0,0,0)


* GARCH(1,1) model
arch r, arch(1/1) garch(1/1) arima(0,0,0)




*! version 1.1, 11 Aug 1999 Modified from mlfit.ado by C F Baum 
*! Thanks to Vince Wiggins of StataCorp
*! version 1.1.12, 15 May 1998  STB-45 sg90 by A. Tobias & M.J. Campbell

program define arimafit, rclass
	version 6.0

	if ("`e(cmd)'"=="arima" & "`e(k_dv)'"=="1") {
    	local n  e(N)   
		local ll2  = -2*e(ll)
		local np = e(df_m)+1     
	} 
	else  {	
		display in red "ERROR: Last estimates not found" 
		display in red "Can only be used after single-equation arima command"
		exit
	} 

	local aic `ll2'+2*`np'
	local sic = `ll2'+`np'*ln(`n')
	display
	display in green " Criteria for assessing ARIMA model fit for `e(depvar)'"
	display in green " Akaike's Information Criterion (AIC) and Schwarz's Criterion (SIC)"
	display 
	display in green "------------------------------------------------------------------------------"
	display in green _col(2) "AIC" _col(16) "SIC" _col(29) "| -2 Log Likelihood" _col(52) "Num.Parameters"
	display in green "----------------------------+-------------------------------------------------"
	display _col(2) `aic' _col(16) `sic' _col(29) in green "|" _col(32) in ye `ll2' _col(55) `np'
	display in green "------------------------------------------------------------------------------"

    return scalar  np = `np'
    return scalar llf = `ll2'
    return scalar aic = `aic'
    return scalar sic = `sic'
end
