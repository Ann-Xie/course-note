* wage data
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics\WAGE1.RAW", clear

reg wage educ exper

reg wage educ
* partialing out interpretation
qui reg educ exper
predict educ_res, r

qui reg wage exper
predict wage_res, r

reg wage_res educ_res

reg wage educ_res

reg wage

** test
reg wage educ exper
test educ=exper
test educ
dis sqrt(r(F))

test educ exper

* restricted model
reg wage

* extend to more independent x variables
reg wage educ exper tenure

*reg wage tenure

test educ exper
test exper tenure
test exper = tenure
test exper+tenure=1

test (exper+tenure=1) (educ=0)


ereturn list

matrix list e(V)

matrix list e(b)

dis _b[educ]

* US chicken demand vs. zhang's
clear
insheet using "F:\Econometrics\chicken_data.csv"

gen lny = ln(y)
gen lnx2 = ln(x2)
gen lnx3 = ln(x3)
gen lnx4 = ln(x4)
gen lnx5 = ln(x5)
gen lnx6 = ln(x6)

reg lny lnx2 lnx3

reg lny lnx2 lnx3 lnx4

reg lny lnx2 lnx3 lnx5

reg lny lnx2 lnx3 lnx4 lnx5

reg lny lnx2 lnx3 lnx6


* Translog production function vs. Cobb-Douglas production function
******************************************
*  Production Data For SIC 33: Primary Metals, 27 Statewide Observations
Source and Note: Data are per establishment, labor is a measure of labor input, and capital is the gross value of
plant and equipment. A scale factor used to normalize the capital figure in the original study has been
omitted. Further details on construction of the data are given in Aigner, et al. (1977) and in
Hildebrand and Liu (1957).

    * Obs = Observation number
    * Valueadd = Value added, (y)
    * Labor = Labor input, (L)
    * Capital = Capital stock. (K)
**********************************************

clear
infile obs y l k using "F:\Econometrics\table5.txt"

gen lny = ln(y)
gen lnL = ln(l)
gen lnK = ln(k)

gen lnL_sq = 0.5*lnL^2
gen lnK_sq = 0.5*lnK^2
gen lnL_K = lnL*lnK

* Translog production function
reg lny lnL lnK lnL_sq lnK_sq lnL_K

test lnL_sq lnK_sq lnL_K

reg lny lnL lnK


*********************************
* glm lny lnL lnK lnL_sq lnK_sq lnL_K
* test lnL_sq lnK_sq lnL_K
* testnl

* SSEur = 0.6799


* Cobb-Douglas production function
reg lny lnL lnK

*SSEr = 0.8516

* test for constant return to scale
test _b[lnL]+_b[lnK]=1

test lnL + lnK = 1











