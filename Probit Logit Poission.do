
********************************
home ownership data
********************************
clear
insheet using "F:\Econometrics\homeowner.csv", clear

* LPM estimation
reg y x

predict yhat, xb

twoway (scatter y x)(line yhat x)

* Logit estimation
logit y x
* note: outcome = x > 14 predicts data perfectly
scatter y x 


*********************
 GPA data
**********************
clear
insheet using "F:\Econometrics\GPA.csv"

* logit model

reg y gpa tuce psi

matrix b=e(b)
logit y gpa tuce psi, from(b, copy)

logit y

predict phat, p
list phat
predict logithat, xb
sort logithat
twoway(line phat logithat)

gen yhat = -1
replace yhat =1 if phat>0.5
replace yhat =0 if phat<0.5
table yhat y, contents(freq)

logit y gpa tuce psi
logit y


* probit model
probit y gpa tuce psi
predict phat2, p

dprobit y gpa tuce psi


******************
* credit example
******************
clear
infile ID str13 SEX str13 MAJOR  AGE GPT HRS str13 RISK using "F:\Econometrics\credit.txt"

describe

sum 
tab MAJOR
tab SEX
tab RISK


gen NRISK = 0
replace NRISK=1 if RISK=="BAD"

gen NSEX = 0 
replace NSEX = 1 if SEX=="MALE"

* Science(SCI)
gen Dum_MAJOR1 = 0
replace Dum_MAJOR1 = 1 if MAJOR=="SCI"

* Soical Science (SOC)
gen Dum_MAJOR2 = 0
replace Dum_MAJOR2 = 1 if MAJOR=="SOC"

* Humanities (HUM)
gen Dum_MAJOR3 = 0
replace Dum_MAJOR3 = 1 if MAJOR=="HUM"

* Business (BUS)
* HRS: hours worked per week

* Logit

logit NRISK NSEX Dum_MAJOR1 Dum_MAJOR2 Dum_MAJOR3 GPT AGE HRS

predict P_risk, p

gen str13 Predicted_Risk = "GOOD" 
replace Predicted_Risk = "BAD" if P_risk>0.5

tabulate RISK Predicted_Risk, row 

*lrtest NSEX Dum_MAJOR1 Dum_MAJOR2 Dum_MAJOR3
estimates store A

logit NRISK GPT AGE HRS
estimates store B

lrtest A B

lrtest A B, stats

* Probit

probit NRISK NSEX Dum_MAJOR1 Dum_MAJOR2 Dum_MAJOR3 GPT AGE HRS

predict P_risk2, p
gen str13 Predicted_Risk2 = "GOOD" 
replace Predicted_Risk2 = "BAD" if P_risk2>0.5

tabulate RISK Predicted_Risk2, row 



* compare logit with probit
clear
insheet using "F:\Econometrics\probitplot.csv"

twoway (line logit z, clcolor("0 0 0") clpat(solid) clwidth(medthick)) (line probit z, clcolor("255 0 0")clpat(dash) clwidth(medthick))
 


*************************
labor union
*************************
 EARN.ASC
 Source: The Panel Study of Income Dynamics, taken from Cornwell and Rupert (1988).
 Description:  Cross-Section, 595 individuals (1982).
 Variables:
 (1) EXP = Years of full-time work experience.
 (2) EXP2 = Square of EXP.
 (3) WKS =  Weeks worked.
 (4) OCC =  (OCC=1, if the individual is in a blue-collar occupation).
 (5) IND =  (IND=1, if the individual works in a manufacturing industry).
 (6) SOUTH =  (SOUTH=1, if the individual resides in the South)
 (7) SMSA =  (SMSA=1, if the individual resides in a standard metropolitan      
              statistical area).
 (8) MS =  (MS=1, if the individual is married).
 (9) FEM =  (FEM=1, if the individual is female).
(10) UNION =  (UNION=1, if the individual's wage is set by a union                 
               contract).
(11) ED = Years of education.
(12) BLK =  (BLK=1, if the individual is black).
(13) LWAGE =  Logarithm of wage.
(14) M =  (M=1, if the individual is male).
(15) F_EDC =  Years of education for females only.

clear
insheet using "F:\Econometrics\union.csv" 

* logit specification
* unrestricted model
logit union exp wks occ ind south smsa ms fem ed blk

* likelihood-ratio test
estimates store UR

* restricted model
logit union wks south smsa ms ed

estimates store R

lrtest UR R


predict phat, p
gen yhat = -1
replace yhat =1 if phat>0.5
replace yhat =0 if phat<0.5
table yhat union, contents(freq)


* probit specification
probit union exp wks occ ind south smsa ms fem ed blk

dprobit union exp wks occ ind south smsa ms fem ed blk

predict phat2, p
gen yhat2 = -1
replace yhat2 =1 if phat>0.5
replace yhat2 =0 if phat<0.5
table yhat2 union, contents(freq)


***************************
* Propensity Score Analysis
***************************
* pscore 
*********
* nodeg = no degree (1 if no degree; 0 othewise)
* treatment indicator (1 if treated, 0 if not treated)
* age
* education
* Black (1 if black, 0 otherwise)
* Hispanic (1 if Hispanic, 0 otherwise)
* married (1 if married, 0 otherwise)*
* nodegree (1 if no degree, 0 otherwise)
* RE75 (earnings in 1975), and RE78 (earnings in 1978). 
* The last variable is the outcome; other variables are pre-treatment.

clear

global root F:\Econometrics\Chapter6_data
use $root\cps1re74.dta

sum age ed black hisp married nodeg re74 re75 re78 if treat==1
sum age ed black hisp married nodeg re74 re75 re78 if treat==0
 
gen ed2=ed^2
gen re74_2=re74^2
gen re75_2=re75^2

* teffects psmatch (re78) (treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2), atet
* pscore estimation 

pscore treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2, pscore(mypscore) blockid(myblock) comsup numblo(5) level(0.005) logit

sum age ed black hisp married nodeg re74 re75 re78 if treat==1 & mypscore>=0.0008067 & mypscore<0.0125
sum age ed black hisp married nodeg re74 re75 re78 if treat==0 & mypscore>=0.0008067 & mypscore<0.0125

sum age ed black hisp married nodeg re74 re75 re78 if treat==1&mypscore>=0.0125&mypscore<0.025
sum age ed black hisp married nodeg re74 re75 re78 if treat==0&mypscore>=0.0125&mypscore<0.025


logit treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2 


* psgraph
* estimate average treatment effects
* Nearest Neighbor Matching
attnd re78 treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2, comsup boot reps(100) dots logit

* attnw re78 treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2, comsup boot reps(100) dots logit 

* Radius Matching
attr re78 treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2, comsup boot reps(100) dots logit radius(0.0001)

* Kernel Matching
attk re78 treat age age2 ed ed2 married black hisp re74 re75 re74_2 re75_2, comsup boot reps(100) dots logit

* Stratification Method
atts re78 treat, pscore(mypscore) blockid(myblock) comsup boot reps(100) dots


**************************
* Possion Regression Model
**************************
Crime1.DES
  Obs:  2725
  1. narr86                   # times arrested, 1986
  2. nfarr86                  # felony arrests, 1986
  3. nparr86                  # property crme arr., 1986
  4. pcnv                     proportion of prior convictions
  5. avgsen                   avg sentence length, mos.
  6. tottime                  time in prison since 18 (mos.)
  7. ptime86                  mos. in prison during 1986
  8. qemp86                   # quarters employed, 1986
  9. inc86                    legal income, 1986, $100s
 10. durat                    recent unemp duration
 11. black                    =1 if black
 12. hispan                   =1 if Hispanic
 13. born60                   =1 if born in 1960
 14. pcnvsq                   pcnv^2
 15. pt86sq                   ptime86^2
 16. inc86sq                  inc86^2

infile narr86 nfarr86 nparr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan  born60  pcnvsq pt86sq inc86sq using "F:\Econometrics\CRIME1.RAW", clear 

sum
table narr86
histogram narr86, freq


* OLS
reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 black hispan born60
estimate store OLS

* Poisson
poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 black hispan born60
estimate store Poisson

estout OLS Poisson, cells(b(star fmt(3)) se(par fmt(2))) legend label starlevels(* 0.10 ** 0.05 *** 0.01) varlabels(_cons Constant) 

* passing intitial values to the ML function
reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 black hispan born60
matrix B=e(b)

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 black hispan born60, from(B)
