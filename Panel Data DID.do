***************
* Panel Data
***************


clear
set matsize 800

infile nr year black exper hisp hours married occ1 occ2 occ3 occ4 occ5 occ6 occ7 occ8 occ9 educ union lwage d81 d82 d83 d84 d85 d86 d87 expersq using "F:\Econometrics_undergraduate\class_dataset\Chapter8_data\WAGEPAN.RAW", clear 

* 11.0 version: xtset nr year
tsset nr year

xtset nr year


* Pooled OLS
reg lwage educ black hisp exper expersq married union

*fixed effect 
tabulate nr

qui tabulate nr, gen(man)

* LSDV
xi: reg lwage educ black hisp exper expersq married union i.nr, noconstant

*FE
xtreg lwage educ black hisp exper expersq married union, i(nr) fe

xtreg lwage educ black hisp exper expersq married union i.year, fe

* random effect
xtreg lwage educ black hisp exper expersq married union
xtreg lwage educ black hisp exper expersq married union, re


* fixed vs. random effects
estimates store random_effects

xtreg lwage educ black hisp exper expersq married union, fe

hausman . random_effects


**********************************************************************************
* Appendix: Policy Analysis with Pooled Cross Sections
**********************************************************************************


* Difference-in-Difference estimator

insheet using F:\Econometrics_undergraduate\class_dataset\Chapter8_data\KIELMC.csv

sum

reg rprice nearinc if y81==1

reg rprice nearinc if y81==0

* DD estimation
gen y81_nearinc = y81*nearinc

reg rprice y81 nearinc y81_nearinc

reg rprice y81 nearinc y81_nearinc age agesq

* log(rprice)
reg lrprice y81 nearinc y81_nearinc

reg lrprice y81 nearinc y81_nearinc age agesq
