
**************************************************
* wage1 data -quadratic function form
**************************************************

infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics_\WAGE1.RAW", clear

reg wage exper expersq

sort exper, stable

predict wagehat, xb

twoway (scatter wage exper)(line wagehat exper)
twoway (line wagehat exper)

* compute partial derivative
gen dwage_dexper2 = _b[exper] + 2*_b[expersq]*exper


********************************************
* Attend data: interaction terms
*********************************************
  Obs:   680

  1. attend                   classes attended out of 32
  2. termgpa                  GPA for term
  3. priGPA                   cumulative GPA prior to term
  4. ACT                      ACT score
  5. final                    final exam score
  6. atndrte                  percent classes attended
  7. hwrte                    percent homework turned in
  8. frosh                    =1 if freshman
  9. soph                     =1 if sophomore
 10. skipped                  number of classes skipped
 11. stndfnl                  (final - mean)/sd
****************************************************************

clear
infile attend termgpa priGPA ACT final atndrte hwrte frosh soph skipped stndfnl using "F:\Econometrics\ATTEND.RAW", clear

gen priGPAsq = priGPA^2
gen ACTsq = ACT^2
gen priGPA_atndrte = priGPA*atndrte
 
reg stndfnl atndrte priGPA ACT priGPAsq ACTsq priGPA_atndrte


**********************
* test omitted variables
**********************
* examination of residuals

clear
insheet using "F:\Econometrics\production.csv"

gen x_sq = x^2
gen x_cubic = x^3

tsset x

reg y x
predict e_linear, residuals
dwstat

reg y x x_sq
predict e_quadratic, residuals
dwstat


reg y x x_sq x_cubic
predict e_cubic, residuals
dwstat

* residuals plots
twoway (line e_linear x), title(Residuals of Linear Model)
twoway (line e_quadratic x), title(Residuals of Quadratic Model)
twoway (line e_cubic x), title(Residuals of Cubic Model)

label variable e_linear "Residuals of Linear Model"
label variable e_quadratic "Residuals of Quadratic Model"
label variable e_cubic "Residuals of Cubic Model"

twoway (line e_linear x) (line e_quadratic x)(line e_cubic x)


* Ramsey's RESET test
reg y x
ovtest

reg y x x_sq
ovtest

reg y x x_sq x_cubic
ovtest



*****************************************
* missing data - BWAGHT data
*****************************************
infile faminc cigtax cigprice bwght fatheduc motheduc parity male white cigs lbwght bwghtlbs packs lfaminc using "F:\Econometrics\BWGHT.RAW", clear

sum

*****************************************
 missing data - LAWSCH85 data
*****************************************
clear
infile rank salary cost LSAT GPA libvol faculty age clsize north south east west lsalary studfac top10 r11_25 r26_40 r41_60 llibvol lcost using "F:\Econometrics\Chapter5_data\LAWSCH85.RAW", clear

sum

*******************************************************
outliers and influential obs.- rdchem data
*******************************************************
infile rd sales profits rdintens profmarg salessq lsales lrd using "F:\Econometrics\RDCHEM.RAW", clear

twoway (scatter rdintens sales)

reg rdintens sales profmarg


* delete outlier obs.
reg rdintens sales profmarg if sales < 38000



*****************
* Dummy variables
*****************
* WAGE1 data
clear
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics\WAGE1.RAW"

* intercept shift
reg wage female educ exper tenure

* different slopes
gen female_educ = female*educ

reg wage female educ female_educ exper tenure

test female female_educ


* Policy Analysis and Difference-in-Differences Estimation

* D-D: Card 1994 AER paper
 Code Book for New Jersey-Pennsylvania Data Set

Note: there are 410 observations in the data set
            

Column Location
 Name:       

Start    End    Format     Explanation
Sheet       
 1        3     3.0   sheet number (unique store id)
CHAIN           
5        5     1.0   chain 1=bk; 2=kfc; 3=roys; 4=wendys
CO_OWNED       
 7        7     1.0   1 if company owned
* STATE           
9        9     1.0   1 if NJ; 0 if Pa                      

Dummies for location:
SOUTHJ         
11       11     1.0   1 if in southern NJ
CENTRALJ       
13       13     1.0   1 if in central NJ
NORTHJ         
15       15     1.0   1 if in northern NJ
PA1            
17       17     1.0   1 if in PA, northeast suburbs of Phila
PA2           
19       19     1.0   1 if in PA, Easton etc
SHORE          
21       21     1.0   1 if on NJ shore

* First Interview
NCALLS         
23       24     2.0   number of call-backs*
* EMPFT          
26       30     5.2   # full-time employees
* EMPPT          
32       36     5.2   # part-time employees
* NMGRS          
38       42     5.2   # managers/ass't managers
WAGE_ST        
44       48     5.2   starting wage ($/hr)
INCTIME        
50       54     5.1   months to usual first raise
FIRSTINC       
56       60     5.2   usual amount of first raise ($/hr)
BONUS          
62       62     1.0   1 if cash bounty for new workers
PCTAFF         
64       68     5.1   % employees affected by new minimum
MEALS          
70       70     1.0   free/reduced price code (See below)
OPEN           
72       76     5.2   hour of opening
HRSOPEN        
78       82     5.2   number hrs open per day
PSODA          
84       88     5.2   price of medium soda, including tax
PFRY           
90       94     5.2   price of small fries, including tax
PENTREE        
96      100     5.2   price of entree, including tax
NREGS         
102      103     2.0   number of cash registers in store
NREGS11       
105      106     2.0   number of registers open at 11:00 am

* Second Interview
TYPE2         
108      108     1.0   type 2nd interview 1=phone; 2=personal
STATUS2       
110      110     1.0   status of second interview: see below
DATE2         
112      117     6.0   date of second interview MMDDYY format
NCALLS2       
119      120     2.0   number of call-backs*
EMPFT2        
122      126     5.2   # full-time employees
EMPPT2        
128      132     5.2   # part-time employees
NMGRS2        
134      138     5.2   # managers/ass't managers
WAGE_ST2      
140      144     5.2   starting wage ($/hr)
INCTIME2      
146      150     5.1   months to usual first raise
FIRSTINC2      
152      156     5.2   usual amount of first raise ($/hr)
SPECIAL2      
158      158     1.0   1 if special program for new workers
MEALS2        
160      160     1.0   free/reduced price code (See below)
OPEN2R        
162      166     5.2   hour of opening
HRSOPEN2      
168      172     5.2   number hrs open per day
PSODA2        
174      178     5.2   price of medium soda, including tax
PFRY2         
180      184     5.2   price of small fries, including tax
PENTREE2      
186      190     5.2   price of entree, including tax
NREGS2        
192      193     2.0   number of cash registers in store
NREGS112      
195      196     2.0   number of registers open at 11:00 am

Codes:

Free/reduced Meal Variable:
0 = none
1 = free meals
2 = reduced price meals
3 = both free and 

reduced price meals

Second Interview Status
0 = refused second interview (count = 1)
1 = answered 2nd interview (count = 399)
2 = closed for renovations (count =2)
3 = closed "permanently" (count = 6)
4 = closed for highway construction (count = 1)
5 = closed due to Mall fire (count = 1)


*************************************************

clear

infile SHEET CHAIN CO_OWNED STATE SOUTHJ CENTRALJ NORTHJ PA1 PA2 SHORE NCALLS EMPFT1 EMPPT1 NMGRS1 WAGE_ST1 INCTIME1 FIRSTINC1 BONUS1 PCTAFF1 MEAL1 OPEN1 HRSOPEN1 PSODA1 PFRY1 PENTREE1 NREGS1 NREGS_E1 TYPE2 STATUS2 DATE2 NCALLS2 EMPFT2 EMPPT2 NMGRS2 WAGE_ST2 INCTIME2 FIRSTINC2 SPECIAL2 MEAL2 OPEN2 HRSOPEN2 PSODA2 PFRY2 PENTREE2 NREGS2 NREGS_E2 using "F:\Econometrics\NewJersey_Pennsylvania\public.dat"


describe

gen id =_n


reshape long EMPFT EMPPT NMGRS WAGE_ST INCTIME FIRSTINC MEAL OPEN HRSOPEN PSODA PFRY PENTREE NREGS NREGS_E, i(id) j(year) 

* year dummy
gen time = 0
replace time=1 if year==2 

* already has state dummy 
      
gen EMPTOT = EMPPT*0.5 + EMPFT +NMGRS

*gen state2=STATE
order STATE
* state = NJ
sum EMPTOT if STATE==1 & time==0
sum EMPTOT if STATE==1 & time==1

* state = PA
sum EMPTOT if STATE==0 & time==0
sum EMPTOT if STATE==0 & time==1

gen DD = time*STATE


* D-D estimation
reg EMPTOT time STATE DD




* dummy variable trap

clear
infile wage educ exper tenure nonwhite female married numdep smsa northcen south west construc ndurman trcommpu trade services profserv profocc clerocc servocc lwage expersq tenursq using "F:\Econometrics\WAGE1.RAW"

gen male = 1-female

reg wage female male educ

reg wage female male educ, noconstant


* AIC, SIC Criterions

estat ic


estat hettest
estat vif


* Mellow's Cp
* download rsquare

