* --- Header
clear all
set more off
set mem 2000M
local odir "OUTPUT"
cap mkdir "OUTPUT"
log using "`odir'/strategic_estimate.log", replace


* --- overall punishment demand

tab part treatment if session == 1
tab part treatment if session == 2
tab part treatment if session == 3
tab part treatment if session == 4
tab part treatment if session == 5
tab part treatment if session == 6
tab part treatment if session == 7
tab part treatment if session == 8
tab part treatment if session == 9
tab part treatment if session == 10

gen gameorder = 1 if treatment == 41 & (session == 1 | session == 3 | session == 4 | session == 5 | session == 6)
replace gameorder = 2 if treatment == 61 & (session == 1 | session == 3 | session == 4 | session == 5 | session == 6)
replace gameorder = 1 if treatment == 61 & (session == 2 | session > 6)
replace gameorder = 2 if treatment == 41 & (session == 2 | session > 6)

egen pseudo = group(gameorder order N)


gen playorder = 1 if session == 1 | session == 3 | session == 4 | session == 5 | session == 6
replace playorder = 2 if session == 2 | session > 6


gen lowest = (con1 < con2 & con1 < con3 & con1 < contrib)
gen wp = (treatment == 61)
gen con1_inv61 = con1_inv * wp
gen paydiff61 = paydiff * wp
gen lowest61 = lowest * wp

xtset sid pseudo

* --- globally linear 
label var PUN "d_{ij}"
label var con1_inv "(20-c_j)"
label var wp "D.WLS"
label var con1_inv61 "D.WLS\times (20-c_j)"
label var lowest "D.min(c_{jkl})"
label var lowest61 "D.WLS\times D.min(c_{jkl})"


** -- con1_inv

gen stable = (t_con1_inv41 == t_con1_inv61 & t_con1_inv41 != 100 )
xtreg PUN con1_inv wp con1_inv61 if stable == 1, cluster(sid) fe

eststo clear 

xtreg PUN con1_inv if treatment == 41, cluster(sid) fe
eststo
xtreg PUN con1_inv if treatment == 61, cluster(sid) fe
eststo
xtreg PUN con1_inv wp con1_inv61, cluster(sid) fe
eststo
xtreg PUN con1_inv wp con1_inv61 if stable == 1, cluster(sid) fe
eststo
xtreg PUN con1_inv lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN con1_inv lowest if treatment == 61, cluster(sid) fe
eststo
xtreg PUN con1_inv wp con1_inv61 lowest lowest61, cluster(sid) fe
eststo
xtreg PUN con1_inv wp con1_inv61 lowest lowest61 if stable == 1, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/con1_inv.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace


** -- 2D punishers

eststo clear 
gen punpun = (t_con1_inv41 == -1 & t_con1_inv61 == -1)
gen rp = (treatment == 41)

xtreg PUN con1_inv if treatment == 41 & punpun == 1, cluster(sid) fe
eststo
xtreg PUN con1_inv if treatment == 61 & punpun == 1, cluster(sid) fe
eststo
xtreg PUN con1_inv wp con1_inv61 if punpun == 1, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/punpun_inv.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace





gen sdiff61 = sdiff * wp
xtreg PUN con1_inv wp con1_inv61 lowest lowest61, cluster(sid) fe
xtreg PUN sdiff wp sdiff61 lowest lowest61, cluster(sid) fe
xtreg PUN con1_inv wp con1_inv61 , cluster(sid) fe
xtreg PUN sdiff wp sdiff61 lowest lowest61, cluster(sid) fe

** -- paydiff

eststo clear 

xtreg PUN paydiff if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff if treatment == 61, cluster(sid) fe
eststo
xtreg PUN paydiff wp paydiff61, cluster(sid) fe
eststo
xtreg PUN paydiff lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff lowest if treatment == 61, cluster(sid) fe
eststo
xtreg PUN paydiff wp paydiff61 lowest lowest61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/paydiff_lin.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace



** -- appendix

xtreg PUN con1_inv if treatment == 41, cluster(sid) fe
eststo
xtreg PUN con1_inv if treatment == 61, cluster(sid) fe
eststo

xtreg PUN paydiff if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff if treatment == 61, cluster(sid) fe
eststo

xtreg PUN con1_inv lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN con1_inv lowest if treatment == 61, cluster(sid) fe
eststo

xtreg PUN paydiff lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff lowest if treatment == 61, cluster(sid) fe
eststo

esttab, se

* --- piecewise linear

** -- sdiff

eststo clear

gen sdiff_az61 = sdiff_az * wp
gen sdiff_bz61 = sdiff_bz * wp

xtreg PUN sdiff_bz sdiff_az if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 , cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 lowest lowest61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/sdiff.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

tab playorder

** -- sdiff if RP first

eststo clear
preserve
keep if playorder == 1
drop if session 1

gen sdiff_az61 = sdiff_az * wp
gen sdiff_bz61 = sdiff_bz * wp

xtreg PUN sdiff_bz sdiff_az if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 , cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 lowest lowest61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/sdiff.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

restore

** -- sdiff if WP first

eststo clear
preserve
keep if playorder == 2
drop if session == 2

gen sdiff_az61 = sdiff_az * wp
gen sdiff_bz61 = sdiff_bz * wp

xtreg PUN sdiff_bz sdiff_az if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 , cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 lowest lowest61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/sdiff.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

restore

** -- by treatment
xtreg PUN sdiff_bz sdiff_az if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az if treatment == 61, cluster(sid) fe
eststo

xtreg PUN paydiff_bz paydiff_az if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff_bz paydiff_az if treatment == 61, cluster(sid) fe
eststo

xtreg PUN sdiff_bz sdiff_az lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiff_bz sdiff_az lowest if treatment == 61, cluster(sid) fe
eststo

xtreg PUN paydiff_bz paydiff_az lowest if treatment == 41, cluster(sid) fe
eststo
xtreg PUN paydiff_bz paydiff_az lowest if treatment == 61, cluster(sid) fe
eststo

** -- interaction effects
gen sdiff_bz61 = sdiff_bz * wp
gen sdiff_az61 = sdiff_az * wp
gen paydiff_bz61 = paydiff_bz * wp
gen paydiff_az61 = paydiff_az * wp
gen sdiff_az61 = sdiff_az * wp
gen lowest61 = lowest * wp
gen outcon161 = outcon1 * wp

eststo clear

  xtreg PUN con1_inv if treatment == 41, cluster(sid) fe
eststo
  xtreg PUN con1_inv if treatment == 61, cluster(sid) fe
eststo
  xtreg PUN outcon1 if treatment == 41, cluster(sid) fe
eststo
  xtreg PUN outcon1 if treatment == 61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/aptab_estimation_discussion.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

  xtreg PUN sdiff_bz sdiff_az sdiff_bz61 sdiff_az61 lowest lowest61, cluster(sid) fe
eststo
  xtreg PUN paydiff_bz paydiff_az paydiff_bz61 paydiff_az61 lowest lowest61, cluster(sid) fe
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )
esttab using "img/aptab_estimation_discussion.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace


corr con1_inv outcon1
corr sdiff paydiff

** -- save for R bubbles
preserve
collapse b_con1_inv41 b_con1_inv61 playorder, by(sid)
corr b_con1_inv41 b_con1_inv61

save "DTA/weakest_beta.dta", replace


gen play = (playorder == 1)

reg b_con1_inv41 b_con1_inv61, noconst
reg b_con1_inv41 b_con1_inv61 play, noconst

restore


* --- collapse to group level

use "DTA/weakest_link_class.dta", clear

do 022_cc_humpshape_retype.do

collapse session t_con1_inv41 t_con1_inv61 t_paydiff41 t_paydiff61 t_cell51, by(sid)
save "TEMP/weakest_t_sid.dta", replace

use "DTA/7_wave.dta", clear

keep if treatment == 21 | treatment == 71

merge m:1 sid using "TEMP/weakest_t_sid.dta", nogenerate

drop gid
egen gid = group(group session)

gen single41 = (t_con1_inv41 == -1)
gen single61 = (t_con1_inv61 == -1)
gen single51 = (t_cell51 == -1)

gen punpun = (t_con1_inv41 == -1 & t_con1_inv61 == -1)

gen equi = (contrib == con1 & con1 == con2 & con2 == con3)
egen minimum = rowmin(contrib con1 con2 con3)

save "TEMP/weakest_tmp.dta", replace
tab equi treatment

collapse punpun session PUN contrib single51 single41 minimum single61 equi,by(treatment period gid)

gen gameorder = 1
replace gameorder = 0 if session == 1 | ( session > 3 & session < 7)


gen count41 = single41 * 4
gen count61 = single61 * 4
gen count51 = single51 * 4

gen payoff = 20 - contrib + 4 * 0.4 * contrib - 10 * PUN

save "DTA/wlrp_R.dta", replace


use "DTA/wlrp_R.dta",  clear

tab single41
tab single61
tab single51
tab equi treatment

  ///drop if gid == 51

tab count41 treatment
tab count61 treatment
tab count51 treatment

tab count41 count51
tab count61 count51

gen count412 =  (count41 == 2)
gen count4134 = (count41 > 2)
gen count612 =  (count61 == 2)
gen count6134 = (count61 > 2)
gen count512 =  (count51 == 2)
gen count5134 = (count51 > 2)
gen count41few =  (count41 == 2 | count41 == 1)



** -- panel group composition (RP)

eststo clear

egen periodt = group(period treatment gameorder)
xtset gid periodt

xtreg contrib count41 if treatment == 21, vce(bootstrap _b[count41])
eststo
estat ic
xtreg contrib count51 if treatment == 21, cluster(gid) re
eststo
estat ic
xtreg contrib count41 count51 if treatment == 21, cluster(gid) re
eststo
estat ic

xtreg payoff count41 if treatment == 21, cluster(gid) re
eststo
xtreg payoff count51 if treatment == 21, cluster(gid) re
eststo
xtreg payoff count41 count51 if treatment == 21, cluster(gid) re
eststo

esttab, b(3) se(3) r2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )

esttab using "img/group_21.tex", b(3) se(3) r2b aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace


xtreg contrib count41 if treatment == 21,  mle
estat ic
xtreg contrib count51 if treatment == 21,  mle
estat ic
xtreg contrib count41 count51 if treatment == 21,  mle
estat ic

xtreg payoff count41 if treatment == 21,  mle
estat ic
xtreg payoff count51 if treatment == 21,  mle
estat ic
xtreg payoff count41 count51 if treatment == 21,  mle
estat ic




tab count41 count51
eststo clear

xtreg contrib count412 count4134 if treatment == 21, cluster(gid)
eststo
xtreg contrib count512 count5134 if treatment == 21, cluster(gid)
eststo
xtreg contrib count412 count4134 count512 count5134 if treatment == 21, cluster(gid)
eststo

xtreg payoff count412 count4134 if treatment == 21, cluster(gid)
eststo
xtreg payoff count512 count5134 if treatment == 21, cluster(gid)
eststo
xtreg payoff count412 count4134 count512 count5134 if treatment == 21, cluster(gid)
eststo

esttab, b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )

esttab using "img/group_21_dummy.tex", b(3) se(3) ar2 aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

restore

** -- group composition (WPS)
eststo clear

xtreg contrib count61 if treatment == 71, cluster(gid)
eststo
xtreg contrib count51 if treatment == 71, cluster(gid)
eststo
xtreg contrib count61 count51 if treatment == 71, cluster(gid)
eststo

eststo clear
** -- equilibrium coordination (all)
xtprobit equi count41 if treatment == 71
eststo
xtprobit equi count61 if treatment == 71
eststo
xtprobit equi count51 if treatment == 71
eststo
xtprobit equi count61 count51 if treatment == 71
eststo

** -- equilibrum coordination (payoff dominant)
gen equi20 = (equi == 1 & contrib == 20)

xtprobit equi20 count41 if treatment == 71
eststo
xtprobit equi20 count61 if treatment == 71
eststo
xtprobit equi20 count51 if treatment == 71
eststo
xtprobit equi20 count61 count51 if treatment == 71
eststo

esttab, b(3) se(3) aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )

esttab using "img/group_71_equi_dummy.tex", b(3) se(3) aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

tab gid if count61 == 0 & treatment == 71

tab equi contrib if gid == 11
tab equi contrib if gid == 40
tab equi contrib if gid == 56
tab equi contrib if gid == 57

tab gid if contrib == 20 & equi == 1 & count61 == 0 & treatment == 71
tab gid if equi == 1 & count61 == 0 & treatment == 71
tab contrib if equi == 1 & gid == 40 & treatment == 71

gen few61 = (count61 == 1 | count61 == 2)
gen many61 = (count61 == 3 | count61 == 4)

xtprobit equi few61 many61, vce(bootstrap _b[few61] _b[many61])

eststo clear
xtprobit equi count412 count4134 if treatment == 71, vce(bootstrap _b[count412] _b[count4134])
eststo
xtprobit equi count612 count6134 if treatment == 71
eststo
xtprobit equi count512 count5134 if treatment == 71
eststo
xtprobit equi count612 count6134 count512 count5134 if treatment == 71, vce(bootstrap _b[count612] _b[count6134] _b[count512] _b[count5134])
eststo

esttab, b(3) se(3) aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )

esttab using "img/group_71_equi_dummy_sep.tex", b(3) se(3) aic(0) bic(0) label star(* 0.1 ** 0.05 *** 0.01 )  replace

    /// no result

xtprobit equi20 count412 count4134 if treatment == 71

xtprobit equi20 count612 count6134 if treatment == 71

xtprobit equi20 count512 count5134 if treatment == 71

xtprobit equi20 count612 count6134 count512 count5134 if treatment == 71

tab count61 count51, exact


gen punpunf = 4 * punpun

xtreg contrib punpunf if treatment == 21, cluster(gid)
xtreg contrib punpunf if treatment == 71, cluster(gid)

xtprobit equi punpunf count51 if treatment == 71

xtprobit equi20 punpunf count51 if treatment == 71

gen few41 = (count41 > 0 & count41 < 3)
gen few61 = (count61 > 0 & count61 < 3)
  ///gen few51 = (count51 > 0 & count51 < 3)
gen few51 = (count51 == 2 )

gen many41 = (count41 > 2)
gen many61 = (count61 > 2)
gen many51 = (count51 > 2)

xtreg contrib few41 many41 if treatment == 21, cluster(gid)
xtreg contrib few61 many61 if treatment == 71, cluster(gid)
xtreg contrib few51 many51 if treatment == 71, cluster(gid)

xtprobit equi few61 many61 if treatment == 71

xtlogit  equi few61 many61 if treatment == 71

xtprobit equi few51 many51 if treatment == 71

probit equi few61 many61 if treatment == 71
probit equi few51 many51 if treatment == 71

tab count61 count51 if treatment == 71

tab few61 few51 if treatment == 71
tab many61 many51 if treatment == 71

xtprobit equi few61 many61 few51 many51 if treatment == 71

gen equi20 = (equi == 1 & contrib == 20)

xtprobit equi20 few61 many61 if treatment == 71

xtlogit  equi20 few61 many61 if treatment == 71

xtprobit equi20 few61 many61 few51 many51 if treatment == 71
xtprobit equi20 count61 count51 if treatment == 71

gen nfew61 = (count61 == 2)
gen nfew51 = (count51 == 2)
gen nmany61 = (count61 > 2)
gen nmany51 = (count51 > 2)

xtprobit equi20 nfew61 nmany61 if treatment == 71

xtlogit  equi20 nfew61 nmany61 if treatment == 71

xtprobit equi20 nfew61 nmany61 nfew51 nmany51 if treatment == 71

xtprobit equi20 count61 count51 if treatment == 71

tab count61 count51 if treatment == 71 , chi2
spearman count61 count51 if treatment == 71

* ----------------------------------------------------------------------
* --- After Mail from Sebastian ---
use "DTA/weakest_link_class.dta", clear

egen minimum = rowmin(contrib con1 con2 con3)

gen ismin = (con1 < contrib & con1 < con2 & con1 < con3)
gen iscontribmin = (contrib == minimum)
gen iscon1min = (con1 == minimum)
gen notmincon1 = con1 - minimum

preserve
keep if treatment == 61
tab ismin
tab iscontribmin

count if PUN > 0 & iscon1min != 1


lgraph PUN notmincon1, errortype(se)
gr export "OUTPUT/Pun_notmincon1.eps", replace

tab notmincon1 PUN


restore

* ----------------------------------------------------------------------
* ---------------------------- OLD ------------------------------
* ----------------------------------------------------------------------
* --- graphical comparison of punishment behavior (individual)
** -- Value labels for treatments
cap label define treats 21 "DCP"
cap label define treats 31 "P-Game", add
cap label define treats 41 "SM-RP-Game", add
cap label define treats 51 "C-Game", add
cap label define treats 61 "SM-WP-Game", add
cap label define treats 71 "WP-Game", add
cap label value treament treats

** -- Generate variables
gen con1_inv = 20 - con1
gen sdiff = contrib - con1
** -- Individual punishment in 5 games (con1_inv)
preserve
    drop if treatment == 51
    egen countID = group(sid)
    forvalue i = 1/44{
        lgraph PUN con1_inv treatment if countID == `i'
        gr export "`odir'con1_inv_`i'.eps", replace
    }
restore

** -- Individual punishment in 5 games (sdiff)
preserve
    drop if treatment == 51
    egen countID = group(sid)
    forvalue i = 1/44{
        lgraph PUN sdiff treatment if countID == `i'
        gr export "`odir'sdiff_`i'.eps", replace
    }
restore

* --- comparing individual contribution in P RP (Period 1 & 10) PWL (Period 1 & 10)
** -- ranksum for equality across orders

preserve
    collapse contrib, by(sid treatment)
    gen order = 0
    replace order = 1 if sid > 70200
    ranksum contrib if treatment == 61 , by(order)
    ranksum contrib if treatment == 41 , by(order)
restore

ranksum contrib if treatment == 61 , by()

** -- generating data sets for merge
*** - C-game (51)
preserve
    keep if treatment == 51
    collapse contrib, by(sid)
    rename contrib contrib51
    save "TEMP/contrib51.dta", replace
restore
*** - P-game (31)
preserve
    keep if treatment == 31
    collapse contrib, by(sid)
    rename contrib contrib31
    save "TEMP/contrib31.dta", replace
restore
*** - DCP (21) Period 1
preserve
    keep if treatment == 21 & period == 1
    collapse contrib, by(sid)
    rename contrib contrib211
    save "TEMP/contrib211.dta", replace
restore
*** - DCP (21) Period 10
preserve
    keep if treatment == 21 & period == 10
    collapse contrib, by(sid)
    rename contrib contrib2110
    save "TEMP/contrib2110.dta", replace
restore
*** - PWL (71) Period 1
preserve
    keep if treatment == 71 & period == 1
    collapse contrib, by(sid)
    rename contrib contrib711
    save "TEMP/contrib711.dta", replace
restore
*** - PWL (71) Period 10
preserve
    keep if treatment == 71 & period == 10
    collapse contrib, by(sid)
    rename contrib contrib7110
    save "TEMP/contrib7110.dta", replace
restore
*** - Join datasets
use "TEMP/contrib51.dta", clear
merge 1:1 sid using "TEMP/contrib31.dta", nogenerate
merge 1:1 sid using "TEMP/contrib211.dta", nogenerate
merge 1:1 sid using "TEMP/contrib2110.dta", nogenerate
merge 1:1 sid using "TEMP/contrib711.dta", nogenerate
merge 1:1 sid using "TEMP/contrib7110.dta", nogenerate
su


*** - Test ordering effect on contribution
signrank contrib211 = contrib711
signrank contrib211 = contrib711 if sid < 70200
signrank contrib211 = contrib711 if sid > 70200

signrank contrib211 = contrib2110
signrank contrib211 = contrib2110 if sid < 70200
signrank contrib211 = contrib2110 if sid > 70200

signrank contrib711 = contrib7110
signrank contrib711 = contrib7110 if sid < 70200
signrank contrib711 = contrib7110 if sid > 70200


* --- testing whether strategy method behavior changed in RP
use "DTA/`wave'_wave.dta", clear

    /* copy order from period 21 to realOrder in treatment 41*/
drop if treatment > 50
gen tmp_order = order if period == 1 & treatment == 21
bysort sid (tmp_order): egen realOrder = mean(tmp_order)
tab realOrder treatment
tab order realOrder
    /* compare order of hypothetical and set dummy for all triples AFTER real triple */
gen afterReal = 0
replace afterReal = 1 if order > realOrder

keep if treatment == 41
gen con1_inv = 20 - con1
    /* session 1 and 2 */
reg PUN con1_inv afterReal, cluster(sid)
    /* only session 2 */
reg PUN con1_inv afterReal if sid > 70200, cluster(sid)

/* No significant impact on punishment behavior after seeing real triple */

    /* removing weird subjects from session */
    /* session 1 = 11; session 2 = 17 */
drop if sid == 70111
drop if sid == 70217

    /* session 1 and 2 */
reg PUN con1_inv afterReal, cluster(sid)
    /* only session 2 */
reg PUN con1_inv afterReal if sid > 70200, cluster(sid)

* --- graphs WP group contribution by session
use "DTA/`wave'_wave.dta", clear
keep if treatment == 71 | treatment == 21

collapse contrib PUN session, by(period group session treatment)

/* graphs weakest link session 1 and 2 */
lgraph contrib period group if session == 1 & treatment == 71
gr export "`odir'wp_contrib_1.eps", replace
lgraph contrib period group if session == 2 & treatment == 71
gr export "`odir'wp_contrib_2.eps", replace

* --- graphs RP group contribution by session

/* graphs contribution repeated public goods session 1 and 2 */
lgraph contrib period group if session == 1 & treatment == 21
gr export "`odir'rp_contrib_1.eps", replace
lgraph contrib period group if session == 2 & treatment == 21
gr export "`odir'rp_contrib_2.eps", replace

* --- individual punishment classification 31, 41, 61




* -- individual level classification
* --- Header
clear all
set more off
set mem 2000M
local odir "OUTPUT"
cap mkdir "OUTPUT"
log using "`odir'/strategic_estimate.log", replace

local wave = 7
use "DTA/`wave'_wave.dta", clear
keep if wave == 7

keep if treatment == 41 | treatment == 61 | treatment == 51

gen con1_inv = 20 - con1
egen mincontrib = rowmin(contrib con1 con2 con3)
gen outcontrib = 20 - contrib + 1.6 * mincontrib if treatment == 61
gen outcon1 = 20 - con1 + 1.6 * mincontrib if treatment == 61
gen outcon2 = 20 - con2 + 1.6 * mincontrib if treatment == 61
gen outcon3 = 20 - con3 + 1.6 * mincontrib if treatment == 61
replace outcon1 = 20 - con1 + 0.4* (contrib + con1 + con2 + con3) if treatment == 41
replace outcontrib = 20 - contrib + 0.4* (contrib + con1 + con2 + con3) if treatment == 41
gen paydiff = outcon1 - outcontrib
egen ID = group(sid)

gen sdiff = contrib - con1
gen dummy_sdiff_bz = (sdiff <= 0)
gen dummy_sdiff_az = (sdiff > 0)
gen sdiff_bz = sdiff * dummy_sdiff_bz
gen sdiff_az = sdiff * dummy_sdiff_az

gen dummy_paydiff_bz = (paydiff <= 0)
gen dummy_paydiff_az = (paydiff > 0)
gen paydiff_bz = paydiff * dummy_paydiff_bz
gen paydiff_az = paydiff * dummy_paydiff_az

bysort ID: RegStore PUN con1_inv,  treatment(41) robust labeling(20 - con1; RP)
bysort ID: RegStore PUN con1_inv,  treatment(61) robust labeling(20 - con1; WP)

bysort ID: RegStore PUN paydiff,  treatment(41) robust labeling(payoff; RP)
bysort ID: RegStore PUN paydiff,  treatment(61) robust labeling(payoff; WP)

bysort ID: RegStore condcon cell, treatment(51) robust labeling(CC; C)

ClassifyPun PUN con1_inv, treatment(41)
ClassifyPun PUN con1_inv, treatment(61)
ClassifyPun PUN paydiff, treatment(41)
ClassifyPun PUN paydiff, treatment(61)

ClassifyPun condcon cell, treatment(51)

save "DTA/weakest_link_class.dta", replace
