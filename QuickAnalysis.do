* --- Header
set more off
clear all
local odir "OUTPUT"
cap mkdir "OUTPUT"
log using "`odir'/strategic_estimate.log", replace

  /* local wave = "clean_all" */
local wave = "7"
use "DTA/`wave'_wave.dta", clear
keep if wave == 7
drop if block == 11

* --- punishment types
    /* keep if treatment == 31 | treatment == 41 | treatment == 61 */ 

** -- con1
gen con1_inv = 20 - con1
egen ID = group(sid)

bysort ID: RegStore PUN con1_inv,  treatment(31) robust labeling(20 - con1 / exogenous obs)
bysort ID: RegStore PUN con1_inv,  treatment(41) robust labeling(20 - con1 / exogenous obs)
bysort ID: RegStore PUN con1_inv,  treatment(61) robust labeling(20 - con1 / exogenous obs)

ClassifyPun PUN con1_inv, treatment(31)
ClassifyPun PUN con1_inv, treatment(41)
ClassifyPun PUN con1_inv, treatment(61)

** -- sdiff
gen sdiff = contrib - con1
gen sdiff_bz_dummy = 0
replace sdiff_bz_dummy = 1 if sdiff < 0
gen sdiff_bz = sdiff * sdiff_bz_dummy
gen sdiff_az_dummy = 0
replace sdiff_az_dummy = 1 if sdiff >= 0
gen sdiff_az = sdiff * sdiff_az_dummy

bysort ID: RegStore PUN sdiff_bz sdiff_az,  sp treatment(31) robust
bysort ID: RegStore PUN sdiff_bz sdiff_az,  sp treatment(41) robust
bysort ID: RegStore PUN sdiff_bz sdiff_az,  sp treatment(61) robust

ClassifyPun PUN sdiff_bz sdiff_az, treatment(31)
ClassifyPun PUN sdiff_bz sdiff_az, treatment(41)
ClassifyPun PUN sdiff_bz sdiff_az, treatment(61)

*** - Reclassification based on contrib

replace t_sdiff_bz31 = t_con1_inv31 if contrib < 5 | contrib > 15
replace t_sdiff_bz41 = t_con1_inv41 if contrib < 5 | contrib > 15
replace t_sdiff_bz61 = t_con1_inv61 if contrib < 5 | contrib > 15

** -- punishment type distribution
preserve
collapse t_con1_inv31 t_con1_inv41 t_con1_inv61, by(sid)
tab t_con1_inv31 t_con1_inv41
tab t_con1_inv31 t_con1_inv61
tabout t_con1_inv41 t_con1_inv61 using "OUTPUT/tab_con1_4161.tex" , style(tex) bt cell(freq cell) replace layout(row)
restore

** -- punishment type distribution
preserve
collapse t_sdiff_bz31 t_sdiff_bz41 t_sdiff_bz61, by( treatment sid)
keep if treatment == 31
tab t_sdiff_bz31 t_sdiff_bz41
tab t_sdiff_bz31 t_sdiff_bz61
tabout t_sdiff_bz41 t_sdiff_bz61 using "OUTPUT/tab_sdiff_4161.tex" , style(tex) bt cell(freq cell) replace layout(row)

restore

** -- save dta file


    /* generate ordering dummy for RP WP */
gen gameorder = 0
replace gameorder = 1 if sid > 70200 & sid < 70300
replace gameorder = 1 if sid > 70700


save "DTA/weakest_link.dta", replace

* ---------------------------------------------------------------
* ---------------------------------------------------------------
* ------------------ ANALYSIS -----------------------------------
* ---------------------------------------------------------------
use "DTA/weakest_link.dta", clear
drop if block == 11

* --- distribution of punishment betas

** -- line graphs punishment
lgraph PUN con1_inv treatment if treatment == 31 | treatment == 41 | treatment == 61
gr export "OUTPUT/lgraph__con1_PRPWP.eps", replace

lgraph PUN sdiff treatment if treatment == 31 | treatment == 41 | treatment == 61
gr export "OUTPUT/lgraph_sdiff_PRPWP.eps", replace

** - xtreg estimation slopes

preserve
keep if treatment == 31 | treatment == 41 | treatment == 61
gen op = 0
replace op = 1 if treatment == 31
gen rp = 0
replace rp = 1 if treatment == 41
gen wp = 0
replace wp = 1 if treatment == 61
gen sdiff_bztr31 = sdiff_bz * op
gen sdiff_aztr31 = sdiff_az * op
gen sdiff_bztr41 = sdiff_bz * rp
gen sdiff_aztr41 = sdiff_az * rp
gen sdiff_bztr61 = sdiff_bz * wp
gen sdiff_aztr61 = sdiff_az * wp

gen con1_invrp = con1_inv * rp
gen con1_invwp = con1_inv * wp
gen sdiffop = sdiff * op
gen sdiffrp = sdiff * rp
gen sdiffwp = sdiff * wp

tab part treatment
gen porder = 0
replace porder = 1 if session > 2
gen rporder = 0
replace rporder = 1 if session == 1 | (session > 2 & session < 7)
gen dualorder = porder * rporder

gen pseudotr = 100000 * part + 1000 * treatment + block * 10 + N
xtset sid pseudotr

eststo clear

xtreg PUN con1_inv rp wp, fe cluster(sid)
xtreg PUN con1_inv rp con1_invrp wp con1_invwp, fe cluster(sid)

xtreg PUN sdiff rp wp, fe cluster(sid)
test _b[rp] = _b[wp]

xtreg PUN sdiffop if treatment == 31, cluster(sid) fe
eststo
xtreg PUN sdiffrp if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiffwp if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiffop rp sdiffrp wp sdiffwp, cluster(sid) fe
eststo

	test _b[sdiffop] = _b[sdiffrp]
	estadd scalar Foprp = r(F)
	estadd scalar p1 = r(p)
	test _b[sdiffrp] = _b[sdiffwp]
	estadd scalar Frpwp = r(F)
	estadd scalar p2 = r(p) 


esttab , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label tex

esttab using "OUTPUT/con1_4161.tex" , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label  booktabs replace 

eststo clear
*** - corresponding appendix table

xtreg PUN sdiffop porder if treatment == 31, cluster(sid) fe
eststo
xtreg PUN sdiffrp rporder if treatment == 41, cluster(sid) fe
eststo
xtreg PUN sdiffwp rporder if treatment == 61, cluster(sid) fe
eststo
xtreg PUN sdiffop rp sdiffrp wp sdiffwp porder rporder dualorder , cluster(sid) fe
eststo

esttab , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label tex

esttab using "OUTPUT/con1_4161_rob.tex" , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label  booktabs replace 


xtreg PUN sdiffop rp sdiffrp wp sdiffwp porder rporder dualorder , cluster(sid)


    /* estimating the impact of self-interest on punishment */
xtreg PUN sdiff rp sdiffrp if treatment == 41 | treatment == 61, cluster(sid) fe
xtreg PUN sdiff_bz sdiff_az rp sdiff_bztr41 sdiff_aztr41 if treatment == 41 | treatment == 61, fe cluster(sid)

graph twoway (scatter PUN sdiff if treatment == 41, m(Oh) title(RP)) (scatter PUN sdiff if treatment == 61, m(Dh) title(WP)) (lfit PUN sdiff if treatment == 41,title(Line RP)) (lfit PUN sdiff if treatment == 61, title(Line WP))
gr export "OUTPUT/scatter_lfit4161.eps", replace


eststo clear

xtreg PUN sdiff_bz sdiff_az if treatment == 31, fe cluster(sid)
eststo
xtreg PUN sdiff_bztr41 sdiff_aztr41 if treatment == 41, fe cluster(sid)
eststo
xtreg PUN sdiff_bztr61 sdiff_aztr61 if treatment == 61, fe cluster(sid)
eststo

xtreg PUN sdiff_bz sdiff_az rp sdiff_bztr41 sdiff_aztr41 wp sdiff_bztr61 sdiff_aztr61 , fe cluster(sid)
eststo
    /* test _b[sdiff_bztr31] = _b[sdiff_bztr41] */
    /* estadd scalar Foprp = r(F) */
    /* estadd scalar p1 = r(p) */
    /* test _b[sdiff_bztr41] = _b[sdiff_bztr61] */
    /* estadd scalar Frpwp = r(F) */
    /* estadd scalar p2 = r(p)  */



    /* xtreg PUN sdiff_bz sdiff_az i.treatment, fe cluster(sid) */

esttab , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label tex
esttab using "OUTPUT/sdiff_314161.tex" , b(3) se(3) ar2(3) star(* 0.1 ** 0.05 *** 0.01) label  replace

restore




** -- t-test individual Betas
preserve
  collapse b_con1_inv* b_sdiff_*, by(sid treatment)
  keep if treatment == 31
  ttest b_con1_inv31 = b_con1_inv41
  ttest b_con1_inv31 = b_con1_inv61
  ttest b_con1_inv41 = b_con1_inv61

  ttest b_sdiff_bz31 = b_sdiff_bz41
  ttest b_sdiff_bz31 = b_sdiff_bz61
  ttest b_sdiff_bz41 = b_sdiff_bz61

  ttest b_sdiff_az31 = b_sdiff_az41
  ttest b_sdiff_az31 = b_sdiff_az61
  ttest b_sdiff_az41 = b_sdiff_az61

keep if sid > 70300
  ttest b_con1_inv31 = b_con1_inv41
  ttest b_con1_inv31 = b_con1_inv61
  ttest b_con1_inv41 = b_con1_inv61

  ttest b_sdiff_bz31 = b_sdiff_bz41
  ttest b_sdiff_bz31 = b_sdiff_bz61
  ttest b_sdiff_bz41 = b_sdiff_bz61

  ttest b_sdiff_az31 = b_sdiff_az41
  ttest b_sdiff_az31 = b_sdiff_az61
  ttest b_sdiff_az41 = b_sdiff_az61

gr twoway scatter b_sdiff_az41 b_sdiff_az61, m(Oh)
gr export "OUTPUT/beta_4161_scatter.eps", replace

restore



** -- equality of contributions

preserve
  gen contrib31 = contrib if treatment == 31
  gen contrib41 = contrib if treatment == 41
  gen contrib61 = contrib if treatment == 61

  collapse contrib31 contrib41 contrib61 gameorder, by(sid)

  /* ttest contrib31 = contrib41 */
  /* ttest contrib31 = contrib61 */

  ttest contrib41 = contrib61
  su contrib41 contrib61

  ttest contrib41 = contrib61 if gameorder == 0
  ttest contrib41 = contrib61 if gameorder == 1

  /* keep if sid > 70300 */
  /*  */
  /*   ttest contrib41 = contrib61 if gameorder == 0 */
  /*   ttest contrib41 = contrib61 if gameorder == 1 */

restore





* ---------------------------------------------------------------
* ---------------------------- OLD ------------------------------
* ---------------------------------------------------------------
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
    * preserve
    *     drop if treatment == 51
    *     egen countID = group(sid)
    *     forvalue i = 1/44{
    *         lgraph PUN con1_inv treatment if countID == `i'
    *         gr export "`odir'con1_inv_`i'.eps", replace
    *     }
    * restore

** -- Individual punishment in 5 games (sdiff)
    * preserve
    *     drop if treatment == 51
    *     egen countID = group(sid)
    *     forvalue i = 1/44{
    *         lgraph PUN sdiff treatment if countID == `i'
    *         gr export "`odir'sdiff_`i'.eps", replace
    *     }
    * restore

* --- comparing individual contribution in P RP (Period 1 & 10) PWL (Period 1 & 10)
** -- ranksum for equality across orders

preserve
  collapse contrib, by(sid treatment)
  gen order = 0
  replace order = 1 if sid > 70200
  ranksum contrib if treatment == 61 , by(order)
  ranksum contrib if treatment == 41 , by(order)
restore


* ------------------------------------------------------------
* ------------------------------------------------------------
    * ----------- OLD --------------
* ------------------------------------------------------------
* ------------------------------------------------------------
* ------------------------------------------------------------
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

