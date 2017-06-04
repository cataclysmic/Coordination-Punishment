* --- HEADER
set more off
mem 1000M


* --- Data preparation
/* load data */
use "DTA/7_wave.dta", clear
/* drop real triple in P-game */
drop if treatment == 31 & block == 11
/* classification benchmark p-val = 0.01 */
local pcomp = 0.01
/* create iterable id */
egen ID = group(sid)
/* invert contribution */
gen con1_inv = 20 - con1 

* - Create types

** -- Run individual regressions on strategy method data
/* individual regression in P-game using robust SEs */
bysort ID: RegStore PUN con1_inv,  treatment(31) robust labeling(20 - con1 P-Game)
/* individual regression in RP-game using robust SEs */
bysort ID: RegStore PUN con1_inv,  treatment(41) robust labeling(20 - con1 RP-Game)
/* individual regression in WP-game using robust SEs */
bysort ID: RegStore PUN con1_inv,  treatment(61) robust labeling(20 - con1 WP-Game)
/* individual regression in C-game using robust SEs */
bysort ID: RegStore condcon cell,  treatment(51) robust labeling(20 - con1 C-Game)

** -- Classification of Punishment behavior
/* individual regression in P-game using robust SEs */
ClassifyPun PUN con1_inv, labeling(20 - con1 / exogenous obs)  treat(31)
/* individual regression in RP-game using robust SEs */
ClassifyPun PUN con1_inv, labeling(20 - con1 / exogenous obs)  treat(41)
/* individual regression in WP-game using robust SEs */
ClassifyPun PUN con1_inv, labeling(20 - con1 / exogenous obs)  treat(61)
/* individual regression in C-game using robust SEs */
    ** TODO /* CLASSIFICATION NEEDS MANUAL CLASSIFICATION OF TRIANGULAR TYPES */

* --- Label Data
** -- label types
*** - create value label definition
cap label define t_class 0 "NPun"
cap label define t_class -1 "Pun" , add
cap label define t_class 2 "APun" , add
cap label define t_class 100 "NCL", add
*** - assign labels
cap label var t_con1_inv31 "linear P-game"
cap label var t_con1_inv41 "linear RP-game"
cap label var t_con1_inv61 "linear WP-game"
cap label value t_con1_inv31 t_class
cap label value t_con1_inv41 t_class
cap label value t_con1_inv61 t_class

** -- label treatments
*** - create value label definition
cap label define t_treat 51 "C-Game"
cap label define t_treat 31 "P-Game" , add
cap label define t_treat 21 "RP-Game" , add
cap label define t_treat 41 "SM-RP-Game" , add
cap label define t_treat 61 "SM-WP-Game" , add
cap label define t_treat 71 "WP-Game" , add
*** - assign labels
cap label value treatment t_treat


** -- label treatment orders in lab
gen laborder = 1
replace laborder = 2 if sid > 70200 & sid < 70300
replace laborder = 3 if sid > 70300 & sid < 70500
replace laborder = 4 if sid > 70500 & sid < 70700
replace laborder = 5 if sid > 70700 & sid < 70900
replace laborder = 6 if sid > 70900 & sid < 71100
*** - create value label definition
cap label define t_labo 1 "C-P-RP-WP"
cap label define t_labo 2 "C-P-WP-RP", add
cap label define t_labo 3 "RP-WP-P-C", add
cap label define t_labo 4 "RP-WP-C-P", add
cap label define t_labo 5 "WP-RP-P-C", add
cap label define t_labo 6 "WP-RP-C-P", add
*** - assign labels
cap label value laborder t_labo

* --- Save data
save "DTA/7_wave_typed.dta", replace


* --- Quick data view
/* viewing distribution on individual level !BEWARE value labels get deleted! */
collapse t_con1_inv31 t_con1_inv41 t_con1_inv61, by(sid)

** -- label types
*** - create value label definition
cap label define t_class 0 "NPun"
cap label define t_class -1 "Pun" , add
cap label define t_class 2 "IPun" , add
cap label define t_class 100 "NCL", add
*** - assign labels
cap label var t_con1_inv31 "linear P-game"
cap label var t_con1_inv41 "linear RP-game"
cap label var t_con1_inv61 "linear WP-game"
cap label value t_con1_inv31 t_class
cap label value t_con1_inv41 t_class
cap label value t_con1_inv61 t_class

** -- Tabulate
tab laborder
tab t_con1_inv31
tab t_con1_inv41
tab t_con1_inv61

tab t_con1_inv31 t_con1_inv41
tab t_con1_inv31 t_con1_inv61
tab t_con1_inv41 t_con1_inv61
 
