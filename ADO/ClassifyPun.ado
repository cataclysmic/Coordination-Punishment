* usage: ClassifyPun ID independent, model(blabla)  > first indept MUST be alpha name
* e.g. ClassifyPun ID con1
cap program drop ClassifyPun
program define ClassifyPun, rclass
    syntax varlist(min=2) [if] [in] [, Model(string asis) Pvalue(real 0.01) LABeling(string asis) NONNull SPear TREATment(integer -100)]
    local words = wordcount("`varlist'")
    if `treatment' != -100{
        local model = "`model'`treatment'"
        }
    tokenize `varlist'
    * generate classification var
    if "`nonnull'" == "" {
        local frn = ""
        }
    else {
        local frn = "b_"
        }
    if _byindex() == 1{
        if "`spear'" == "" {
            gen t_`frn'`2'`model' = 100
            label var t_`frn'`2'`model' "Classification of `labeling'"
        }
        }
    * single value (non-split)/*{{{*/
    if `words' == 2 {
        *** Types - OLS *{{{*/

    *   Prefixes and Names for type variables {{{*/
        * prefixes and type names for OLS variables 
        local b = "b_`2'`model'"
        local bp = "bp_`2'`model'"
        local a = "a_`2'`model'"
        local ap = "ap_`2'`model'"
    *}}}*/
        * Others
        * --- default = 100 ---
        if "`spear'" == "" {
            * NoPun     (b, a > 0 ~ . but insig.)
            if "`nonnull'" == ""{
                replace t_`frn'`2'`model' = 0 if avg_`1'`treatment' == 0
                }
            else {
                replace t_`frn'`2'`model' = 0 if `bp' > `pvalue' & `ap' > `pvalue'
                }
            * Pun
            if `treatment' == 51{
                replace t_`frn'`2'`model' = -1 if `b' > 0 & `bp' <= `pvalue'
                replace t_`frn'`2'`model' = -1 if `b' > 0 & `bp' <= `pvalue'
            }
            else{
                replace t_`frn'`2'`model' = -1 if `b' > 0 & `bp' <= `pvalue'
                replace t_`frn'`2'`model' = -1 if `b' > 0 & `bp' <= `pvalue'
            }
            * AntiSocial !!!! b < 0 restriction or positive alpha
            replace t_`frn'`2'`model' = 2 if `a' > 0 & `ap' <= `pvalue' & `bp' > `pvalue'
            replace t_`frn'`2'`model' = 2 if `b' < 0 & `bp' <= `pvalue' 
        }
        *}}}*/

        *** Types - Spearman Rho *{{{*/
        if "`spear'" != ""{
        *   Prefixes and Names for type variables {{{*/
            * prefixes and type names for Spearman Rhos 
            local b = "rho_`2'`model'"
            local bp = "rhop_`2'`model'"
            if _byindex() == 1{
                gen t_spear_`2'`model' = 100
                label var t_spear_`2'`model' "Classification of `labeling'"
                }
            *}}}*/
            * Others
            * --- default = 100 ---
            * NoPun     (b, a > 0 ~ . but insig.)
            replace t_spear_`frn'`2'`model' = 0 if avg_`1'`treatment' == 0
            * Pun
            replace t_spear_`frn'`2'`model' = -1 if `b' > 0 & `bp' <= `pvalue'
            * AntiSocial !!!! b < 0 restriction or positive alpha
            replace t_spear_`frn'`2'`model' = 2 if `b' < 0 & `bp' <= `pvalue' 
            }
    * end word == 1
        }
        *}}}*/
    *}}}*/

    * two values (split)/*{{{*/
    if `words' == 3 {
        *** Types OLS *{{{*/

        *   Prefixes and Names for type variables {{{*/
        * prefixes and type names for OLS variables 
        local bbz = "b_`2'`model'"
        local bbzp = "bp_`2'`model'"
        local baz = "b_`3'`model'"
        local bazp = "bp_`3'`model'"
        local a = "a_`2'`model'"
        local ap = "ap_`2'`model'"
        *}}}*/

        * Others
        * --- default = 100 ---
        * NoPun     (b, a > 0 ~ . but insig.)  __
        if "`nonnull'" == ""{
            replace t_`frn'`2'`model' = 0 if avg_`1'`treatment' == 0
            }
        else {
            replace t_`frn'`2'`model'  =  0     if `bbzp' > `pvalue' & `bazp' > `pvalue'  & `ap' > `pvalue'
            }
        * PROSOCIAL------------
        * 14 - SPunV \/
        replace t_`frn'`2'`model'  = -14     if `bbz'  < 0 & `bbzp' <= `pvalue'  &  `baz' > 0 & `bazp' <= `pvalue' & `bbz'_div < 0 & `bbzp'_div <= `pvalue' & `baz'_div > 0 & `bazp'_div <= `pvalue' 
        * 13 - UPun  (ftest shows no difference in both)         //
        replace t_`frn'`2'`model'  = -13     if `bbz' > 0 & `bbzp' <= `pvalue' & `baz' > 0 & `bazp' <= `pvalue' & ftest_`2'_`3'`model' > `pvalue' 
        * 12 - SPun bbz = steiler , baz = flacher-/
        replace t_`frn'`2'`model'  = -12     if `bbz' > 0 & `bbzp' <= `pvalue' & `baz' > 0 & `bazp' <= `pvalue' & `bbz' > `baz' & ftest_`2'_`3'`model' <= `pvalue'
        replace t_`frn'`2'`model'  = -12     if `bbz' > 0 & `bbzp' <= `pvalue' & `bazp' > `pvalue' & `a' > 0 & `ap' <= `pvalue' & `bbz' > `baz' & ftest_`2'_`3'`model' < `pvalue'
        * 11 - SPun bbz = flacher, baz = steiler -/
        * tail smaller means further from 0
        replace t_`frn'`2'`model'  = -11     if `bbz' > 0 & `bbzp' <= `pvalue' & `baz' > 0 & `bazp' <= `pvalue' & `bbz' < `baz' & ftest_`2'_`3'`model' <= `pvalue' 
        * 10 - SPun bbz = 0, baz = sig. Anstieg  _/
        replace t_`frn'`2'`model'  = -10     if `bbzp' > `pvalue' & `baz'  > 0         & `bazp' <= `pvalue' 
        * 10 - bbz < 0 & baz > 0, recheck single estimation bbz_div == 0 & baz_div > 0 (necessary as OLS slopes down bbz == 0 if baz > 0 for better fit)
        replace t_`frn'`2'`model'  = -10     if `bbzp'_div > `pvalue' & `baz'_div > 0 & `bazp'_div <= `pvalue' 
        * ANTISOCIAL-----------
        * 24 - SUPunV /\
        replace t_`frn'`2'`model'  = 24     if `bbz'  > 0 & `bbzp' <= `pvalue'  &  `baz' < 0 & `bazp' <= `pvalue' & `baz'_div < 0 & `bazp'_div <= `pvalue' & `bbz'_div > 0 & `bbzp'_div <= `pvalue' 
        * 23 - UAPun           //
        replace t_`frn'`2'`model'  = 23     if `bbz' < 0 & `bbzp' <= `pvalue' & `baz' < 0 & `bazp' <= `pvalue' & ftest_`2'_`3'`model' > `pvalue' 
        * 22 - SAPun bbz = flacher, baz = steiler -\
        replace t_`frn'`2'`model'  = 22     if `bbz' < 0 & `bbzp' <= `pvalue' & `baz' < 0 & `bazp' <= `pvalue' &  `bbz' > `baz' & ftest_`2'_`3'`model' <= `pvalue' 
        replace t_`frn'`2'`model'  = 22     if `bbzp' > `pvalue' & `baz' < 0 & `bazp' <= `pvalue' & `a' > 0 & `ap' <= `pvalue' & `bbz' < `baz' & ftest_`2'_`3'`model' <= `pvalue' 
        * 21 - SAPun bbz = steiler , baz = flacher \-
        * tail smaller means further from 0
        replace t_`frn'`2'`model'  = 21     if `bbz' < 0 & `bbzp' <= `pvalue' & `baz' < 0 & `bazp' <= `pvalue' & `bbz' < `baz' & ftest_`2'_`3'`model' <= `pvalue' 
        * 20 - SAPun baz = 0, bbz = sig. Abfall  \_
        replace t_`frn'`2'`model'  = 20     if `bazp' > `pvalue' & `bbz' < 0 & `bbzp' <= `pvalue' 
        * 20 - bbz < 0 & baz > 0, recheck single estimation bbz_div == 0 & baz_div > 0 (necessary as OLS slopes down bbz == 0 if baz > 0 for better fit)
        replace t_`frn'`2'`model'  = 20     if `bazp'_div > `pvalue' & `bbz'_div < 0 & `bbzp'_div <= `pvalue' 
        ** recheck left side insig bbz BUT sig pos alpha 1 ( using indepent regression as in 10)
        *replace t_`frn'`2'`model'  = 21     if `baz' < 0 & `bazp' <= `pvalue' & `bbzp'_div > `pvalue' & `a'_div > 0 & `ap'_div <= `pvalue' & t_`frn'`2'`model'  == 100
*}}}*/

        *** Types Spearman*{{{*/

        if "`spear'" != ""{
            *   Prefixes and Names for type variables {{{*/
            * prefixes and type names for OLS variables 
            local bbz = "rho_`2'`model'"
            local bbzp = "rhop_`2'`model'"
            local baz = "rho_`3'`model'"
            local bazp = "rhop_`3'`model'"
            if _byindex() == 1{
                gen t_spear_`2'`model' = 100
                label var t_spear_`2'`model' "Classification of `labeling'"
                }
            *}}}*/

            * Others
            * --- default = 100 ---
            * NoPun     (b, a > 0 ~ . but insig.)  __
            replace t_spear_`frn'`2'`model'  =  0     if `bbzp' > `pvalue' & `bazp' > `pvalue'
            * PROSOCIAL------------
            * 10 - SPun bbz = 0, baz = sig. Anstieg  _/
            replace t_spear_`frn'`2'`model'  = -10     if `bbzp' > `pvalue' & `baz' > 0 & `bazp' <= `pvalue'
            * ANTISOCIAL-----------
            * 20 - SAPun baz = 0, bbz = sig. Abfall  \_
            replace t_spear_`frn'`2'`model'  = 20     if `bazp' > `pvalue' & `bbz' < 0 & `bbzp' <= `pvalue'
            }
        *}}}*/
        */*}}}*/
    * end word == 2
        }
*}}}*/
end
*}}}*/

