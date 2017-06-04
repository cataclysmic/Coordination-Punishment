program define RegStore, rclass byable(recall)
    * number if IDs
    syntax varlist(min=2) [if] [in] [, Model(string asis) ROBust CLUster(string asis) LABeling(string asis) SPlit spear MAXDeviation(integer 20) Pvalue(real 0.01) NOConst TREATment(integer -100) DIVided OPTions(string asis)]
    local id = _byindex()
    tokenize `varlist'
    local indep: list varlist - 1
    local words = wordcount("`varlist'")
    if "`cluster'" != ""{
        local cluster = "cluster(`cluster')"
    } 
    if `treatment' != -100{
        local treatmentstring = "& treatment == `treatment'"
        local model = "`model'`treatment'"
        }
    else{
        local treatmentstring = ""
        local treatment = ""
        }
* --- REGRESSION ---/*{{{*/
    if "`spear'" == ""{
        * generate variables/*{{{*/
        if _byindex() == 1{
            local constant = ""
            if "`noconst'" == ""{
                gen a_`2'`model' = .
                gen ap_`2'`model' = .
                label var a_`2'`model' "alpha of `labeling'"
                label var ap_`2'`model' "p-val of alpha of `labeling'"
                }
            gen ftest_`2'_`3'`model' = .
            gen ltail_`2'_`3'`model' = .
            gen rtail_`2'_`3'`model' = .
            gen rmse_`2'`model' = .
            gen r2_`2'`model' = .
            gen r2a_`2'`model' = .
            gen rss_`2'`model' = .
            gen aic_`2'`model' = .
            gen bic_`2'`model' = .
            * labels
            label var ftest_`2'_`3'`model' "f-test of `labeling'"
            label var ltail_`2'_`3'`model' "left tail of f-test of `labeling'"
            label var rtail_`2'_`3'`model' "right tail of f-test of `labeling'"
            label var rmse_`2'`model' "Root Mean Squared Error `labeling'"
            label var  r2_`2'`model' "R sq. `labeling'"
            label var r2a_`2'`model' "adj. R sq. `labeling'"
            label var rss_`2'`model' "Residual sum of square `labeling'"
            label var aic_`2'`model' "AIC `labeling'"
            label var bic_`2'`model' "BIC `labeling'"
            foreach el in `indep'{
                    gen b_`el'`model' = .
                    gen bp_`el'`model' = .
                    * labels
                    label var b_`el'`model' "beta `el' of `labeling'"
                    label var bp_`el'`model' "p-val of beta `el' of `labeling'"
                    }
                }
        * check whether dependent var was averaged, if not, create and store average Punishment/Contribution by individual
        cap confirm var avg_`1'`treatment', exact
        if _rc {
            gen avg_`1'`treatment' = .
            label var avg_`1'`treatment' "average Punishment individual assigned (`treatment')"
            }
        su `1' if `_byvars' == `id' `treatmentstring'
        replace avg_`1'`treatment' = r(mean) if `_byvars' == `id'
        * Regression /*{{{*/
        reg `varlist' if `_byvars' == `id' `treatmentstring' , `cluster' `robust' `noconst' `options'
        *}}}*/

        * collect + store output {{{*/
        * Information Criteria /*{{{*/
        replace rmse_`2'`model' = e(rmse) if `_byvars' == `id'
        replace r2_`2'`model' = e(r2) if `_byvars' == `id'
        replace r2a_`2'`model' = e(r2_a) if `_byvars' == `id'
        replace rss_`2'`model' = e(rss) if `_byvars' == `id'
        cap estat ic
        matrix _tmp = r(S)
        replace aic_`2'`model' = _tmp[1,5] if `_byvars' == `id'
        replace bic_`2'`model' = _tmp[1,6] if `_byvars' == `id'
        * free rider/perfect prediction
        replace r2_`2'`model' = 1 if e(r2) == . & `_byvars' == `id'
        replace r2a_`2'`model' = 1 if e(r2_a) == . & `_byvars' == `id'
        replace aic_`2'`model' = 0 if _tmp[1,5] == . & `_byvars' == `id'
        replace bic_`2'`model' = 0 if _tmp[1,6] == . & `_byvars' == `id'

        if "`noconst'" == ""{
            * alpha
            replace a_`2'`model' = _b[_cons] if `_byvars' == `id'
            test _cons = 0 
            replace ap_`2'`model' = r(p) if `_byvars' == `id'
            }
        * betas
        foreach el in `indep'{
            replace b_`el'`model' = _b[`el'] if `_byvars' == `id'
            test "`el'" = 0
            replace bp_`el'`model' = r(p) if `_byvars' == `id'
            * perfect correlation - compensation for missing pvalue
            replace bp_`el'`model' = 0 if r(p) == . & (_b[`el'] == 1 | _b[`el'] == -1 ) & `_byvars' == `id'
        }

 * beta_1 - beta_2 == 0 f-test onesided
         if "`split'" != ""{
             if _byindex() == 1{
             }
             test `2' - `3' = 0
             replace ftest_`2'_`3'`model' = r(p) if `_byvars' == `id'
             local sign_diff = sign(_b[`2'] - _b[`3'])
             * left tail <= 0
             replace ltail_`2'_`3'`model' = ttail(r(df_r),`sign_diff'*sqrt(r(F))) if `_byvars' == `id'
             * right tail => 0
             replace rtail_`2'_`3'`model' = 1-ttail(r(df_r),`sign_diff'*sqrt(r(F))) if `_byvars' == `id'
         }

* test if coeff. * MaxDev < 1 & sig., if so, retest with y = a + b*coeff
            if "`split'" != "" {
                forvalues el = 2/3{
                    * check for var if not exist generate
                    cap confirm variable a_``el''`model'_div, exact
                    if _rc{
                        gen a_``el''`model'_div = .
                        gen ap_``el''`model'_div = .
                        label var a_``el''`model'_div "alpha of elem. reg. if beta*MaxDiv < 1"
                        label var ap_``el''`model'_div "p-val of alpha of  elem. reg. if beta*MaxDiv < 1'"
                        gen b_``el''`model'_div = .
                        gen bp_``el''`model'_div = .
                        label var b_``el''`model'_div "beta of elem. reg. if beta*MaxDiv < 1"
                        label var bp_``el''`model'_div "p-val of beta of  elem. reg. if beta*MaxDiv < 1'"
                        }

                    reg `1' ``el'' if `_byvars' == `id' `treatmentstring' & ``el''_dummy == 1 , `robust'

                    * alpha
                    replace a_``el''`model'_div = _b[_cons] if `_byvars' == `id'
                    test _cons = 0 
                    replace ap_``el''`model'_div = r(p) if `_byvars' == `id'
                    * beta
                    replace b_``el''`model'_div = _b[``el''] if `_byvars' == `id'
                    test ``el'' = 0 
                    replace bp_``el''`model'_div = r(p) if `_byvars' == `id'
                    }
                }
        *}}}*/

    }

* --- SPEARMAN'S RHO ---
    if "`spear'" != ""{
        * generate variables
        if _byindex() == 1{
            foreach el in `indep'{
                gen rho_`el'`model' = .
                gen rhop_`el'`model' = .
                * labels
                label var rho_`el'`model' "Spearman Rho of `el' of `labeling'"
                label var rhop_`el'`model' "p-val of Spearman Rho of `el' of `labeling'"
                }
            }
        spearman `varlist' if `_byvars' == `id' `treatmentstring'
        * Rho matrix
        if `words' == 2{
            foreach el in `indep'{
                replace rho_`el'`model' = r(rho) if `_byvars' == `id'
                replace rhop_`el'`model' = r(p) if `_byvars' == `id'
                }
            }
        if `words' > 2 {
            mat spears = r(Rho)
            mat spearsP = r(P)
            local c = 1
            foreach el in `indep'{
                local c = `c' + 1
                replace rho_`el'`model' = spears[1,`c'] if `_byvars' == `id'
                replace rhop_`el'`model' = spearsP[1,`c'] if `_byvars' == `id'
                }
            }
        }
end
