program define TypeGraph
    syntax varlist(min=2) using/ [in] [, Model(string asis) Types(string asis) Labels(string asis) SPlit Options(string asis) TREATment(integer -100) fortyfive]
    tokenize `varlist'
    if "`split'" != ""{
        local sp = "_bz"
        }
    else{
        local sp = ""
        }
    local model = `model'`treatment'
    local numel = wordcount("`types'")
    * all elements
    gen t_`2'`sp'`model'_tmp = abs(t_`2'`sp'`model')
    bysort t_`2'`sp'`model'_tmp `2': egen mean_`2'`model'_pt = mean(`1')
    bysort `2': egen mean_`2'`model'_all = mean(`1')
    sort sid
    count if sid != sid[_n-1] & t_`2'`sp'`model' != .
    local N = r(N)
    label var mean_`2'`model'_all "Total Avg. N=`N'"
    *
    local graphlist = "mean_`2'`model'_all"
    forv i = 1/`numel'{
        local typ : word `i' of `types'
        local titel : word `i' of `labels'
        gen mean_`2'`model'_pt_`typ' = mean_`2'`model'_pt if t_`2'`sp'`model'_tmp == `typ'
        count if sid != sid[_n-1] & t_`2'`sp'`model'_tmp == `typ' 
        local N`typ' = r(N)
        local N`typ'p = round(`N`typ''/`N'*100)
        label var mean_`2'`model'_pt_`typ' "`titel' [N = `N`typ''] (`N`typ'p'%)"
        local graphlist = "`graphlist' mean_`2'`model'_pt_`typ'"
        }
    *
    if "`fortyfive'" != ""{
        gen `2'_tmp = `2'
        label var `2'_tmp "45 degree"
        local ffdegree = "(line `2'_tmp `2', col(gray) lpattern(dash))"
        local aspect = "aspect(1)"
    }
    else{
        local ffdegree = ""
        local aspect = ""
    }
    *
    sort `2'
    gr twoway (connected `graphlist' `2', `options' ) `ffdegree', graphregion(color(white)) bgcolor(white) plotregion(color(white)) legend(span) `aspect'
    *
    gr export "`using'", replace
    * clean up
    drop mean_*
    cap drop `2'_tmp
end
