/*
*------------------------------------------ 
*author: Alessandro V. M. Oliveira/ITA
*------------------------------------------
*v 1.0.1

*below are examples

discard
sysuse auto, clear

*only multiplication
vartransform Mpg = mpg
vartransform Price = price, transf(/1000)
vartransform Weight = weight, transf(/1000)
vartransform Rep78 = rep78
reg Price Mpg Weight Rep78

*zscore normalization
vartransform Mpg = mpg, z
vartransform Price = price, z transf(/1000)
vartransform Weight = weight, z transf(/1000)
vartransform Rep78 = rep78
reg Price Mpg Weight Rep78

*ln
vartransform Mpg = mpg, ln
vartransform Price = price, ln transf(/1000)
vartransform Weight = weight, ln transf(/1000)
vartransform Rep78 = rep78
reg Price Mpg Weight Rep78
*/

program vartransform, eclass byable(onecall) prop(svyb svyj svyr) 
syntax anything(id = "=exp" equalok) [if] [in] ///
		[, LN Z ORNOT TRANSF(string) SUF(string) AFTER EXP LADD(string)]

	gettoken newvar 0 : 0, parse("= ")  
	gettoken eqs 0 : 0, parse("= ")  
	gettoken existingvar 0 : 0, p(", ")

*checks
if "`newvar'"=="`existingvar'" {
di as err "new and existing variables are the same"
exit
}
if "`z'"!=""&"`ln'"!="" {
di as err "you must chose either z or ln"
exit
}

if "`ladd'"!="" local labeladd = " (`ladd')"

*original variables
local or = "or"
if "`ornot'"!="" local or == ""


*suffix of original variables (if requested)
if "`suf'"=="" local suf = "_or"

*certify that the new variable may be generated without error message
capture drop `newvar' 
if _rc==0 di as txt "dropped existing `newvar'"

capture drop `suf'`newvar' 
if _rc==0 di as txt "dropped existing `suf'`newvar'"

*characteristic of existing var
local type : type `existingvar'

*no transform
if "`ln'"=="" & "`z'"=="" {

*new var
gen `newvar' = `existingvar'`transf' `if' `in' 
format `newvar' `: format `existingvar''
label var `newvar' "`newvar'"
order `newvar', after(`existingvar')
note `newvar': gen `newvar' = `existingvar'`transf' `if' `in' 
local vars = "`existingvar' `newvar'"

if "`or'"!="" {
gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
format `suf'`newvar' `: format `existingvar''
label var `suf'`newvar' "`newvar'`labeladd'"
if "`after'"!="" order `suf'`newvar', after(`newvar')
note `suf'`newvar': gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
local vars = "`existingvar' `suf'`newvar' `newvar'"
}
}


*ln transform
if "`ln'"!="" {

*new var
gen `newvar' = ln(`existingvar'`transf') `if' `in' 
local length = max(5,length("`existingvar'"))
format `newvar' %`length'.2f
label var `newvar' "`newvar'"
order `newvar', after(`existingvar')
note `newvar': gen `newvar' = ln(`existingvar'`transf') `if' `in'
local vars = "`existingvar' `newvar'"

if "`or'"!="" {
gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
format `suf'`newvar' `: format `existingvar''
label var `suf'`newvar' "`newvar'`labeladd'"
if "`after'"!="" order `suf'`newvar', after(`newvar')
note `suf'`newvar': gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
local vars = "`existingvar' `suf'`newvar' `newvar'"
}
}


*zscore transform
if "`z'"!="" {

*new var
tempvar tv1
gen `tv1'=`existingvar'`transf'
su `tv1'
local rmean = `r(mean)'
local rsd = `r(sd)'
gen `newvar' = (`existingvar'`transf' - `rmean')/`r(sd)' `if' `in' 
local length = max(5,length("`existingvar'"))
format `newvar' %`length'.2f
label var `newvar' "`newvar'"
order `newvar', after(`existingvar')
note `newvar': gen `newvar' = (`existingvar'`transf' + `rmean')/`r(sd)' `if' `in'
local vars = "`existingvar' `newvar'"

if "`or'"!="" {
gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
format `suf'`newvar' `: format `existingvar''
label var `suf'`newvar' "`newvar'"
if "`after'"!="" order `suf'`newvar', after(`newvar')
note `suf'`newvar': gen `suf'`newvar' = `existingvar'`transf' `if' `in' 
local vars = "`existingvar' `suf'`newvar' `newvar'`labeladd'"
}
}



notes `vars'
su `vars'

end




