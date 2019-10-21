* parameter:
*   hhid(varlist): list of variables that uniquely identify households
*   hhsize(varname): variable with household size
*   strata(varlist): variables identifying strata
*   weight(varname): household weight
*   hhmod(varname): assigned optional module to household
*   cluster(varname): cluster variable, like enumeration area
*   xfcons(name): stub of variable name for module food consumption (e.g. for stub 'xfcons', variables must be called xfcons0 for core, and xfcons1 etc for optional modules)
*   xnfcons(name): stub of variable name for module non-food consumption (e.g. for stub `xnfcons', variables must be called xnfcons0 for core, and xnfcons1 etc for optional modules)
*   mcon(name): stub for continuous co-variates
*   mcat(name): stub for categorical co-variates (will be used in model with i. prefix)
*   rcscons(name): variable name for the output variable containing imputed consumption
*   [nmi(integer 10)]: number of multiple imputations
cap: program drop rcs_estimate
program define rcs_estimate
	syntax , hhid(varlist) hhsize(varname) strata(varlist) weight(varname) hhmod(varname) cluster(varname) xfcons(name) xnfcons(name) mcon(string) mcat(string) rcscons(name) [nmi(integer 10)]
	*save dataset
	tempfile ft
	quiet: save "`ft'", replace
	*prepare variable lists
	unab lmcon : `mcon'
	fvunab lmcat : i.`mcat'
	*create class for model selection and estimation
	capture classutil drop .re
	.re = .RCS_estimator.new
	.re.prepare , hhid("`hhid'") weight("`weight'") hhmod("`hhmod'") cluster("`cluster'") xfcons("`xfcons'") xnfcons("`xnfcons'") nmi(`nmi')
	.re.select_model `hhsize' `strata' `lmcon' `lmcat', method("forward aicc")
	*report regression results
	tempname tcons
	egen `tcons' = rowtotal(`xfcons'* `xnfcons'*)
	quiet: replace `tcons' = log(`tcons')
	quiet: replace `tcons' = log(.01) if missing(`tcons')
	reg `tcons' `.re.logmodel' i.`hhmod' [pweight=`weight']
	drop `tcons'
	di "Running imputation ..."
	.re.est_mi_2cel
	quiet: gen xcons = .
	mi register passive `rcscons'
	quiet: mi passive: replace `rcscons' = 0
	foreach v of varlist `xfcons'? `xnfcons'? {
		quiet: mi passive: replace `rcscons' = `rcscons' + `v'
	}
	*cleaning
	mi register imputed `rcscons'
	mi update
	keep `hhid' *`rcscon' _mi*
	quiet: merge 1:1 `hhid' using "`ft'", assert(match) nogen
	order *`rcscons' _mi*, last
end
