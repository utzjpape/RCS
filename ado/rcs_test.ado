* parameter:
*   hhsize(varname): variable with household size
*   weight(varname): household weight
*   rcscons(varname): variable containing imputed consumption
*   rcscons(varname): variable containing full consumption
cap: program drop rcs_test
program define rcs_test
	syntax , hhsize(varname) weight(varname) rcscons(varname) fullcons(varname)
	
	keep `weight' `hhsize' *`rcscons' `fullcons' _mi*
	* calculate FGT for all possible poverty lines
	_pctile `fullcons' [pweight=`weight'*`hhsize'], nq(100)
	quiet forvalues i = 1/100 {
		local pline`i' = r(r`i')
	}
	quiet: gen t_fgt0 = .
	quiet: gen t_fgt1 = .
	quiet: mi register passive t_fgt0 t_fgt1
	quiet forvalues i = 1/100 {
		*for reference
		gen r_fgt0_i`i' = `fullcons' < `pline`i''
		gen r_fgt1_i`i' = max(`pline`i'' - `fullcons',0) / `pline`i''
		*for estimates
		mi passive: replace t_fgt0 = xcons < `pline`i''
		mi passive: replace t_fgt1 = max(`pline`i'' - `rcscons',0) / `pline`i''
		*shortcut to avoid mi collapse
		egen x_fgt0_i`i' = rowmean(_*_t_fgt0)
		egen x_fgt1_i`i' = rowmean(_*_t_fgt1)
	}
	quiet: mi unset
	keep r_fgt* x_fgt* `weight' `hhsize'
	tempname id
	gen `id' = 1
	collapse (mean) r_fgt* x_fgt* [pweight=`weight'*`hhsize'], by(`id')
	quiet: reshape long r_fgt0_i x_fgt0_i r_fgt1_i x_fgt1_i, i(`id') j(p)
	label var p "Percentile Poverty Line"
	ren *_i *
	drop `id'
	order p r_fgt0 x_fgt0 r_fgt1 x_fgt1
	*calculate absolute differences
	forvalues i = 0/1 {
		label var r_fgt`i' "FGT`i' Reference"
		label var x_fgt`i' "FGT`i' RCS"
		quiet: gen zfgt`i' = r_fgt`i'-x_fgt`i'
		quiet: gen dfgt`i' = abs(zfgt`i')
		label var dfgt`i' "Absolute difference for FGT`i'"
	}
	mean dfgt*
	graph twoway (line zfgt0 p) (line zfgt1 p) 
end
