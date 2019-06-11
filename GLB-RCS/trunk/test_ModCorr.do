*check module correlation

*preliminary work
use "${gsdData}/KEN-KIHBS2015P-HHData.dta", clear
keep hhid xfood*
reshape long xfood, i(hhid) j(itemid)
save "${gsdTemp}/KEN-KIHBS2015P-Food.dta", replace

*parameters
local nm = 5

use "${gsdTemp}/KEN-KIHBS2015P-Food.dta", clear
*assign modules to items
gen r = runiform()
bysort itemid: gen rr = r[1]
gen imod = int(rr * (`nm'+1))
drop r rr
*calculate module consumption
forvalues i = 0/`nm' {
	quiet: gen cc`i' = xfood if imod == `i'
}
collapse (sum) cc*, by(hhid)
*get average correlation
corr cc*
matrix C = r(C)
mata
	C = st_matrix("C")
	C[1,1]=.
	st_local("cmean",strofreal(mean(abs(C[,1]))))
end

*get hh characteristics
merge 1:1 hhid using "${gsdData}/KEN-KIHBS2015P-HHData.dta", nogen assert(match) keepusing(mcat_* mcon_*)
*module application
egen hhmod = seq(), from(1) to(`nm')
forvalues i = 0/`nm' {
	quiet: gen xc`i' = cc`i' if (hhmod == `i') | (`i'==0)
}
egen rc = rowtotal(xc*)
gen lrc = log(rc)
fvunab mcat : i.mcat_*
*quiet: xi: vselect lrc mcon_* `mcat', fix(i.hhmod) forward aicc
*local model = r(predlist)
xi `mcat'
local model = "mcon_nchild mcon_elec_acc mcon_nadult mcon_assets mcon_padult mcon_ownhouse mcon_pchild mcon_nsenior mcon_rooms _Imcat_hhed_3 _Imcat_hhed_4 _Imcat_wall_2 _Imcat_wall_3 mcon_impsan _Imcat_roof_3 _Imcat_floo_3 _Imcat_hhed_2 _Imcat_hhh__2 _Imcat_wall_5 _Imcat_male_1 mcon_impwater _Imcat_depe_4 _Imcat_room_4 _Imcat_ageh_2 mcon_literacy _Imcat_pass_2"

*start estimation
* reshape to long format
ren xc0 fcore
reshape long xc, i(hhid) j(imod)
ren xc y
*remember 0 consumption
gen y_0 = y==0 if !missing(y)
*regularize for zero consumption
replace y = .01 if y<=0
replace y = ln(y)
*run MI
mi set wide
mi register imputed y y_0
mi register regular imod
mi register regular `model'
*step conditional step if almost all hh have module consumption >0
levelsof imod, local(lmod)
local add = "add(10)"
foreach imod of local lmod {
	quiet: count if (y_0==1) & (imod==`imod')
	if r(N)>0 {
		capture: mi impute monotone (logit, augment) y_0 (reg, cond(if y_0==0)) y = `model' if imod==`imod', `add'
		local r= _rc
		if `r'==430 {
			mi impute monotone (logit y_0 `: word 1 of `model'', augment) (reg y `model', cond(if y_0==0)) if imod==`imod', custom `add'
		}
		else if `r'>0 error `r'
	}
	else {
		mi impute reg y = `model' if imod==`imod', `add'
	}
	local add = "replace"
}
*transform into household-level dataset
keep hhid y y_0 _* imod fcore cc*
mi passive: gen z = exp(y)
*reshape back to the hh-level
mi passive: replace z = 0 if y_0==1
drop y_0 y _I*
mi register imputed z
mi update
mi rename z icons
mi reshape wide icons, i(hhid) j(imod)
mi ren fcore icons0
save "${gsdTemp}/test.dta", replace

*test results
use "${gsdTemp}/test.dta", clear
egen cc = rowtotal(cc*)
mi passive: egen ic = rowtotal(icons*)
corr cc0 cc1 cc2 cc3 cc4 cc5
matrix C=r(C)
*Module correlation correction
forvalues j=1/5 {
	egen micons`j' = rowmean(_?_icons`j')
}
forvalues i=1/10 {
*	corr icons0 _`i'_icons*
	drawnorm y0 y1 y2 y3 y4 y5, cov(C)
*	corr y*
	quiet: replace icons0 = icons0 + y0/10
	*add to MI estimates
	forvalues j=1/5 {
		quiet: replace _`i'_icons`j' = micons`j' + y`j'
	}
	drop y*
*	corr icons0 _`i'_icons*
}
mi passive: egen zc = rowtotal(icons*)

*cdfplots
preserve
ren cc z0
forvalues i=1/10 {
	ren _`i'_ic z1_`i'
	ren _`i'_zc z2_`i'
}
mi unset
keep hhid z0 z1_* z2_*
reshape long z1_ z2_, i(hhid) j(mi)
ren z?_ z?
reshape long z, i(hhid mi) j(i)
cdfplot z, by(i)
cdfplot z if mi==1, by(i)
*transform
bysort i: egen xsd = sd(z)
gen xxsd= xsd[1]
bysort i: egen xm= mean(z)
gen zz = (z-xm)/xsd*xxsd + xm
cdfplot zz, by(i)



restore
* calculate FGT for all possible poverty lines
_pctile cc, nq(100)
quiet forvalues i = 1/100 {
	local pline`i' = r(r`i')
}
gen t_fgt0 = .
gen t_fgt1 = .
gen y_fgt0 = .
gen y_fgt1 = .
mi register passive t_fgt0 t_fgt1 y_fgt0 y_fgt1
quiet forvalues i = 1/100 {
	*for reference
	gen r_fgt0_i`i' = cc < `pline`i''
	gen r_fgt1_i`i' = max(`pline`i'' - cc,0) / `pline`i''
	*for estimates
	mi passive: replace t_fgt0 = ic < `pline`i''
	mi passive: replace t_fgt1 = max(`pline`i'' - ic,0) / `pline`i''
	mi passive: replace y_fgt0 = zc < `pline`i''
	mi passive: replace y_fgt1 = max(`pline`i'' - zc,0) / `pline`i''
	*shortcut to avoid mi collapse
	egen x_fgt0_i`i' = rowmean(_*_t_fgt0)
	egen x_fgt1_i`i' = rowmean(_*_t_fgt1)
	egen z_fgt0_i`i' = rowmean(_*_y_fgt0)
	egen z_fgt1_i`i' = rowmean(_*_y_fgt1)
}
mi unset
keep r_fgt* x_fgt* z_fgt*
gen id = 1
collapse (mean) r_fgt* x_fgt* z_fgt*, by(id)
reshape long r_fgt0_i x_fgt0_i z_fgt0_i r_fgt1_i x_fgt1_i z_fgt1_i, i(id) j(p)
label var p "Percentile Poverty Line"
ren *_i *
drop id
order p r_fgt0 x_fgt0 z_fgt0 r_fgt1 x_fgt1 z_fgt1
*calculate absolute differences
forvalues i = 0/1 {
	label var r_fgt`i' "FGT`i' Reference"
	label var x_fgt`i' "FGT`i' RCS"
	label var z_fgt`i' "FGT`i' zRCS"
	gen qxfgt`i' = r_fgt`i'-x_fgt`i'
	gen dxfgt`i' = abs(qxfgt`i')
	gen qzfgt`i' = r_fgt`i'-z_fgt`i'
	gen dzfgt`i' = abs(qzfgt`i')
}
mean dxfgt* dzfgt*
graph twoway (line qxfgt0 p) (line qzfgt0 p) 

