use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_avg.dta",clear
su ref if simulation==1 ,d
hist(ref)  if simulation==1, title("SSD")
fastgini(ref)

gen ly=ln(ref)
hist(ly)  if simulation==1, title("SSD")

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_avg.dta",clear
keep if simulation==1
collapse (mean) ref [aw=weight], by(simulation)
su ref 
local hh_mean=r(mean)

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_avg.dta",clear
keep if simulation==1
collapse (mean) ref weight, by(simulation cluster)
su ref [aw=weight] ,d
local cluster_mean=r(mean)



use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_avg.dta",clear
keep if simulation==1
su ref [aw=weight] ,d
local hh_mean=r(mean)
tempfile hh_level
hist(ref) if ref<=1000 , title("HH-level") xline(`hh_mean' `cluster_mean') saving(`hh_level', replace) 

use "C:\Users\wb252129\Box Sync\RapidCons\RCS\trunk\Output\SSD\d3m4\Temp\simd_avg.dta",clear
keep if simulation==1
collapse (mean) ref weight, by(cluster simulation)
tempfile cluster_level
hist(ref) , title("Cluster-level") xline(`nat_mean') saving(`cluster_level', replace)

graph combine "`hh_level'" "`cluster_level'", title("SSD") col(1) xcommon ycommon 
gr combine 
