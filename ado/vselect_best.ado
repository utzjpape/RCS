capture program drop vselect_best
program define vselect_best, rclass
	args m
	mata vselect_best("`m'","k")
	return local best = "`r(best`k')'"
end

*mata helper functions
cap mata mata drop vselect_best()
mata :
void vselect_best(string scalar m,string scalar ret) 
{
    X = st_matrix(m)
	k = .
	x = .
    for(i=1; i<=rows(X); i++){
        x = min((x,X[i,2]))
		if (x==X[i,2]) {
			k = X[i,1]
		}
    }
	st_local(ret,strofreal(k))
}
end
