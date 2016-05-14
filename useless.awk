# filter configurations which are utterly useless 
# (i.e.: objectively betters ones exist)

{
    for(i=1;  i<=N; i++) {
	#if($2 >= a[i] && $4 >= b[i] && ($6+7) >= c[i] && $14 >= d[i] && $12 >= e[i] && $18 <= k[i]) {
	if($2 >= a[i] && ($6+7) >= c[i] && $14 >= d[i] && $12 >= e[i] && $18 <= k[i]) {
	    #print "useless:", n[i], ">" $0
	    next
	}
    }
    ++N;
    n[N] = NR
    a[N] = $2
    b[N] = $4
    c[N] = $6
    d[N] = $14
    e[N] = $12
    k[N] = $18
    print
}
