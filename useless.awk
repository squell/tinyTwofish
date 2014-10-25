

# 2 4 5 7 9 11 13 15

{
    for(i=1;  i<=N; i++) {
	if($2 >= a[i] && $4 >= b[i] && $5 >= c[i] && $7 >= d[i] && $13 >= g[i] && $15 >= h[i]) {
	    print "useless:", n[i], ">" $0
	    next
	}
    }
    ++N;
    n[N] = NR
    a[N] = $2
    b[N] = $4
    c[N] = $5
    d[N] = $7
    g[N] = $13
    h[N] = $15
    print $0
}
