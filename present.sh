#! /bin/sh

# post-process the results of analysis.sh to be more usable

sort -nk2 | awk -f useless.awk | awk '{print int($6/128), $0}' | sort -n -k1,1 -k3,3 | sort --stable -k19,19 | cut -d' ' -f2- | sed 's/SP:[^\t]*\t//g'
