# http://daniel.haxx.se/blog/2010/12/14/add-latency-to-localhost/
alias throttlelocal="tc qdisc add dev lo root handle 1:0 netem delay 10msec"
alias throttlestop="tc qdisc del dev lo root"
