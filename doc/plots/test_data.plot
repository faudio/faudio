
set title 'Some math functions'
set xrange [-1:1]
set yrange [-1:1]
set zeroaxis
plot 'doc/plots/test_data.dat' using 2:1 with lines title 'Foo', \
    'doc/plots/test_data.dat' using 3:1 with impulses title 'Bar'