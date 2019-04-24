#!/bin/bash -l

source /home/mbianchi/Work/tomocode/SHELL/tomo.bshm
[ ! -f "$tomobase/SHELL/helpme.sh" ] && echo "No helpme.sh found." && exit
source $tomobase/SHELL/helpme.sh

val="1"
ps=fig.ps
rm "$ps"

MAPR="-R-75/-35/-39/0"

gmtset PAPER_MEDIA A4
g_shift -X2.5c -Y2.5c > $ps
shift="-X0"
what="borders sta label"

if [ -f "s.h.$val.gmt" ]; then
	echo "Plotting s.v.$val.gmt"
	plotOneHMap $val "1a15" "$shift"        stomo $what >> $ps
	shift="-X11"
fi

if [ -f "r.h.$val.gmt" ]; then
	echo "Plotting r.v.$val.gmt"
	plotOneHMap $val "1a15" "$shift"        tomo $what >> $ps
	shift="-X11"
fi

if [ -f "r.v.$val.gmt" ]; then
	echo "Plotting s.v.$val.gmt"
	plotprofile $val "100a100WSne" "$shift" 900 tomo >> $ps
	shift="-X11"
fi

if [ -f "s.v.$val.gmt" ]; then
	echo "Plotting s.v.$val.gmt"
	plotprofile $val "100a100WSne" "$shift" 900 stomo >> $ps
	shift="-X11"
fi

# End gmt file
g_end >> "$ps"

# Clean up
rm topo.grd ilutopo.grd ilutomo.grd colortopo.cpt

gv $ps

