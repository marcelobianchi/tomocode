#!/bin/bash

## BSHM CHECK
[ -z "$tomobase" ] && echo "tomobase VAR NOT DEFINED." && exit
[ ! -f "$tomobase/SHELL/tomo.bshm" ] && echo "No tomo.bshm module found." && exit
source "$tomobase/SHELL/tomo.bshm" || exit

## LOAD
[ ! -f "$tomobase/SHELL/helpme.sh" ] && echo "No helpme.sh support file found into '$tomobase'." && exit
source $tomobase/SHELL/helpme.sh || exit

hfirst=1
vfirst=1

hlast=`ls -1 r.h.*.gmt | wc -l`
vlast=`ls -1 r.v.*.gmt | wc -l`

clon=$(getareacenter | awk '{print $1}')
clat=$(getareacenter | awk '{print $2}')

#MAPR="-R-70/-37/-32/-6"
# MAPR="-R-71/-64/-30/-24"
MAPR=$(gethorcoords $clon $clat R)

ps="results.ps"

rm -f "$ps"

prepareTopo
preparetomoilu "r.h.1.gmt" ilutomo.grd

# Set paper size
w=`echo "" | awk -v n="$hlast" '{print 470+(370*n)+100}'`
gmtset PAPER_MEDIA Custom_1100x$w

what="sta borders label tomoeve"

g_shift -X2.5c -Y20 > $ps


[ $hlast -ge $vlast ] && max=$hlast || max=$vlast
[ `ls -1 s.*.gmt 2>/dev/null | wc -l` -ge 1 ] && synt=1 || synt=0

echo "Hor $hfirst $hlast"
echo "Ver $vfirst $vlast"
echo "Max: $max"
echo ""

X_mark_space=$(gethorcoords $clon $clat | awk '{printf "%d",(($2-$1)/3)}')
label="1a${X_mark_space}WESN"
labelb="1a${X_mark_space}WEsN"

label2="50a250/50a150WSe"
label2b="50a250/50a150WSe"

maxdepth=450

for toplot in `seq 1 $max`
do

	if [ $toplot -le $hlast ]
	then
		plotOneHMap $toplot "$label" "   " tomo $what >> $ps
		[ $toplot -eq 1 ] && getareacenter | psxy -R -J -O -K -Sc0.2 -Gblack >> $ps
		[ $toplot -eq 1 -a $vlast -ge 1 ] && addprofiles 1-$vlast >> $ps
		if [ $synt -ge 1 ]; then
			plotOneHMap $toplot "$labelb" "-Y8.3"        stomo $what >> $ps
			[ $toplot -eq 1 ] && getareacenter | psxy -R -J -O -K -Sc0.2 -Gblack >> $ps
			g_shift -Y-8.3 >> $ps
		fi
		label="1a${X_mark_space}wESN"
		labelb="1a${X_mark_space}wEsN"
	fi

	if [ $toplot -le $vlast ]
	then
		ashift=5.0
		plotprofile $toplot "$label2" "-Y-$ashift" $maxdepth tomo topo  moho tomoeve sta >> $ps
		label2="50a250/50a150:"EMPTY":WSe"
		if [ $synt -ge 1 ]; then
			plotprofile $toplot "$label2b" "-Y-5.5" $maxdepth stomo topo moho tomoeve >> $ps
			label2b="50a250/50a150:"EMPTY":WSe"
			ashift=10.5
		fi
		g_shift -Y$ashift >> $ps
	fi

	g_shift -X8.5 >> $ps
done

# End gmt file
g_end >> "$ps"

# Clean up
rm -f topo.grd ilutopo.grd ilutomo.grd colortopo.cpt

gv $ps
