#!/bin/bash -l

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

#BRAZIL# MAPR="-R-67/-33/-37/-5"
MAPR="-R-71/-64/-30/-24"

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

label="1a2WESN"
labelb="5a10WEsN"
label2="50a100/50a100WSne"
label2b="50a100WSne"

for toplot in `seq 1 $max`
do

	if [ $toplot -le $hlast ]
	then
		plotOneHMap $toplot "$label" "   " tomo $what >> $ps
		[ $toplot -eq 1 -a $vlast -ge 1 ] && addprofiles 1-$vlast >> $ps
		if [ $synt -ge 1 ]; then
			plotOneHMap $toplot "$labelb" "-Y8.2"        stomo $what >> $ps
			g_shift -Y-8.2 >> $ps
		fi
		label="1a2wESN"
		labelb="1a2wEsN"
	fi

	if [ $toplot -le $vlast ]
	then
		plotprofile $toplot "$label2" "-Y-4.5" 350 tomo topo  moho tomoeve sta >> $ps
		label2="50a100/50a100:"EMPTY":WSne"
		ashift=4.5
		if [ $synt -ge 1 ]; then
			plotprofile $toplot "$label2b" "-Y-5.5" 350 stomo moho tomoeve sta >> $ps
			label2b="50a100/100a500:"EMPTY":WSne"
			ashift=10
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
