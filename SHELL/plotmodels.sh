#!/bin/bash 

source ~/bin/BSHM/tomo.bshm 

R="-R0/10/-500/0"
ps=models.ps

# [ ! -f ref_start.bln ]  && echo "Are you in the right folder?" && exit

psbasemap $R -JX8/15 -Gwhite -Xc -Yc -P -K > $ps

# bln2gmt.sh ref_start.bln  | psxy -m -R -J -O -K -W5p,200 >> $ps

#color=100
#for mdfile in $(ls -1 ref?.bln)
#do
#    echo "Plotting $mdfile / $color"
#    bln2gmt.sh $mdfile | psxy -m -R -J -O -K -W1p,$color/0/0 >> $ps
#    color=$(( color + 30 ))
#done

awk 'NR > 1 {print $2,-$1}' ../INI_PARAM/ref_start.dat | psxy -m -R -J -O -K -W1p,$color/0/0 >> $ps
awk 'NR > 1 {print $3,-$1}' ../INI_PARAM/ref_start.dat | psxy -m -R -J -O -K -W1p,$color/0/0 >> $ps
                
color=100
for md in $(ls -1 ref?.dat)
do
	echo "Plotting $md / $color"
	awk 'NR > 1 {print $2,-$1}' $md | psxy -m -R -J -O -K -W1p,$color/0/0 >> $ps
        awk 'NR > 1 {print $3,-$1}' $md | psxy -m -R -J -O -K -W1p,$color/0/0 >> $ps	
	color=$(( color + 30 ))
done

#area=$(getfigarea)
#model=$(getfigmodel)
#awk 'NR>1{print $2,-$1}' $tomobase/DATA/$area/$model/INI_PARAM/refmod.dat | psxy  -R -J -O -K -m -W1,0/0/255 >> $ps
#awk 'NR>1{print $3,-$1}' $tomobase/DATA/$area/$model/INI_PARAM/refmod.dat | psxy  -R -J -O -K -m -W1,0/0/255 >> $ps

psxy -R -J -O -B1a2:"Velocity (km/s)":/20a100:"Depth (km)":WsNe /dev/null >> $ps
gv $ps
