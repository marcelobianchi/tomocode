# Defaults

[ -z "$GMTDATA" ] && echo "helpme2.sh: Cannot find GMTDATA folder" && return 1

#export topoGrid="$GMTDATA/SA.srtm"
export topoGrid="$GMTDATA/etopo2.grd"
export folder=""

rm -f .gmt*

anotFontSize=10

## Both
export mapP=Q$(getareacenter|awk '{print $1}')"/"$(getareacenter|awk '{print $2}')"/"
export mapW=7

gmtset BASEMAP_TYPE plain
gmtset LABEL_FONT_SIZE 10
gmtset ANNOT_FONT_SIZE_PRIMARY 10

## Sections
export sectionH=3
export topH=0.5
export evcol=50
export stcol=25
export vocol=50

## GENERAL Parameters
lineWidth=0.5p
markSize=0.1

## Marks
export pmarks=100

## Maps
export MAPR="-R-71/-64/-29/-24"
export eveSize=0.05

export TOMOSET=GENERAL

## CPT
export cptz="-10/10/1"

## Global stuff
gmtset BASEMAP_TYPE plain
export LOG=`tty`

##################################################################################################################
# Important -- Implement this methods to customize the package to your data input
##################################################################################################################

##################################################################################################################
### Show Config ##################################################################################################
##################################################################################################################
source $tomobase/SHELL/helpersgeo.sh

function config() {
 message "Map Area: $MAPR"
 message "Map Width: $mapW"
 message "Section Height: $sectionH"
 message "Top Section Height: $topH"
 message "Events collection: $evcol"
 message "Stations collection: $stcol"
 message "Volcanos collection: $vocol"
 message "Profile X anotation: $Xi"
 message "Profile Y anotation: $Yi"
 message ""
}

##################################################################################################################
### Map     ######################################################################################################
##################################################################################################################
function plotOneHMap() {
 id=$1;shift
 anot="$1";shift
 shift="$1"; shift

 if [ -f  r.h.ref.tab ]
 then
  depth=`awk -v id="$id" '$1 == id {printf "%.0f\n",$2}' r.h.ref.tab`
  refvel=`awk -v id="$id" '$1 == id {printf "%.2f\n",$3}' r.h.ref.tab`
  [ -z "$refvel" -o "$refvel" == "0.00" ] && refvel=""
 else
  depth="-"
  refvel="-"
 fi

 message "Map $id"

 tomogrd="${folder}r.h.$id.gmt"
 stomogrd="${folder}s.h.$id.gmt"
 [[ $* =~ " tomo" && ! -f $tomogrd ]] && error "Cannot find tomo grid" && return 1
 [[ $* =~ "stomo" && ! -f $stomogrd ]] &&  error "Cannot find stomo grid" && return 1
 [[ $* =~ "topo" && ! -f "topo.grd" ]] && error "Cannot find topo grd (use prepareTopo)" && return 1
 [[ $* =~ "generic" && ! -f "generic.cpt" ]] && error "Generic mode needs a generic.cpt color table" && return 1

 getTomocpt > color.cpt
 getTopocpt > topo.cpt

 size="-J$mapP$mapW"
 [ -s "$ps" ] && open="-O -K" || open="-K"
 psbasemap $MAPR $size $open $shift -B$anot

 x=`echo $MAPR | awk -F"/" '{print $1}' | sed -e 's/-R//g'`
 y=`echo $MAPR | awk -F"/" '{print $3}'`
 y2=`echo $MAPR | awk -F"/" '{print $4}'`

 extratomo=""
 extratopo=""
 topogrd="topo.grd"
 for i in $*
 do
 case $i in
 border)
  psbasemap -R -O -J -K -B0/0
  ;;
 blocks)
  plotblocks
  ;;
 genericcpt)
  cp generic.cpt color.cpt
  ;;
 tomoeve)
  m=`getfigmodel`
  a=`getfigarea "$m"`
  f="$tomobase/DATA/$a/INIDATA/rays_local.dat"
  #f="$tomobase/DATA/$a/$m/TIMES/srces_true.dat"
  message " Tomo used earthquakes"
  [ ! -f "$f" ] && message " No file $f" && continue
  awk 'NF >= 3 {print $1,$2}' $f |\
   psxy -R -J -O -K -Sc0.05 -Gblack 
  ;;
 label)
  [ ! -z "$refvel" ] && str="Depth = $depth km V@-p@- = $refvel km/s" || str="Depth = $depth km"
  echo "$x $y  $anotFontSize 0 0 BL $str" | pstext -R -O -K -J -N -W255o -D0.2c/0.3c
  ;;
 topo)
  message " Topography"
  [ -f ilutopo.grd ] && extratopo="-Iilutopo.grd" || warning " No topo ilumination"
  grdview $topogrd $extratopo -Ctopo.cpt -Qi -R -J -O -K -B0
  ;;
 gtopo)
  message " Topography"
  [ -f ilutopo.grd ] && extratopo="-Iilutopo.grd" || warning " No topo ilumination"
  grdview $topogrd $extratopo -Ctopo.cpt -Qig -R -J -O -K -B0
  ;;
 tomo)
  message " Tomography"
  [ -f ilutomo.grd ] && extratomo="-Iilutomo.grd" || warning " No tomo ilumination"
  grdview $tomogrd $extratomo -Ccolor.cpt -Qc -R -J -O -K -B0
  ;;
 ctomo)
  message " Synthetic Countour"
  addcont $stomogrd -2 2
  ;;
 stomo)
  message " Synthetic Tomography"
  [ -f ilutomo.grd ] && extratomo="-Iilutomo.grd" || warning " No tomo ilumination"
  grdview $stomogrd $extratomo -Ccolor.cpt -Qc -R -J -O -K -B0
  ;;
 borders)
  message " Borders"
  pscoast -R -J -O -K -A100000 -N1/0.5p,darkred,- -W0.5p
  ;;
 generic)
  message " Generic grid $id"
  [ -f ilutomo.grd ] && extratomo="-Iilutomo.grd" || warning " No Generic ilumination"
  grdview $id $extratomo -Cgeneric.cpt -Qc -R -J -O -K -B0
  ;;
 coast)
  message " Coastline"
  pscoast -R -J -O -K -A100000 -W0.5p -Slightblue
  ;;
 sta|stations)
  message " Stations"
  stations | psxy -R -J -O -K -Si.12 -Gdarkblue
  ;;
 slab)
  message " Slab"
  slabs | psxy -R -J -m -O -K -W0.8p,100,10_8:0.0
  ;;
 vol|volcanos)
  message " Volcanos"
  volcanos | psxy -R -J -O -K -Gdarkred  -St.20
  ;;
 caption*)
  value=`echo $i | cut -d"=" -f 2`
  echo "$x $y2 $anotFontSize 0 0 TL $value" | pstext -R -J -O -K -N -Wwhite -D0.2c/-0.2c
  ;;
 esac
 done

 rm -f color.cpt topo.cpt
 message ""
 return 0
}

##################################################################################################################
### Profile ######################################################################################################
##################################################################################################################
function plotprofile() {
 id=$1;shift
 anot="$1";shift
 shift="$1"; shift
 maxdepth="$1"; shift

 gridfile="${folder}r.v.$id.gmt"
 sgridfile="${folder}s.v.$id.gmt"
 crustfile="${folder}m.v.$id.gmt"

 [[ $* =~ " tomo" && ! -f "$gridfile" ]] && error "Grid file ($gridfile) not found" && return 1
 [[ $* =~ "stomo" && ! -f "$sgridfile" ]] && error "SGrid file ($sgridfile) not found" && return 1
 [[ $* =~ "ctomo" && ! -f "$sgridfile" ]] && error "SGrid file ($sgridfile) not found" && return 1
 [[ $* =~ "moho" && ! -f "$crustfile" ]] && error "Moho file ($crustfile) not found" && return 1

 name=`awk -v id="$id" '$5==id {print $6}' r.v.ref.tab`
 c1=`awk -v id="$id"  '$5==id {print $1"/"$2}' r.v.ref.tab`
 c2=`awk -v id="$id"  '$5==id {print $3"/"$4}' r.v.ref.tab`
 c1d=`awk -v id="$id" '$5==id {print $2" "$1}' r.v.ref.tab`
 c2d=`awk -v id="$id" '$5==id {print $4" "$3}' r.v.ref.tab`

 [ -z "$name" ] && name=$id
 [ -z "$c1" -o -z "$c2" ] && error "Profile coordinates not found." && return
 maxl=`disaz  $c1d $c2d | awk '{print $3}'`

 size="-JX$mapW/$sectionH"

 if [ -f "$gridfile" ]; then
  x1=`grdinfo $gridfile -I2 -C | awk '{print $2}'`
  x2=`grdinfo $gridfile -I2 -C | awk '{print $3}'`
  y1=`grdinfo $gridfile -I2 -C | awk '{print $4}'`
  y2=`grdinfo $gridfile -I2 -C | awk '{print $5}'`
 elif [ -f "$sgridfile" ]; then
  x1=`grdinfo $sgridfile -I2 -C | awk '{print $2}'`
  x2=`grdinfo $sgridfile -I2 -C | awk '{print $3}'`
  y1=`grdinfo $sgridfile -I2 -C | awk '{print $4}'`
  y2=`grdinfo $sgridfile -I2 -C | awk '{print $5}'`
 else
  x1=0.0
  x2=$maxl
  depthsign="+"
  #[ $depthsign == "+" ] && y1=-100.0 || y1=0.0
  [ "$depthsign" == "+" ] && y2=0.0 || y2=100.0
 fi

 if [ ! -z "$maxdepth" ]; then
  y1=-$maxdepth
 fi

 R="-R$x1/$x2/$y1/$y2"
 RT=`echo $R | awk -F"/" 'BEGIN {OFS="/"} {print $1,$2,0.01,1.5}'`

 message "Pofile: $id ($gridfile) $name $R -- $RT"

 [ -f "$ps" ] && open="-O -K" || open="-K"

 Xi=`echo $anot | bsplit xrange`
 Yi=`echo $anot | bsplit yrange`
 xl=`echo $anot | bsplit xlabel`
 yl=`echo $anot | bsplit ylabel`
 axis=`echo $anot | bsplit anot`
 [ -z "$Xi" ] && Xi="$pmarks"
 [ -z "$Yi" ] && Yi="100"
 [ -z "$xl" ] && xl="Distance (km)"
 [ -z "$yl" ] && yl="Depth (km)"

 [ "Z$xl" == "ZEMPTY" ] && xl=""
 [ "Z$yl" == "ZEMPTY" ] && yl=""

 psbasemap $R $size -Glightgray -B${Xi}:"${xl}":/${Yi}:"${yl}":${axis} $open $shift --LABEL_FONT_SIZE=12

 getTomocpt > color.cpt
 ### Normal part of the section
 for i in $*
 do
 case $i in
 genericcpt)
  cp generic.cpt color.cpt
  ;;
 tomo)
  grdview $gridfile -R -J -Ccolor.cpt -Qc -B0/0  -O -K
  ;;
 stomo)
  message " Synthetic Tomography"
  grdview $sgridfile -R -J -Ccolor.cpt -Qc -B0/0  -O -K
  ;;
 ctomo)
  message " Synthetic Countour"
  addcont $sgridfile -2 2
  ;;
 moho)
  message " Moho"
  cat "$crustfile" |\
   grdtrack -G"$gridfile" |\
   awk '$3 != "NaN" {print $1,$2}' |\
   psxy -R -J -O -K -W$lineWidth,--
  ;;
 eve)
  message " Earthquakes"
  earthquakes |\
   palong "$c1" "$c2" $evcol $maxl |\
   awk '{print $4,-1*$3}' |\
   psxy -R -J -O -K -Sc$eveSize -Gblack
  ;;
 tomoeve)
  m=`getfigmodel`
  a=`getfigarea`
  f="$tomobase/DATA/$a/INIDATA"
  message " Tomo used earthquakes"
  [ ! -f "$f/rays_local.dat" ] && message " No file $f/rays_local.dat" && continue
  awk 'NF == 4 {print $1,$2,$3}' $f/rays_local.dat |\
   palong "$c1" "$c2" $evcol $maxl |\
   awk '{print $4,-1*$3}' |\
   psxy -R -J -O -K -Sc$eveSize -Gblack
 esac
 done

 ### Topo part of the section
 psxy /dev/null $RT -JX$mapW/$topH -O -K -Sp.01 -Y$sectionH -B0/1.0Wne
 for i in $*
 do
 case $i in
 topo)
  message " Topography"
  project -C$c1 -E$c2 -Q -G1k |\
   grdtrack -G$topoGrid |\
   awk '{print $3,$4/1000.0}' |\
   psxy $RT -J -O -K -N -W.5p,red
 ;;
 vol)
  message " Volcanos"
  volcanos | palong "$c1" "$c2" $vocol $maxl |\
   awk '{print $3,0}'  |\
   psxy $RT -J -O -K -St0.20 -D0.0/1.0 -Gdarkred -N
 ;;
 sta)
  message " Stations"
  stations |\
   palong "$c1" "$c2" $stcol $maxl |\
   awk '{print $3,0}' |\
   psxy $RT -J -O -K -Si0.25 -D0.0/0.8 -Gdarkblue -N
 ;;
 ns|we|ew|sn)
  a=`echo $i | cut -c1 | tr "[:lower:]" "[:upper:]"`
  b=`echo $i | cut -c2 | tr "[:lower:]" "[:upper:]"`
  message " Label ($a$b)"
  echo "0.0 1.0 $anotFontSize 0 0 CM $a" | pstext -R0/1/0/1 -J -O -K -N -D0.0/0.3
  echo "1.0 1.0 $anotFontSize 0 0 CM $b" | pstext -R0/1/0/1 -J -O -K -N -D0.0/0.3
 ;;
 esac
 done
 ## Reset the shift
 psxy /dev/null $R $size -O -K -Y-$sectionH -Sp.01

 echo "0 0 $anotFontSize 0 0 BL $name" | pstext -R0/1/0/1 -J -O -K -D0.3/0.3 -W255o

 rm -f color.cpt > /dev/null 2>&1
 ## Reset back the coordinates
 psxy /dev/null $R $size -O -K  -Sp.01
 message ""
}

##################################################################################################################
### Add profile
##################################################################################################################
addprofiles() {
 [ $# -eq 0 ] && warning "Invalid id number" && return

 seq=""
 for ele in `echo $* | tr "," " "`
 do
  if [[ $ele =~ "-" ]]; then
    f=`echo $ele | cut -d"-" -f 1`
    l=`echo $ele | cut -d"-" -f 2`
    seq="$seq `seq $f $l | tr "\n" " "`"
  else
    seq="$seq $ele"
  fi
 done

 for id in $seq
 do
  c1=`awk -v id="$id"  '$5==id {print $1"/"$2}' r.v.ref.tab`
  c2=`awk -v id="$id"  '$5==id {print $3"/"$4}' r.v.ref.tab`
  name=`awk -v id="$id"  '$5==id {print $6}' r.v.ref.tab`
  [ -z "$name" ] && name=$id

  # Add the line
  echo -e "$c1\n$c2" | tr "/" " " | psxy -R -J -O -K -W${lineWidth}

  # Add the marks
  project -C$c1 -E$c2 -Q -G$pmarks |\
   tac | awk 'NR > 1 {print $0}'  | tac |\
   psxy -R -J -O -K -Sc$markSize -Gblack

 # Add the name
 project -C$c1 -E$c2 -Q -G100 |\
  awk -v name="$name" -v fontSize=$anotFontSize 'NR == 1 {print $1, $2, fontSize, "0 0 CM", name}' |\
  pstext -R -J -O -K -W255o -D-0.0/-0.3

 done
}

addprofiles2() {
 [ $# -eq 0 ] && warning "Invalid id number" && return

 seq=""
 for ele in `echo $* | tr "," " "`
 do
  if [[ $ele =~ "-" ]]; then
    f=`echo $ele | cut -d"-" -f 1`
    l=`echo $ele | cut -d"-" -f 2`
    seq="$seq `seq $f $l | tr "\n" " "`"
  else
    seq="$seq $ele"
  fi
 done

 for id in $seq
 do
  c1=`awk -v id="$id"  '$5==id {print $1"/"$2}' r.v.ref.tab`
  c2=`awk -v id="$id"  '$5==id {print $3"/"$4}' r.v.ref.tab`
  name=`awk -v id="$id"  '$5==id {print $6}' r.v.ref.tab`
  [ -z "$name" ] && name=$id

  # Add the line
  echo -e "$c1\n$c2" | tr "/" " " | psxy -R -J -O -K -W$lineWidth

  # Add the marks
  project -C$c1 -E$c2 -Q -G$pmarks |\
   tac | awk 'NR > 1 {print $0}'  | tac |\
   psxy -R -J -O -K -Sc$markSize -Gblack

 # Add the name
 project -C$c1 -E$c2 -Q -G100 |\
  awk -v name="$name" -v fontSize=$anotFontSize 'NR == 1 {print $1,$2, fontSize, "0 0 CB", name}' |\
  pstext -R -J -O -K -W255o -D0.3/0.0

 done
}

##################################################################################################################
### Add contour ##################################################################################################
##################################################################################################################
addcont(){
 [ $# -ne 3 ] && return
 contfile="$1"
 levelmin="$2"
 levelmax="$3"
 [ ! -f "$contfile" ] && return
 cat <<EOF > contint
$levelmin C
$levelmax C
EOF
 grdcontour "$contfile" -R -J -O -K -W${lineWidth} -Ccontint
rm -f contint
}

##################################################################################################################
### Colo scale    ################################################################################################
##################################################################################################################
addlegendcolor(){
 yoff=$1 && shift
 title="$1" && shift
 cpt="$1" && shift
 anot="$1" && shift
 remove=0

 [ ! -z "$cpt" -a ! -f "$cpt" ] && error "Cpt ($cpt) not found" && return 1
 [ -z "$yoff" ] && yoff=-0.5

 if [ -z "$cpt" ]; then
   cpt=`mktemp`
   getTomocpt > $cpt
   remove=1
 fi

 [ -z "$title" ] && title="P-wave velocity anomaly - % V@-p@-"
 [ -z "$anot" ] && anot="4g2"
 
 pslegend -Dx0/$yoff/$mapW/3.0/TL -J -R -O -K << EOF
G 0.5
B $cpt 1.0 0.30 -B$anot:"$title": -E -I --LABEL_FONT_SIZE=12 --ANNOT_FONT_SIZE_PRIMARY=12  
G 0.5
EOF

 [ $remove -eq 1 ] && rm -f $cpt
}

##################################################################################################################
### Legend        ################################################################################################
##################################################################################################################
function addlegend() {
 off=0.5
 off2=1.0

tmpA=`mktemp`
tmpB=`mktemp`
tmp=$tmpA
#echo "N 2" > $tmp

echo "G 0.5" >> $tmpA
echo "G 0.5" >> $tmpB

yoff=-2

while [ $# -ge 1 ]
do
 case $1 in
 station|sta)
  echo "S $off i .4 darkblue - $off2 Stations" >> $tmp
  ;;
 volcano|vol)
  echo "S $off t .4 darkred - $off2 Main volcanos" >> $tmp
  ;;
 profiles)
  echo "S $off - .6 - 2p $off2 Profile directions" >> $tmp
  ;;
 slab)
  echo "S $off - .6 - 1p,100,- $off2 Slab contour" >> $tmp
  ;;
 crust)
  echo "S $off - .6   -  1p,-- $off2 Moho depth" >> $tmp
  ;;
 quakes)
  echo "S $off p .1 black - $off2 Earthquakes" >> $tmp
  ;;
 mark)
  echo "S $off c .3 black - $off2 Profiles 100km marks" >> $tmp
  ;;
 yoff)
  shift
  yoff=$1
 esac

 [ $tmp == $tmpA ] && tmp=$tmpB || tmp=$tmpA
 shift
done

 half=`echo "scale=3; $mapW / 2" | bc`
 pslegend $tmpA -Dx0/$yoff/$mapW/2.5/TL -J -R -O -K
 pslegend $tmpB -Dx$half/$yoff/$mapW/2.5/TL -J -R -O -K
 rm -f $tmpA $tmpB
}


##################################################################################################################
### Project Along ################################################################################################
##################################################################################################################
function palong() {
 c1=$1
 c2=$2
 [ $# -ge 3 ] && wfilter="-W-$3/$3"
 [ $# -ge 4 ] && lfilter="-L0/$4"

 project -C$c1 -E$c2 -Q $wfilter $lfilter
}


##################################################################################################################
### Get Topo Color Scale #########################################################################################
##################################################################################################################
function getTopocpt(){
	defaultTopocpt
}

##################################################################################################################
### Default Tomo color scale #####################################################################################
##################################################################################################################
function getTomocpt() {
cat <<EOF
# COLOR_MODEL = RGB
-10.0000	080 010 010	 -9.0000	080 010 010
 -9.0000	129 024 024	 -8.0000	129 024 024
 -8.0000	159 000 000	 -7.0000	159 000 000
 -7.0000	208 000 000	 -6.0000	208 000 000
 -6.0000	255 003 000	 -5.0000	255 003 000
 -5.0000	255 064 000	 -4.0000	255 064 000
 -4.0000	255 117 000	 -3.0000	255 117 000
 -3.0000	255 157 000	 -2.0000	255 157 000
 -2.0000	255 196 000	 -1.0000	255 196 000
 -1.0000	255 235 158	  0.0000	255 235 158
  0.0000	222 255 255	  1.0000	222 255 255
  1.0000	156 255 255	  2.0000	156 255 255
  2.0000	060 224 255	  3.0000	060 224 255
  3.0000	025 192 255	  4.0000	025 192 255
  4.0000	089 160 255	  5.0000	089 160 255
  5.0000	119 136 238	  6.0000	119 136 238
  6.0000	141 114 216	  7.0000	141 114 216
  7.0000	141 077 204	  8.0000	141 077 204
  8.0000	112 019 204	  9.0000	112 019 204
  9.0000	061 000 163	 10.0000	061 000 163
B	080	010	010
F	061	000	163
N	220	220	220
EOF
}

function getDensitycpt() {

cat <<EOF
# COLOR_MODEL = RGB
  0.0000	255 255 255	  0.1000	255 255 255
  0.1000	127 255 000	  0.5000	127 255 000
  0.5000	050 255 050	  0.7500	050 255 050
  0.7500	020 200 020	  1.0000	020 200 020
  1.0000	255 145 000	  3.0000	255 145 000
  3.0000	128 073 000	 10.0000	128 073 000
B	255	255	255
F	000	000	000
N	220	220	220
EOF
}

##################################################################################################################
### Default Topo color scale #####################################################################################
##################################################################################################################
function defaultTopocpt() {
cat <<EOF
#COLOR_MODEL = RGB
#
-8000	0	0	0	-7000	0	5	25
-7000	0	5	25	-6000	0	10	50
-6000	0	10	50	-5000	0	80	125
-5000	0	80	125	-4000	0	150	200
-4000	0	150	200	0	86	197	184
0	86	197	184	200	172	245	168
200	172	245	168	400	211	250	211
400	211	250	211	2000	250	255	255
2000	250	255	255	2500	253	249	218
2500	253	249	218	2800	252	244	182
2800	252	244	182	3000	252	239	156
3000	252	239	156	3500	250	235	132
3500	250	235	132	3900	250	231	108
3900	250	231	108	4300	216	196	87
4300	216	196	87	4700	167	147	68
4700	167	147	68	5000	132	112	55
5000	132	112	55	5500	250	250	250
B	0	0	0
F	255	255	255
N	255	255	255
EOF
}

##################################################################################################################
### Prepare Ilumination ##########################################################################################
##################################################################################################################
function preparetomoilu() {
 gridfile="$1"
 g1="$2"

 R=`grdinfo -C "$gridfile" | awk '{print "-R"$2"/"$3"/"$4"/"$5}'`

 grdcut $R $topoGrid -Gla.grd
 makeilugrid la.grd "$g1" 0.2
 rm -f la.grd

# grdsample `grdinfo -I "$gridfile"` "$g1" -Ga.grd -T
 grdsample `grdinfo -I "$gridfile"` "$g1" -Ga.grd
 mv a.grd "$g1"
}

##################################################################################################################
### Earthquakes   ################################################################################################
##################################################################################################################
function earthquakes() {
  awk '{print $2,$1,$3}' $GMTDATA/catalog/engdahl-all.dat
}

##################################################################################################################
### Slabs         ################################################################################################
##################################################################################################################
function slabs() {
for i in `find $GMTDATA/catalog/slab/ -type f -print`
do
  echo ">"
  awk '{if ($1 != ">" ) {print $2,$1} else {print $0}}' $i
done
}

##################################################################################################################
### Stations      ################################################################################################
##################################################################################################################
function stations() {
if [ "$TOMOSET" == "BR" ]; then
cat << EOF | awk '{print $1,$2}'
 -55.70  -20.476    0.00 AQDB
 -48.53  -21.066    0.00 BB19
 -44.76  -20.998    0.00 BSCB
 -52.84  -18.769    0.00 C2SB
 -50.85  -29.314    0.00 CNLB
 -53.49  -30.512    0.00 CPSB
 -49.56  -23.343    0.00 FRTB
 -52.13  -27.235    0.00 ITAB
 -50.36  -19.704    0.00 ITRB
 -48.30  -12.106    0.00 PEXB
 -53.65  -31.755    0.00 PLTB
 -46.44  -18.537    0.00 PMNB
 -52.06  -24.716    0.00 PTGB
 -47.53  -22.419    0.00 RCLB
 -41.18  -18.703    0.00 SJMB
 -52.63  -22.795    0.00 TRCB
 -51.33  -20.672    0.00 TRIB
 -46.97  -23.002    0.00 VABB
 -55.80  -10.873    0.00 CLDB
 -54.88  -17.600    0.00 PP1B
 -54.71  -34.333    0.00 OGAUY
 -44.31  -15.058    0.00 JANB
 -56.73  -11.612    0.00 PDRB
 -48.21  -17.983    0.00 IPMB
 -55.69  -15.901    0.00 SALV
 -47.59   -8.862    0.00 SMTB
 -40.73  -20.617    0.00 ALF01
 -41.66  -21.826    0.00 CAM01
 -42.37  -22.081    0.00 DUB01
 -48.10  -24.811    0.00 JAC01
 -43.96  -22.865    0.00 MAN01
 -47.28  -24.290    0.00 PET01
 -40.39  -19.314    0.00 RIB01
 -45.16  -23.324    0.00 SLP01
 -49.00  -25.324    0.00 TIJ01
 -43.44  -22.280    0.00 VAS01
 -49.01  -27.397    0.00 MAJ01
 -49.13  -28.532    0.00 TER01
 -40.50  -16.400    0.00 RBNB
 -56.62  -29.671    0.00 Uruguaiana
 -57.70  -22.000    0.00 PMurt
 -57.60  -18.900    0.00 Corumb
 -59.00  -15.800    0.00 Caceres
 -53.00  -14.000    0.00 UnB_MT
 -61.00  -13.000    0.00 Rondonia
 -43.00  -18.000    0.00 Minas
 -57.00  -15.500    0.00 MT1
 -54.00  -16.000    0.00 MT2
 -52.30  -16.700    0.00 MT3
 -56.50  -17.000    0.00 Pant1
 -56.00  -18.500    0.00 Pant2
 -57.00  -20.500    0.00 PantS
 -55.00  -19.000    0.00 MS1
 -53.50  -20.500    0.00 MS2
 -55.00  -23.000    0.00 MS3
 -60.00  -21.000    0.00 PY1
 -60.00  -23.000    0.00 PY2
 -58.00  -24.000    0.00 PY3
 -56.00  -25.000    0.00 PY4
 -54.00  -24.500    0.00 PR1
 -53.00  -26.000    0.00 PR2
 -54.00  -28.000    0.00 RS1
 -55.00  -29.000    0.00 RS2
 -57.00  -31.000    0.00 UY1
 -58.00  -32.500    0.00 UY2
 -54.70  -26.500    0.00 AR1
 -57.00  -27.600    0.00 AR2
 -60.00  -26.000    0.00 AR4
 -60.00  -28.000    0.00 AR5
 -60.50  -31.000    0.00 AR6
 -59.50  -17.000    0.00 BO1
 -59.50  -18.500    0.00 BO2
EOF
elif [ "$TOMOSET" == "ANDES" ]; then
cat << EOF | awk '{print $1,$2}'
-67.01085 -26.74054 PD EW01
-66.83708 -26.75064 PD EW02
-66.75685 -26.85890 PD EW03
-66.50560 -26.81051 PD EW04
-66.29530 -26.78616 PD EW05
-66.12803 -26.84903 PD EW06
-66.04399 -26.89498 PD EW07
-65.78853 -26.93577 PD EW08
-65.69293 -26.95181 PD EW22
-65.39759 -27.04078 PD EW24
-66.94810 -25.84465 PD GALAN
-67.42625 -26.09811 PD NS01
-67.41432 -26.20073 PD NS02
-67.38685 -26.31547 PD NS03
-67.33714 -26.42004 PD NS04
-67.40669 -26.03858 PD NS19
-67.37074 -25.93144 PD NS20
-67.29403 -25.83913 PD NS21
-67.26596 -25.75669 PD NS22
-67.22632 -25.66313 PD NS23
-67.23443 -25.58281 PD NS24
-67.21032 -25.51773 PD NS25
-67.20659 -25.41476 PD NS26
-68.24326 -26.00962 PD BB05
-68.37226 -25.54584 PD BB06
-65.86607 -26.62990 PD BB16
-65.89589 -25.91444 PD BB17
-66.36365 -26.54278 PD BB18
-66.78656 -25.43882 PD BB20
-65.94144 -25.53586 PD BB21
-67.40337 -26.51252 X6 NS05
-67.43999 -26.61626 X6 NS06
-67.55050 -26.66767 X6 NS07
-67.64845 -26.78411 X6 NS08
-67.65257 -26.91009 X6 NS09
-67.68758 -27.05054 X6 NS10
-67.70256 -27.11250 X6 NS11
-67.75683 -27.22831 X6 NS12
-67.77361 -27.33600 X6 NS13
-67.69418 -27.44366 X6 NS14
-67.60333 -27.47958 X6 NS15
-67.61336 -27.56860 X6 NS16
-67.62470 -27.70794 X6 NS17
-67.62895 -27.90084 X6 NS18
-67.18414 -26.68414 X6 EW09
-67.26208 -26.59208 X6 EW10
-67.67617 -26.66623 X6 EW11
-67.77360 -26.68510 X6 EW12
-67.87725 -26.65231 X6 EW13
-68.00261 -26.70900 X6 EW14
-68.16286 -26.74437 X6 EW15
-68.27663 -26.85147 X6 EW16
-68.14371 -27.42318 X6 BB01
-68.09003 -27.11112 X6 BB02
-67.61808 -25.51601 X6 BB07
-67.91799 -26.01605 X6 BB08
-68.09876 -26.52510 X6 BB09
-67.69070 -26.37901 X6 BB10
-66.94356 -27.11246 X6 BB11
-67.12454 -27.51315 X6 BB12
-66.59798 -27.64128 X6 BB13
-65.97848 -27.59715 X6 BB14
-66.36978 -27.34054 X6 BB15
-67.06150 -26.30630 X6 BB19
-68.09766 -27.83271 X6 BB24
-68.43 -26.88 X6 EW17
-68.55 -26.91 X6 EW18
-68.67 -26.88 X6 EW19
-68.77 -26.91 X6 EW20
-69.26 -27.49 X6 BB03
-69.02 -26.54 X6 BB04
-69.24 -26.16 X6 BB22
-69.43 -25.37 X6 BB23
EOF
else 
    local area=$(getfigarea)
    local model=$(getfigmodel)
    awk '{print $1,$2}' $tomobase/DATA/$area/INIDATA/stations_local.dat
fi
}

##################################################################################################################
### bsplit        ################################################################################################
##################################################################################################################
function bsplit() {
	awk -v item="$1" '
BEGIN {
	a[0] = $0;
}
{
	pos = match($0,/[p|s]?([afg0-9]+)?((:)([^:.]*)(:))?[/]?([afg0-9]+)?((:)([^:.]+)(:))?((:[.])([^:.]*)(:))?([WwSsNneE]+)?/,a);
}
END {
	if (pos == -1) echo "No match";
	if (item == "all") print a[0];
	if (item == "xrange") print a[1];
	if (item == "xlabel") print a[4];
	if (item == "yrange") print a[6];
	if (item == "ylabel") print a[9];
	if (item == "mainl") print a[13];
	if (item == "anot") print a[15];
}'
}

##################################################################################################################
### Shift paper   ################################################################################################
##################################################################################################################
function g_shift() {
 shift="$*"
 [ -s "$ps" ] && open="-O -K" || open="-K"
 [ -s "$ps" ] && R="-R" || R="-R0/1/0/1"
 psxy /dev/null $R -JX1/1 -Sp $open $shift
}

##################################################################################################################
### Shift paper   ################################################################################################
##################################################################################################################
function g_end() {
 psxy /dev/null -R0/1/0/1 -JX1/1 -Sp -O
}

##################################################################################################################
### Prepare ilu   ################################################################################################
##################################################################################################################
function prepareTopo(){
 rm -f topo.grd
 rm -f ilutopo.grd

 grdcut $MAPR $topoGrid -Gtopo.grd
 [ $? -ne 0 ] && return
 makeilugrid topo.grd ilutopo.grd 0.2
}

function makeilugrid() { 
 filein="$1"
 fileout="$2"
 [ $# -eq 3 ] && factor=$3 || factor=0.3

 [ ! -f "$filein" ] && echo "Need input file"  && return
 [ -z "$fileout" ]  && echo "Need output file" && return
 [ -f "$fileout" ]  && echo "Erasing $fileout" && rm -f "$fileout"

 grdgradient "$filein" -N0.5 -A90 -Gint.grd
 grdhisteq int.grd -Gsmooth_int.grd -N -V
 grdmath smooth_int.grd $factor x = "$fileout"

 rm -f int.grd smooth_int.grd
}

##################################################################################################################
### Communication ################################################################################################
##################################################################################################################
function warning() {
  echo -e "\e[33;1m$*\e[m" >> $LOG
}

function message() {
  echo -e "$*" >> $LOG
}

function error() {
  echo -e "\e[31;1m$*\e[m" >> $LOG
}
## END

##################################################################################################################
### Volcanos      ################################################################################################
##################################################################################################################
function volcanos() {
cat <<EOF | awk '{print $2,$1}'
-25.98	-66.93  - - GALAN
-27.12	-68.55	6887	1900	Ojos de Salado
-26.62	-68.15	5740	Peinado
-26.47	-67.47	CarachiPampa
-25.56	-67.90	Antofagasta
-26.76	-67.74	Cerro Blanco
-27.20	-66.30	Farallon Negro
EOF
 awk '{print $2,$1}' $GMTDATA/catalog/important.dat
 awk '{print $2,$1}' $GMTDATA/catalog/global-volcano-list.txt
}

