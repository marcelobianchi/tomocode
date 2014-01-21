#/bin/bash -l

##################################################################################################################
### BLN to GMT ###################################################################################################
##################################################################################################################
function streambln() {
 file="$1"
 [ -f "$file" ] && awk 'NR>1 {print $0}' "$file" || echo ""
}

source $BSHM/tomo.bshm
[ `pwd` != "$tomobase/FIG_FILES" ] && echo "I should be in the fig-files dir !" && exit

area=$(getfigarea)
model=$(getfigmodel)
it=$(getfigit)
checkmodel "$area" "$model" || exit

echo "Area:$area Model:$model Iteration:$it"

hmin=1
hmax=$(getnhor)
vmin=1
vmax=$(getnver)
echo "Hmin: $hmin Hmax: $hmax Vmin: $vmin Vmax: $vmax"

ros=`ros "$area" "$model"`
echo "Model is $ros"

outputfolder="$tomobase/DATA/$area/$model/GMTGRIDS"
[ -d "$outputfolder" ] && rm -rf "$outputfolder"
mkdir -p "$outputfolder/" && echo "Output to: $outputfolder"

tomo saveParams

################################################################################################################################################
# Prepare the horizontal grids #################################################################################################################
################################################################################################################################################
echo "Horizontal:"
for i in `seq $hmin $hmax`
do 
    id=`printf "%2d" $i`
    outreal="r.h.$i.gmt"
    outsynt="s.h.$i.gmt"

    #Real
    gridfile="`ls -1 "HOR/dv1_$id.grd" 2> /dev/null`"
    [ ! -s "$gridfile" ] && echo "${id}R) Fail" || echo "${id}R) OK ($gridfile)"
    [ -s "$gridfile" ] && grids2g.sh "$gridfile" "$outputfolder/$outreal" "-I0.02/0.02"

    #Synth
    [ "$ros" == "r" ] && continue
    gridfile="`ls -1 "SYN_INI/hor1_$id.grd" 2> /dev/null`"
    [ ! -s "$gridfile" ] && echo "${id}S) Fail" || echo "${id}S) OK ($gridfile)"
    [ -s "$gridfile" ] && grids2g.sh "$gridfile" "$outputfolder/$outsynt" "-I0.02/0.02"
done
[ -f "HOR/refvelocities.dat" ] && echo "Saving Reference Velocties." && cp HOR/refvelocities.dat  "$outputfolder/r.h.ref.tab"

################################################################################################################################################
# Prepare the vertical grids ###################################################################################################################
################################################################################################################################################
echo "Vertical:"
for i in `seq $vmin $vmax`
do 
    id=`printf "%2d" $i`
    outreal="r.v.$i.gmt"
    outsynt="s.v.$i.gmt"

    # Real
    gridfile="`ls -1 "VERT/ver_1$id.grd" 2> /dev/null`"
    [ ! -s "$gridfile" ] && echo "${id}R) Fail" || echo "${id}R) OK ($gridfile)" 
    [ -s "$gridfile" ] && grids2g.sh "$gridfile" "$outputfolder/$outreal" "-I0.5/2.5"

    # Moho line
    blnfile="`ls -1 "VERT/moho_$id.bln" 2> /dev/null`"
    [ ! -s "$blnfile" ] && echo "${id}M) Fail" || echo "${id}M) OK ($blnfile)" 
    [ -s "$blnfile" ] &&  streambln "$blnfile" > "$outputfolder/m.v.$i.gmt"

    #Synth
    [ "$ros" == "r" ] && continue
    gridfile="`ls -1 "SYN_INI/syn_dv${id}1.grd" 2> /dev/null`"
    [ ! -s "$gridfile" ] && echo "${id}S) Fail" || echo "${id}S) OK ($gridfile)"
    [ -s "$gridfile" ] && grids2g.sh "$gridfile" "$outputfolder/$outsynt" "-I0.5/2.5"
done
[ -f "VERT/referencever.dat" ] && echo "Saving Reference Profiles." && cp VERT/referencever.dat  "$outputfolder/r.v.ref.tab"

################################################################################################################################################
# Prepare the v5d cube grids ###################################################################################################################
################################################################################################################################################
if [ "Z`which tomo2v5d > /dev/null 2>&1`" != "Z" ] 
then 
    echo "Vis 5d"
    datagrid="3D_MODEL/dv3d_perc_1${it}.dat"
    synthgrid="3D_MODEL/vsyn_3d_1.dat"
    [ "$ros" == "s" ] && outgrid="r.s.$it.v5d" || outgrid="r.$it.v5d"

    if [ "$ros" == "r" -a -s "$datagrid" ]
    then
        echo "Converting Data 3D model."
        tomo2v5d "$datagrid" "$outputfolder/$outgrid"
        exit
    fi

    if [ "$ros" == "s" -a -s "$datagrid" -a -s "$synthgrid" ]
    then
        echo "Converting Data/Synth 3D model"
        tomo2v5d "$datagrid" "$outputfolder/$outgrid" "$synthgrid"
        exit
    fi

    if [ "$ros" == "s" -a -s "$datagrid" -a ! -s "$synthgrid" ]
    then
        echo "Converting Data Only in Synthtic dir"
        tomo2v5d "$datagrid" "$outputfolder/$outgrid"
        exit
    fi

    echo " Fail to make 3D vis5d file."
fi

echo ""
echo "------------------------------------------------------------------------"
echo "WARNING*WARNING*WARNING*WARNING*WARNING*WARNING*WARNING*WARNING*WARNING*"
echo "Now you can type: tomo gogmt to go to the folder where all the data was exported."
echo "------------------------------------------------------------------------"
