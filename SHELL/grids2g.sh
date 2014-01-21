#!/bin/bash

function getnumbersequence(){
awk 'NR>5{print $0}' "$1" |\
  sed -re 's/^\ +//g' |\
  sed -re 's/\ +$//g' |\
  sed -re 's/\ +\n?/\n/g' |
  awk '{print $1}'
}

if [ $# -lt 1 ]
then
 echo "Usage: grids2g.sh <Input Grd> [Output Grd]"
 exit
fi

if [ -n "$1" -a -f "$1" ]
then
 id=`head -1 "$1" | awk 'NR==1 {print $1}'`
 [ "$id" != "DSAA" ] && echo "Not a Surfer grid file $id." && exit
 file="$1"
else
 echo "E:> Input file (${1}) not found."
 exit
fi

[ $# -ge 2 ] && outfile="$2" || outfile=`basename "$file" .grd`.gmt
[ -f "$outfile" ] && rm "$outfile"

[ $# -eq 3 ] && ressample="$3"

# Finished parsing #####################################################################################

nx=`sed -n -e '2p'  "$file" | awk '{print $1}'`
ny=`sed -n -e '2p'  "$file" | awk '{print $2}'`
n=`sed -n -e '2p'  "$file" | awk '{print $1*$2}'`
minlon=`sed -n -e '3p'  "$file" | awk '{print $1}'`
maxlon=`sed -n -e '3p'  "$file" | awk '{print $2}'`
minlat=`sed -n -e '4p'  "$file" | awk '{print $1}'`
maxlat=`sed -n -e '4p'  "$file" | awk '{print $2}'`

hh=`sed -n -e '5p'  "$file" | awk '{print $2}'`
ll=`sed -n -e '5p'  "$file" | awk '{print $1}'`
#echo "From $hh to $ll"
count=`getnumbersequence "$file" |wc -l`

#export ll=0.001

# Check sample count.
[ $count -ne $n ] && echo "Wrong number of data points. Expected: $n ($nx, $ny) Get: $count" && exit

#echo "I:> Converting: $file"
#echo " I:> Nx: $nx Ny: $ny N Expected: $n "
#echo " I:> Y-axis: $minlat $maxlat X-axis: $minlon $maxlon"
#echo " I:> Output file is: $outfile"

getnumbersequence "$file" | xyz2grd -N$ll -G"$outfile" -ZBLa -R1/$nx/1/$ny -I1/1 
#getnumbersequence "$file" | xyz2grd -G"$outfile" -ZBLa -R1/$nx/1/$ny -I1/1 
grdedit -R$minlon/$maxlon/$minlat/$maxlat -D"Longitude"/"Latitude"/=/=/=/"$file"/=  "$outfile"

if [ ! -z "$ressample" ]
then
  grdsample $ressample "$outfile" -Gla
  mv la "$outfile"
fi
 
