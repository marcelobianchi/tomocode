if [ -a moho_coordinates.dat ]; then rm moho_coordinates.dat; fi

for long in `seq 78 85`
do
	for lat in `seq 37 42`
	do

		echo $long".0" $lat".0" >> moho_coordinates.dat


	done
done
