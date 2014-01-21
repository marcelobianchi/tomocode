!USE DFPORT

character*8 ar,ar_syn,ar_real,line
character*8 re,re_syn,re_real
character*1 rg_all(100),rg
integer pausing

common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)

! Enable or disable pausing
pausing=0
open(61, file='../../kod_syn_all.dat')
read(61,*) num_models
read(61,*) 
read(61,*) 
read(61,*) 
do imodel=1,num_models

	read(61,'(a8,3x,a8,3x,i1)')re_syn,re_real
	read(61,'(a8,3x,a8,3x,i1,3x,i1)')ar_syn,ar_real,niter,kod_1d
	read(61,*)

	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\3D_MODEL')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\GRIDS')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\RESULT')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\TIMES')
	if (pausing.eq.1) pause

	write(*,*)' input:',ar_real
	write(*,*)' output:',ar_syn
	write(*,*)' iterations:',niter

	open(11, file='../../kod_syn.dat')
	write(11,'(a8,1x,a8)')re_syn,ar_syn
	write(11,'(a8,1x,a8)')re_real,ar_real
	close(11)


	ar=ar_syn
	re=re_syn

	call copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_syn.dat','..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
	if (pausing.eq.1) pause


	open(11,file='../../model.dat')
	write(11,'(a8)')re		
	write(11,'(a8)')ar		
	close(11)

	
	write(*,*) '*************************************'
	write(*,*)'reference table, synthetic 1D model'
	write(*,*) '*************************************'

	i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
	if (pausing.eq.1) pause

	write(*,*)'	 ****************************************************'
	write(*,*)'	 Compute the synthetic times: '
	i=runcommand('..\B_SYNTH_TIMES\rays.exe')
	if (pausing.eq.1) pause


	
	call copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_start.dat', '..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
	if (pausing.eq.1) pause

	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
		do i=1,10000
			read(1,'(a8)',end=511)line
			if(line.eq.'SELECT_P') goto 512
		end do
		511 continue
		write(*,*)' cannot find SELECT PARAMETERS in major_param.dat!!!'
		if (pausing.eq.1) pause
		512 continue
			read(1,*)
			read(1,*) kod_local
	close(1)


	if(kod_1d.ne.0) then

		open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ref_syn.dat')
		read(1,*,end=81)vpvs
		i=0
		82	i=i+1
			read(1,*,end=81)hmod(i),vmodp(i),vs
			if(vpvs.lt.0.000001) then
				vmods(i)=vs
			else
				vmods(i)=vmodp(i)/vpvs
			end if
			!write(*,*)hmod(i),vmodp(i),vmods(i)
		goto 82
		81	close(1)
		nrefmod=i-1
		!write(*,*)' nrefmod=',nrefmod


		open(21,file='../../FIG_FILES/1DMOD/ref_true.bln')
		write(21,*)nrefmod
		do i=1,nrefmod
			write(21,*)vmodp(i),-hmod(i)
		end do
		write(21,*)nrefmod
		do i=1,nrefmod
			write(21,*)vmods(i),-hmod(i)
		end do
		close(21)

		open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ref_start.dat')
		read(1,*,end=81)vpvs
		i=0
		182	i=i+1
			read(1,*,end=181)hmod(i),vmodp(i),vs
			if(vpvs.lt.0.000001) then
				vmods(i)=vs
			else
				vmods(i)=vmodp(i)/vpvs
			end if
			!write(*,*)hmod(i),vmodp(i),vmods(i)
		goto 182
		181	close(1)
		nrefmod=i-1
		!write(*,*)' nrefmod=',nrefmod


		open(21,file='../../FIG_FILES/1DMOD/ref_start.bln')
		write(21,*)nrefmod
		do i=1,nrefmod
			write(21,*)vmodp(i),-hmod(i)
		end do
		write(21,*)nrefmod
		do i=1,nrefmod
			write(21,*)vmods(i),-hmod(i)
		end do
		close(21)




		open(11,file='../../model.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		close(11)
		i=runcommand('..\..\1D_MODEL\START_1D\start_real.exe')
		if (pausing.eq.1) pause

	end if

	open(11,file='../../model.dat')
	write(11,'(a8)')re		
	write(11,'(a8)')ar		
	write(11,'(i1)')1		
	write(11,'(i1)')1	
	write(11,'(i1)')0	
	close(11)

	write(*,*) '*************************************'
	write(*,*)'reference table, final 1D model'
	write(*,*) '*************************************'

	i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
	if (pausing.eq.1) pause

	if (kod_local.eq.1) then
		write(*,*) '*************************************'
		write(*,*)'location in optimized 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
		if (pausing.eq.1) pause
	end if


	!******************************************************************
	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
	do i=1,10000
		read(1,'(a8)',end=573)line
		if(line.eq.'ORIENTAT') goto 574
	end do
	573 continue
	write(*,*)' cannot find ORIENTATIONS in major_param.dat!!!'
	if (pausing.eq.1) pause
	574 read(1,*)nornt
	close(1)
	!******************************************************************
	kod_param=1
	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
	do i=1,10000
		read(1,'(a8)',end=513)line
		if(line.eq.'GRID_PAR') goto 514
	end do
	513 continue
	write(*,*)' cannot find GRID PARAMETERS in major_param.dat!!!'
	if (pausing.eq.1) pause
	514 continue
	read(1,*)
	read(1,*)
	read(1,*)
	read(1,*)kod_param
	close(1)
	!******************************************************************



	! Execute the inversion for grids with different orientations:
	do iter=1,niter	
		write(*,*)
		write(*,*)
		write(*,*)
		write(*,*)
		write(*,*)' model:',ar,' iteration:',iter


		open(11,file='../../model.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		write(11,'(i1)')iter		
		write(11,'(i1)')1	
		close(11)

		write(*,*)'	 ****************************************************'
		write(*,*)'	 Source location'
		!if (iter.ne.1)	i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
		i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
		if (pausing.eq.1) pause

		if(kod_param.eq.1) then

			do igr=1,nornt
				open(11,file='../../model.dat')
				write(11,'(a8)')re		
				write(11,'(a8)')ar		
				write(11,'(i1)')iter		
				write(11,'(i1)')igr	
				close(11)

				if(iter.eq.1) then

					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the ray density'
					i=runcommand('..\2_RAY_DENSITY\plotray.exe')
					if (pausing.eq.1) pause
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3_GRID\grid.exe')
					if (pausing.eq.1) pause
					i=runcommand('..\4_TETRAD\tetrad.exe')
					if (pausing.eq.1) pause
					i=runcommand('..\5_SOSEDI\add_matr.exe')
					if (pausing.eq.1) pause
				end if
				i=runcommand('..\6_MATR\matr.exe')
				if (pausing.eq.1) pause
				i=runcommand('..\7_INVERS\invbig.exe')
				if (pausing.eq.1) pause
			end do
			i=runcommand('..\8_3D_MODEL\mod_3d.exe')
			if (pausing.eq.1) pause

		else if(kod_param.eq.2) then

			do igr=1,nornt
				open(11,file='../../model.dat')
				write(11,'(a8)')re
				write(11,'(a8)')ar
				write(11,'(i1)')iter
				write(11,'(i1)')igr
				close(11)
				if(iter.eq.1) then

					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the ray density'
					i=runcommand('..\2a_RAY_DENSITY\plotray.exe')
					if (pausing.eq.1) pause
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3a_GRID\block.exe')
					if (pausing.eq.1) pause
				end if
				i=runcommand('..\6a_MATR\matr_hor.exe')
				if (pausing.eq.1) pause
				i=runcommand('..\7a_INVERS\invers_hor.exe')
				if (pausing.eq.1) pause
			end do
			i=runcommand('..\8a_3D_MODEL\3d_model_block.exe')
			if (pausing.eq.1) pause
		end if
	end do ! Different iterations
end do
close(1)
stop
end
