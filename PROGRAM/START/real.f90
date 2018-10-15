!USE DFPORT
character*8 ar,ar_all(10),re,re_all(10),line
character*1 rg_all(100),rg,it
integer kod_loc(10),kod_iter(10),kod_oe(10),kod_rf(10)
integer pausing

! Variables used to select the model
! from the argument to the program
integer start,end
integer whichmodel, narg, iargc
character parinput

! Define if you want to pause at every stage
pausing=0

open(1, file='../../all_areas.dat')
do i=1,5
	read(1,*)
end do
do i=1,10
	read(1,'(a8,1x,a8,3(1x,i1))',end=7)re_all(i),ar_all(i),kod_iter(i),kod_oe(i),kod_rf(i)
	write(*,*)re_all(i),' ',ar_all(i),kod_iter(i),kod_rf(i)
end do	
7 close(1)
n_ar=i-1
write(*,*)' n_ar=',n_ar

!Defaults
start=1
end=n_ar

!Decide the model to run
narg=iargc()
IF (narg.GE.1) THEN
 CALL getarg (1,parinput)
 READ(parinput,*) whichmodel
 IF ((n_ar.LT.whichmodel).OR.(whichmodel.LE.0)) THEN
  WRITE(*,*) 'Model indicated does not exist'
  STOP
 ENDIF
 start=whichmodel
 end=whichmodel
ENDIF

! Check all the areas:
do iar=start,end
	re=re_all(iar)
	ar=ar_all(iar)
	niter=kod_iter(iar)
	koe=kod_oe(iar)
	kref=kod_rf(iar)

	write(*,*) 'New Area: ', ar, 'Niter: ',niter
	call createdir('..\..\DATA\'//re//'\'//ar//'\3D_MODEL')
	call createdir('..\..\DATA\'//re//'\'//ar//'\GRIDS')
	call createdir('..\..\DATA\'//re//'\'//ar//'\RESULT')
	call createdir('..\..\DATA\'//re//'\'//ar//'\TIMES')
	if (pausing.eq.1) call pause()

	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
	do i=1,10000
		read(1,'(a8)',end=511)line
		if(line.eq.'SELECT_P') goto 512
	end do
	511 continue
	write(*,*)' cannot find SELECT PARAMETERS in major_param.dat!!!'
	if (pausing.eq.1) call pause()
	512 continue
		read(1,*)
! Check if local or teleseismic events are used 
		read(1,*) kod_local		
	close(1)


	CALL copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_start.dat','..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
	!CALL copyfile('..\..\DATA\'//re//'\INIDATA\rays_local.dat','..\..\DATA\'//re//'\'//ar//'\TIMES\rays_loc.dat')
	if (pausing.eq.1) call pause()
	
	if (kod_local.eq.0) goto 78

	if(kref.eq.1) then

		open(11,file='../../model.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		write(11,'(i1)')1		
		write(11,'(i1)')1	
		write(11,'(i1)')koe	
		close(11)

		write(*,*) '*************************************'
		write(*,*)'reference table, start 1D model'
		write(*,*) '*************************************'


		i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
		if (pausing.eq.1) call pause()

		write(*,*) '*************************************'
		write(*,*)'Location in start 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
		if (pausing.eq.1) call pause()

		open(11,file='../../model.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		close(11)
	
		write(*,*) '*************************************'
		write(*,*)'optimization of 1D model'
		write(*,*) '*************************************'
		
		i=runcommand('..\..\1D_MODEL\START_1D\start_real.exe')
		if (pausing.eq.1) call pause()
	end if

	78 continue
	!call pause()

	open(11,file='../../model.dat')
	write(11,'(a8)')re		
	write(11,'(a8)')ar		
	write(11,'(i1)')1		
	write(11,'(i1)')1	
	write(11,'(i1)')koe	
	close(11)

	write(*,*) '*************************************'
	write(*,*)'reference table, optimized 1D model'
	write(*,*) '*************************************'

	i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
	if (pausing.eq.1) call pause()

	if (kod_local.eq.1) then
		write(*,*) '*************************************'
		write(*,*)'location in optimized 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
		if (pausing.eq.1) call pause()
	end if

	!******************************************************************
	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
	do i=1,10000
		read(1,'(a8)',end=573)line
		if(line.eq.'ORIENTAT') goto 574
	end do
	573 continue
	write(*,*)' cannot find ORIENTATIONS in major_param.dat!!!'
	if (pausing.eq.1) call pause()

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
	if (pausing.eq.1) call pause()

	514 continue
	read(1,*)
	read(1,*)
	read(1,*)
	read(1,*)kod_param
	close(1)
	!******************************************************************

	write(*,*) '*************************************************'
	write(*,*)'SELECT DATA for itereative Tomo inversion'
	write(*,*) '*************************************************'
	i=runcommand('..\0_SELECT_DATA\select.exe')
	if (pausing.eq.1) call pause()


! Execute the inversion for grids with different orientations:
	do iter=1,niter	

		write(it,'(i1)')iter

		!goto 999

		open(11,file='../../model.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		write(11,'(i1)')iter		
		write(11,'(i1)')1	
		write(11,'(i1)')koe	
		close(11)


		write(*,*)'	 ****************************************************'
		write(*,*)'	 3D rays tracing'
		!if (iter.ne.1)	i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
		i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
		if (pausing.eq.1) call pause()

999		continue

		open(11,file='../../VISUAL/area.dat')
		write(11,'(a8)')re		
		write(11,'(a8)')ar		
		write(11,*)iter		
		write(11,*)1,nornt	
		write(11,*)0	
		write(11,*)0	
		write(11,*)0	
		write(11,*)0	
		write(11,*)0	
		close(11)
! now the travel time residual should be calculated 
! (Corrected for Moho depth and station elevation!)

! if grid is paramized in nodes:
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
					if (pausing.eq.1) call pause()
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3_GRID\grid.exe')
					if (pausing.eq.1) call pause()
					i=runcommand('..\4_TETRAD\tetrad.exe')
					if (pausing.eq.1) call pause()
					i=runcommand('..\5_SOSEDI\add_matr.exe')
					if (pausing.eq.1) call pause()
				end if
				i=runcommand('..\6_MATR\matr.exe')
				if (pausing.eq.1) call pause()
				i=runcommand('..\7_INVERS\invbig.exe')
				if (pausing.eq.1) call pause()
			end do
			i=runcommand('..\8_3D_MODEL\mod_3d.exe')
			if (pausing.eq.1) call pause()
			!i=runcommand('..\..\VISUAL\HOR_RESULT_NODES\visual.exe')
			!if (pausing.eq.1) call pause()
! if grid is paramized in cells:
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
					if (pausing.eq.1) call pause()
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3a_GRID\block.exe')
					if (pausing.eq.1) call pause()
				end if
				i=runcommand('..\6a_MATR\matr_hor.exe')
				if (pausing.eq.1) call pause()
				i=runcommand('..\7a_INVERS\invers_hor.exe')
				if (pausing.eq.1) call pause()
			end do
			i=runcommand('..\8a_3D_MODEL\3d_model_block.exe')
			if (pausing.eq.1) call pause()
		end if

	end do ! Different iterations finished
end do	! Different areas finished 
stop
end

