!USE DFPORT
character*8 ar,ar_all(10),re,re_all(10),line
character*1 rg_all(100),rg,it
integer kod_loc(10),kod_iter(10),kod_oe(10),kod_rf(10)
character*128 arg

! Variables used to select the model
! from the argument to the program
integer narg, iargc

! Define if you want to pause at every stage
logical pausing, full, selcalc, loccalc, invcalc, error

! initiallize
pausing  = .FALSE.
full     = .FALSE.
selcalc  = .FALSE.
loccalc  = .FALSE.
invcalc  = .FALSE.

! Parsing CMD line
do i = 1, iargc()
	call getarg(i, arg)
	if (arg.eq.'-sel') then
		selcalc = .TRUE.
		if (full.or.loccalc.or.invcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-loc') then
		loccalc = .TRUE.
		if (full.or.selcalc.or.invcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-inv') then
		invcalc = .TRUE.
		if (full.or.loccalc.or.selcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-full') then
		full = .TRUE.
		if (selcalc.or.loccalc.or.invcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-pausing'.or.arg.eq.'-pause') then
		pausing = .TRUE.
	end if
end do

write(*,'(a)') 'Running:'
write(*,'(a,l)') ' Full =',full
write(*,'(a,l)') ' Selection =',selcalc
write(*,'(a,l)') ' Location  =',loccalc
write(*,'(a,l)') ' Inversion =',invcalc

if (.not.(full.or.selcalc.or.invcalc.or.loccalc)) then
	write (*,*)
	write (*,*) 'Bad command line argument. Usage:'
	write (*,*) '    start_synth -full|-sel|-loc|-inv [-pausing]'
	stop
end if

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

! Check all the areas:
do iar=1,n_ar
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
	if (pausing) call pause()

	call readmajor(re, ar, error, kod_local, nornt, kod_param)
	if (error) stop

	CALL copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_start.dat','..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
	!CALL copyfile('..\..\DATA\'//re//'\INIDATA\rays_local.dat','..\..\DATA\'//re//'\'//ar//'\TIMES\rays_loc.dat')
	if (pausing) call pause()
	
	if (kod_local.eq.0) goto 78

	if(kref.eq.1) then
		write(*,*) '*************************************'
		write(*,*)'reference table, start 1D model'
		write(*,*) '*************************************'

		call writemodel(re, ar, 1, 1, koe)
		i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
		if (pausing) call pause()

		write(*,*) '*************************************'
		write(*,*)'Location in start 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
		if (pausing) call pause()

		write(*,*) '*************************************'
		write(*,*)'optimization of 1D model'
		write(*,*) '*************************************'

		call writemodel(re, ar, -1, -1, -1)
		i=runcommand('..\..\1D_MODEL\START_1D\start_real.exe')
		if (pausing) call pause()
	end if

	78 continue
	!call pause()

	if (full.or.loccalc) then
		write(*,*) '*************************************'
		write(*,*)'reference table, optimized 1D model'
		write(*,*) '*************************************'

		call writemodel(re, ar, 1, 1, koe)
		i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
		if (pausing) call pause()

		if (kod_local.eq.1) then
			write(*,*) '*************************************'
			write(*,*)'location in optimized 1D model'
			write(*,*) '*************************************'

			i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
			if (pausing) call pause()
		end if
	endif 
	
	if (full.or.selcalc) then
		write(*,*) '*************************************************'
		write(*,*)'SELECT DATA for itereative Tomo inversion'
		write(*,*) '*************************************************'
		i=runcommand('..\0_SELECT_DATA\select.exe')
		if (pausing) call pause()
		if (.not.full) STOP
	endif

! Execute the inversion for grids with different orientations:
	if (.not.full) niter = 1
	do iter=1,niter	
		write(it,'(i1)')iter
		call writemodel(re, ar, iter, 1, koe)

		if (full.or.loccalc) then
			write(*,*)'	 ****************************************************'
			write(*,*)'	 3D rays tracing'
			i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
			write(*,*)'	 ****************************************************'
			if (pausing) call pause()
			if (.not.full) STOP
		endif 

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
	
		if(kod_param.eq.1) then ! if grid is paramized in nodes:
			if (full.or.invcalc) then
				do igr=1,nornt
					call writemodel(re, ar, iter, igr, -1)
					if(iter.eq.1) then
						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute the ray density'
						i=runcommand('..\2_RAY_DENSITY\plotray.exe')
						write(*,*)'	 ****************************************************'
						if (pausing) call pause()

						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute the parameterization grid:'
						i=runcommand('..\3_GRID\grid.exe')
						write(*,*)'	 ****************************************************'
						if (pausing) call pause()

						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute ... :'
						i=runcommand('..\4_TETRAD\tetrad.exe')
						write(*,*)'	 ****************************************************'
						if (pausing) call pause()

						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute Matrix :'
						i=runcommand('..\5_SOSEDI\add_matr.exe')
						write(*,*)'	 ****************************************************'
						if (pausing) call pause()
					end if
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Assembly matrix:'
					i=runcommand('..\6_MATR\matr.exe')
					write(*,*)'	 ****************************************************'
					if (pausing) call pause()

					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute Inversion :'
					i=runcommand('..\7_INVERS\invbig.exe')
					write(*,*)'	 ****************************************************'
					if (pausing) call pause()
				end do

				write(*,*)'	 ****************************************************'
				write(*,*)'	 Compute final 3d model from different grids:'
				i=runcommand('..\8_3D_MODEL\mod_3d.exe')
				write(*,*)'	 ****************************************************'
				if (pausing) call pause()
			endif
		else if(kod_param.eq.2) then ! if grid is paramized in cells:
			do igr=1,nornt
				call writemodel(re, ar, iter, igr, -1)
				if(iter.eq.1) then
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the ray density'
					i=runcommand('..\2a_RAY_DENSITY\plotray.exe')
					write(*,*)'	 ****************************************************'
					if (pausing) call pause()

					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3a_GRID\block.exe')
					write(*,*)'	 ****************************************************'
					if (pausing) call pause()
				end if
				write(*,*)'	 ****************************************************'
				i=runcommand('..\6a_MATR\matr_hor.exe')
				write(*,*)'	 ****************************************************'
				if (pausing) call pause()
				write(*,*)'	 ****************************************************'
				i=runcommand('..\7a_INVERS\invers_hor.exe')
				write(*,*)'	 ****************************************************'
				if (pausing) call pause()
			end do
			write(*,*)'	 ****************************************************'
			i=runcommand('..\8a_3D_MODEL\3d_model_block.exe')
			write(*,*)'	 ****************************************************'
			if (pausing) call pause()
		end if

	end do ! Different iterations finished
end do	! Different areas finished 
stop
end

