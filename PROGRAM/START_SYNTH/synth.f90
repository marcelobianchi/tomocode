subroutine readmajor(re, ar, error, kod_local, nornt, kod_param)
	logical haskod, hasorient, hasparam, error
	integer kod_local, nornt, kod_param
	character*128 line
	character*8 re,ar

	haskod    = .FALSE.
	hasparam  = .FALSE.
	hasorient = .FALSE.
	error = .FALSE.

	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
	do i=1,10000
		read(1,'(a8)',end=511)line

		if(line.eq.'ORIENTAT') then
			read(1,*) nornt
			hasorient = .TRUE.
		end if

		if(line.eq.'SELECT_P') then
			read(1,*)
			read(1,*) kod_local
			haskod = .TRUE.
		end if

		if(line.eq.'GRID_PAR') then
			read(1,*)
			read(1,*)
			read(1,*)
			read(1,*)kod_param
			hasparam = .TRUE.
		end if
	end do
	close(1)

	511 continue

	if (.not.hasparam) then
		write(*,*)' cannot find GRID PARAMETERS in major_param.dat!!!'
		error = .TRUE.
	end if

	if (.not.hasorient) then
		write(*,*)' cannot find ORIENTATIONS in major_param.dat!!!'
		error = .TRUE.
	end if

	if (.not.haskod) then
		write(*,*)' cannot find SELECT PARAMETERS in major_param.dat!!!'
		error = .TRUE.
	end if

end subroutine readmajor

subroutine writemodel(re, ar, iter, igr)
	character*8 ar, re
	integer iter, igr

	open(11,file='../../model.dat')
	write(11,'(a8)')re
	write(11,'(a8)')ar
	write(11,'(i1)')iter
	write(11,'(i1)')igr
	write(11,'(i1)')0
	close(11)
end subroutine writemodel

subroutine writesyn(re_syn, ar_syn, re_real, ar_real)
	character*8 re_syn, ar_syn, re_real, ar_real
	open(11, file='../../kod_syn.dat')
	write(11,'(a8,1x,a8)') re_syn,ar_syn
	write(11,'(a8,1x,a8)') re_real,ar_real
	close(11)
end subroutine writesyn

function nmodels()
	integer nmodels

	nmodels = 0
	open(61, file='../../kod_syn_all.dat')
	read(61,*) nmodels
	close(61)
end function nmodels

subroutine readmodels(which, re_real, ar_real, re_syn, ar_syn, niter, kod_1d)
	character*8 ar,ar_syn,ar_real
	character*8 re,re_syn,re_real
	integer niter, kod_1d, which

	open(61, file='../../kod_syn_all.dat')
	read(61,*) num_models
	read(61,*) 
	read(61,*) 
	read(61,*) 
	do i = 1, which-1
		read(61,'(a8,3x,a8,3x,i1)')re_syn,re_real
		read(61,'(a8,3x,a8,3x,i1,3x,i1)')ar_syn,ar_real,niter,kod_1d
		read(61,*)
	end do
	close(61)
	return
end subroutine readmodels

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! -- Main Code
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!USE DFPORT
character*128 arg
character*8 ar,ar_syn,ar_real,line
character*8 re,re_syn,re_real
character*1 rg_all(100),rg
integer i
logical pausing, full, syntcalc, invcalc,loccalc
logical error

common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)

! initiallize
pausing  = .FALSE.
full     = .TRUE.
syntcalc = .FALSE.
loccalc  = .FALSE.
invcalc  = .FALSE.

! Parsing CMD line
do i = 1, iargc()
	call getarg(i, arg)
	if (arg.eq.'-synt') then
		full = .FALSE.
		syntcalc = .TRUE.
	end if
	if (arg.eq.'-loc') then
		full = .FALSE.
		loccalc = .TRUE.
	end if
	if (arg.eq.'-inv') then
		full = .FALSE.
		invcalc = .TRUE.
	end if
	if (arg.eq.'-pausing'.or.arg.eq.'-pause') then
		pausing = .TRUE.
	end if
end do

write(*,'(a)') 'Running:'
write(*,'(a,l)') ' Full =',full
write(*,'(a,l)') ' Synt =',syntcalc
write(*,'(a,l)') ' Loca =',loccalc
write(*,'(a,l)') ' Inve =',invcalc

! Prepare
num_models = nmodels()
if ((num_models.ne.1).and.(.not.full)) then
	write(*,*) 'Cannot run part of code with more than one model'
	stop
end if

do imodel=1,num_models
	call readmodels(i, re_real, ar_real, re_syn, ar_syn, niter, kod_1d)
	write(*,'(a12,a1,a8,a1,a8)')' input:',' ',re_real,'/',ar_real
	write(*,'(a12,a1,a8,a1,a8)')' output:',' ',re_syn,'/',ar_syn
	write(*,'(a12,i3,a,i3)')' iterations:',niter,' kod_1d:',kod_1d

	! Prepare folders
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\3D_MODEL')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\GRIDS')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\RESULT')
	call createdir('..\..\DATA\'//re_syn//'\'//ar_syn//'\TIMES')
	if (pausing) call pause()

	! Write KodSyn
	call writesyn(re_syn, ar_syn, re_real, ar_real)

	ar=ar_syn
	re=re_syn

	call readmajor(re, ar, error, kod_local, nornt, kod_param)
	if (error) stop

	! Reference table 1D
	if (full.or.syntcalc) then
		call copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_syn.dat','..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
		if (pausing) call pause()

		call writemodel(re, ar, 0, 0)

		write(*,*) '*************************************'
		write(*,*) 'reference table, synthetic 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
		if (pausing) call pause()

		write(*,*) '*************************************'
		write(*,*) 'Compute the synthetic times: '
		write(*,*) '*************************************'

		i=runcommand('..\B_SYNTH_TIMES\rays.exe')
		if (pausing) call pause()
	end if

	call copyfile('..\..\DATA\'//re//'\'//ar//'\INI_PARAM\ref_start.dat', '..\..\DATA\'//re//'\'//ar//'\INI_PARAM\refmod.dat')
	if (pausing) call pause()

	if (full.or.loccalc) then
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

			call writemodel(re, ar, 0, 0)
			i=runcommand('..\..\1D_MODEL\START_1D\start_real.exe')
			if (pausing) call pause()
		end if

		call writemodel(re, ar, 1, 1)
		write(*,*) '*************************************'
		write(*,*) 'reference table, final 1D model'
		write(*,*) '*************************************'

		i=runcommand('..\..\LIN_PROG\0_REF_RAYS\refrays.exe')
		if (pausing) call pause()

		if (kod_local.eq.1) then
			write(*,*) '*************************************'
			write(*,*) 'location in optimized 1D model'
			write(*,*) '*************************************'

			i=runcommand('..\..\LIN_PROG\1_LOC_EVENT\locate.exe')
			if (pausing) call pause()
		end if
	end if

	! Execute the inversion for grids with different orientations:
	if (.not.full) niter = 1
	do iter=1,niter
		write(*,*)
		write(*,*)'************************************************************'
		write(*,*)' model:',ar,' iteration:',iter
		write(*,*)'************************************************************'
		call writemodel(re, ar, iter, 1)

		if (full.or.loccalc) then
			write(*,*)'****************************************************'
			write(*,*)'Source location in 3d'
			write(*,*)'****************************************************'
			i=runcommand('..\1_3D_LOCATE\3d_locate.exe')
			if (pausing) call pause()
		end if

		if(kod_param.eq.1) then
			if (full.or.invcalc) then
				do igr=1,nornt
					call writemodel(re, ar, iter, igr)
					if(iter.eq.1) then
						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute the ray density'
						i=runcommand('..\2_RAY_DENSITY\plotray.exe')
						if (pausing) call pause()
						write(*,*)'	 ****************************************************'
						write(*,*)'	 Compute the parameterization grid:'
						i=runcommand('..\3_GRID\grid.exe')
						if (pausing) call pause()
						i=runcommand('..\4_TETRAD\tetrad.exe')
						if (pausing) call pause()
						i=runcommand('..\5_SOSEDI\add_matr.exe')
						if (pausing) call pause()
					end if

					i=runcommand('..\6_MATR\matr.exe')
					if (pausing) call pause()
					i=runcommand('..\7_INVERS\invbig.exe')
					if (pausing) call pause()
				end do ! Different grids
				i=runcommand('..\8_3D_MODEL\mod_3d.exe')
				if (pausing) call pause()
			end if

		else if(kod_param.eq.2) then
			do igr=1,nornt
				call writemodel(re, ar, iter, igr)
				if(iter.eq.1) then

					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the ray density'
					i=runcommand('..\2a_RAY_DENSITY\plotray.exe')
					if (pausing) call pause()
					write(*,*)'	 ****************************************************'
					write(*,*)'	 Compute the parameterization grid:'
					i=runcommand('..\3a_GRID\block.exe')
					if (pausing) call pause()
				end if
				i=runcommand('..\6a_MATR\matr_hor.exe')
				if (pausing) call pause()
				i=runcommand('..\7a_INVERS\invers_hor.exe')
				if (pausing) call pause()
			end do ! Different grids
			i=runcommand('..\8a_3D_MODEL\3d_model_block.exe')
			if (pausing) call pause()
		end if

	end do ! Different iterations
end do ! Different Models

stop
end
