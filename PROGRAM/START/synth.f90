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
full     = .FALSE.
syntcalc = .FALSE.
loccalc  = .FALSE.
invcalc  = .FALSE.

! Parsing CMD line
do i = 1, iargc()
	call getarg(i, arg)
	if (arg.eq.'-synt') then
		syntcalc = .TRUE.
		if (full.or.loccalc.or.invcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-loc') then
		loccalc = .TRUE.
		if (full.or.syntcalc.or.invcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-inv') then
		invcalc = .TRUE.
		if (full.or.loccalc.or.syntcalc) then
			write (*,*) 'Only one option is allowed'
			stop
		end if
	end if
	if (arg.eq.'-full') then
		full = .TRUE.
		if (syntcalc.or.loccalc.or.invcalc) then
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
write(*,'(a,l)') ' Synt =',syntcalc
write(*,'(a,l)') ' Loca =',loccalc
write(*,'(a,l)') ' Inve =',invcalc

if (.not.(full.or.syntcalc.or.invcalc.or.loccalc)) then
	write (*,*)
	write (*,*) 'Bad cmd line. Usage:'
	write (*,*) '    start_synth -full|-synt|-loc|-inv [-pausing]'
	stop
end if

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
