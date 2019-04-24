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

subroutine writemodel(re, ar, iter, igr, koe)
	character*8 ar, re
	integer iter, igr

	open(11,file='../../model.dat')
	write(11,'(a8)') re
	write(11,'(a8)') ar
	write(11,'(i1)') iter
	write(11,'(i1)') igr
	write(11,'(i1)') koe
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

