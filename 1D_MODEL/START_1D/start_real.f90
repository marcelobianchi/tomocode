!USE DFPORT
character*8 ar,re,line
character*1 rg_all(100),rg,it
integer kod_loc(10),kod_iter(10),kod_oe(10)

pausing=0

open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
close(1)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/1Dmod_PARAM.DAT')
read(1,*)niter
close(1)
write(*,*)' number of iterations:',niter

i=system('../../1D_MODEL/1_SELECT/select.exe')
if (pausing.EQ.1) call pause()

!******************************************************************
	
do iter=1,niter	

	write(it,'(i1)')iter

	open(11,file='../../model.dat')
	write(11,'(a8)')re		
	write(11,'(a8)')ar		
	write(11,'(i1)')iter		
	close(11)

	write(*,*)'	 ****************************************************'
	write(*,*)'	 Reference table:'
	i=system('../../1D_MODEL/2_REFTABLE/refrays.exe')
        if (pausing.EQ.1) call pause()

	write(*,*)'	 ****************************************************'
	write(*,*)'	 Source location:'
	i=system('../../1D_MODEL/3_LOCATE/locate.exe')
        if (pausing.EQ.1) call pause()

	write(*,*)'	 ****************************************************'
	write(*,*)'	 Matrix calculation:'
	i=system('../../1D_MODEL/4_MATR/matr.exe')
        if (pausing.EQ.1) call pause()

	write(*,*)'	 ****************************************************'
	write(*,*)'	 Inversion:'
	i=system('../../1D_MODEL/5_INVERS/invers.exe')
        if (pausing.EQ.1) call pause()

end do ! Different iterations
write(it,'(i1)')niter

open(1,file='../../DATA/'//re//'/'//ar//'/RESULT/ref'//it//'.dat')
open(11,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/refmod.dat')
read(1,*)
write(11,*)0
nref=0
182	read(1,*,end=181)h,vp,vs
	write(11,*)h,vp,vs
	nref=nref+1
goto 182
181	close(1)
close(11)
write(*,*)' nref=',nref


stop
end