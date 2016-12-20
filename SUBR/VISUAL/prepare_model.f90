subroutine prepare_model(ar,ips,iter,igr)

character*1 gr,ps,nit
character*8 ar

common/pi/pi,per
common/center/fi0,tet0
common/grid/nornt,ornt(4),sinal,cosal,&
		nlev,hlev(30),ntop(30),nobr,&
		xtop(10000,30), ytop(10000,30),n_pop(10000,30),&
		dv_mod(10000)
write(ps,'(i1)')ips

write(nit,'(i1)')iter
write(gr,'(i1)')igr


open(1,file='../../DATA/center.dat')
read(1,*)fi0,tet0
close(1)


open(1,file='../../DATA/'//ar//'/INI_PARAM/par_grid.dat')
read(1,*)nornt
read(1,*)(ornt(i),i=1,nornt)
close(1)

!write(*,*)' fi0=',fi0,' tet0=',tet0
orient=ornt(igr)
sinal=sin(orient*per)
cosal=cos(orient*per)

open(1,file='../../DATA/'//ar//'/grid/levinfo'//ps//gr//'.dat')
i=0
722 i=i+1
	read(1,*,end=721)np,hlev(i)
	!write(*,*)np,hlev(i)
	goto 722
721 nlev=i-1
!write(*,*)' nlev=',nlev
close(1)


ntop=0
open(1,file='../../DATA/'//ar//'/grid/topn'//ps//gr//'.dat')

do n=1,nlev
	read(1,*,end=556)ntop(n)
!	write(*,*)' ntop=',ntop(n)
	read(1,*)(xtop(i,n),ytop(i,n),n_pop(i,n),i=1,ntop(n))
end do
556		close(1)

!open(21,file='grid2tmp.dat')
!do i=1,ntop(2)
!	x=xtop(i,2)
!	y=ytop(i,2)
!	zz=0
!	ind=0
!	call decsf(x,y,zz,ind,fi0,tet0,FI,TET,h)
!	write(21,*)fi,tet
!end do
!close(21)
!call pause()

open(1,file='../../DATA/'//ar//'/grid/obr'//ps//gr//'.dat')
read(1,*) nobr
if(nobr.gt.10000) then
	write(*,*)' nobr=',nobr
	write(*,*)' One should increase size of dv_mod'
	call pause()
end if
close(1)

open(1,file='../../DATA/'//ar//'/RESULT/dv'//ps//gr//'.dat')
do i=1,nobr
	read(1,*)  dv_mod(i)
end do
close(1)

return
end

