subroutine read_z_lim(re,ar)
character*8 ar,re
common/zlimit/zbase,nar,npar(5),zmar(5),flim(5,100),tlim(5,100)

nar=0
zbase=1000
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/z_limit.bln')
read(1,*,end=1)zbase
2 read(1,*,end=1)np,zmax
!write(*,*)' np=',np,' zmax=',zmax
nar=nar+1
npar(nar)=np
zmar(nar)=zmax
do i=1,np
	read(1,*) flim(nar,i), tlim(nar,i)
end do
goto 2
1 close(1)

!write(*,*)' nar=',nar


return
end