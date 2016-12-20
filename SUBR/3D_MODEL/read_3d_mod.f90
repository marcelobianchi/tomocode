subroutine read_3D_mod(ar,iter)
character*8 ar
character*1 ps,rm,it 
common/mod_3D/dv_3D(2,70,70,10),xx1,nxx,dxx,yy1,nyy,dyy,nzz,hlev(10)
common/zlimits/zmin,zmax
nxmax=70
nymax=70
nzmax=10

nxx=0
nyy=0
nzz=0

if(iter.lt.1) return

write(it,'(i1)')iter

open(1,file='../../DATA/'//ar//'/INI_PARAM/par_grid.dat')
do i=1,7
	read(1,*)
end do
read(1,*)zmin,zmax
close(1)


open(1,file='../../DATA/'//ar//'/3D_MODEL/dv'//it//'.dat',form='unformatted')
read(1)nzz
if(nzz.gt.nzmax) then
	write(*,*)' nzz > nzmax!'
	write(*,*)' Value of the nzz in common file "mod_3D" should be increased'
	call pause()
end if
read(1)(hlev(i),i=1,nzz)
read(1)xx1,nxx,dxx
if(nxx.gt.nxmax) then
	write(*,*)' nxx > nxmax!'
	write(*,*)' Value of the nxx in common file "mod_3D" should be increased'
	call pause()
end if
read(1)yy1,nyy,dyy
if(nyy.gt.nymax) then
	write(*,*)' nyy > nymax!'
	write(*,*)' Value of the nyy in common file "mod_3D" should be increased'
	call pause()
end if
do ips=1,2
	do ilev=1,nzz
		read(1)((dv_3D(ips,ixx,iyy,ilev),ixx=1,nxx),iyy=1,nyy)
	end do
end do
close(1)

!write(*,*)' dv_3D(1,36,37,1)=',dv_3D(1,36,37,1)

write(*,*)' nxx=',nxx,' nyy=',nyy,' nzz=',nzz


return
end
