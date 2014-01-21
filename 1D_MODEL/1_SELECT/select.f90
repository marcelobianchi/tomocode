character*8 ar,re,line
integer nkr_z(1000,20),kzt_z(1000,20),nmx_z(1000)


common/krat/nkrat,istkr(500),tobkr(500),ipskr(500),qualkr(500),trfkr(500),ngood(500),alkr(500),diskr(500)


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
write(*,*)' area=',re,' model=',ar

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/1Dmod_PARAM.DAT')
read(1,*)
read(1,*)zmin,zztmax,dzstep,nzmax
close(1)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
pause
554 read(1,*)fi0,tet0
write(*,*)fi0,tet0
close(1)


!dist_max=10
!ff=30.2
!tt=0.8
!call sfdec(ff,tt,0,xxx,yyy,zzz,fi0,tet0)
!write(*,*)xxx,yyy

nzt=0
nray=0
nkr_z=0
kzt_z=0
nmx_z=0
open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_loc0.dat',form='binary')
1	read(1,end=2)xzt,yzt,zzt,nkrat
	nzt=nzt+1
	do i=1,nkrat
		read(1)istkr(i),ipskr(i),tobkr(i),trfkr(i)
	end do
!	dist=sqrt((xzt-xxx)**2+(yzt-yyy)**2)
	!if (dist.gt.dist_max) goto 1
	!write(*,*)xzt,yzt,zzt,nkrat
	if (zzt.gt.zztmax) goto 1
	do iz=1,1000
		z1=zmin+(iz-1)*dzstep
		z2=zmin+iz*dzstep
		if((zzt-z1)*(zzt-z2).le.0.)exit
	end do
	!write(*,*)' iz=',iz,' z1=',z1,'z2=',z2,' nkrat=',nkrat
	if(nmx_z(iz).lt.nzmax) then
		nmx_z(iz)=nmx_z(iz)+1
		kzt_z(iz,nmx_z(iz))=nzt
		nkr_z(iz,nmx_z(iz))=nkrat
	else
		nmin=9999
		do imx=1,nzmax
			if(nkr_z(iz,imx).ge.nmin) cycle
			nmin=nkr_z(iz,imx)
			imin=imx
		end do 
		if(nmin.lt.nkrat) then
			kzt_z(iz,imin)=nzt
			nkr_z(iz,imin)=nkrat
		end if
	end if

	!pause
	nray=nray+nkrat
	goto 1
2 close(1)
write(*,*)' nzt=',nzt,' nray=',nray

do iz=1,1000
	if(nmx_z(iz).eq.0) cycle
	z1=zmin+(iz-1)*dzstep
	z2=zmin+iz*dzstep
	write(*,'(f6.1,10i4)')z1,(nkr_z(iz,im),im=1,nmx_z(iz))
end do



nzt=0
izt=0
nray=0
open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_loc0.dat',form='binary')
open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/rays0.dat',form='binary')
open(12,file='../../fig_files/1dmod/'//re//ar//'_best_events.dat')
11	read(1,end=12)xzt,yzt,zzt,nkrat
	izt=izt+1
	do i=1,nkrat
		read(1)istkr(i),ipskr(i),tobkr(i),trfkr(i)
	end do

	do iz=1,1000
		if(nmx_z(iz).eq.0) cycle
		do im=1,nmx_z(iz)
			if(kzt_z(iz,im).eq.izt) goto 14
		end do
	end do
	goto 11
14	continue
	

	nzt=nzt+1
	if(mod(nzt,10).eq.0) write(*,*)xzt,yzt,zzt,nkrat
	call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,h)
	write(12,*)fzt,tzt,zzt,nkrat
	nray=nray+nkrat
	write(11)xzt,yzt,zzt,nkrat
	do i=1,nkrat
		write(11)istkr(i),ipskr(i),tobkr(i),trfkr(i)
	end do
	goto 11
12 close(1)
close(11)
write(*,*)' nzt=',nzt,' nray=',nray

stop
end
