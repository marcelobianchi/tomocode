character*8 ar,re,line
character*1 it,ppss,gr
real ornt(20),yprof(10,20)
real xray4(10000),yray4(10000),zray4(10000),tray4(10000)
real plotray(:,:,:,:),dsy(:,:),deriv4(3,200)
integer nprof(10),istkr(200),ipskr(200),nray(2)


allocatable plotray,dsy

one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0

open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter		! code of the grid
read(1,*)igr		! code of the grid
close(1)
write(it,'(i1)')iter
write(gr,'(i1)')igr

write(*,*)' Computing ray density'
write(*,*)' re=',re,' ar=',ar,' it=',it,' gr=',gr

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
call pause()
554 read(1,*)fi0,tet0
write(*,*)fi0,tet0
close(1)

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=573)line
	if(line.eq.'ORIENTAT') goto 574
end do
573 continue
write(*,*)' cannot find ORIENTATIONS in major_param.dat!!!'
call pause()
574 read(1,*)nornt
read(1,*)(ornt(i),i=1,nornt)
close(1)


!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=533)line
	if(line.eq.'GRID_PAR') goto 534
end do
533 continue
write(*,*)' cannot find GRID_PARAMETERS in major_param.dat!!!'
call pause()
534 continue
read(1,*)xlim1,xlim2,dxpl
read(1,*)ylim1,ylim2,dypl
read(1,*)zlim1,zlim2,dzpl
close(1)
!******************************************************************


orient=ornt(igr)
write(*,*)' orient=',orient
sinbase=sin(orient*per)
cosbase=cos(orient*per)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/grid_param.dat')
close(1)

nxpl=int_best((xlim2-xlim1)/dxpl)
nypl=int_best((ylim2-ylim1)/dypl)
nzpl=int_best((zlim2-zlim1)/dzpl)

write(*,*)' nx=',nxpl,' ny=',nypl,' nz=',nzpl

allocate(plotray(2,nxpl,nypl,nzpl),dsy(2,nypl))

plotray=0.
dsy=0.
nray=0

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='unformatted')
open(2,file='../../TMP/ray_paths_p_'//it//'.dat',form='unformatted')

nzt=0

728 continue
	read(1,end=729)xzt,yzt,zzt,nkrat,kod_local
	nzt=nzt+1

	do ikr=1,nkrat
		read(1,end=729)ist,ips,tobs,tmod
		read(2)npray
		if(npray.eq.0)cycle
		do i=1,npray
			read(2)xray4(i),yray4(i),zray4(i)
		end do
!if(nzt.lt.199) cycle
!write(*,*)ikr,ips,npray,nkrat
!if(ikr.lt.71) cycle
		res=tobs-tmod
		if(abs(res).gt.10) cycle
		nray(ips)=nray(ips)+1

		do ip=2,npray
			xl1=xray4(ip-1)
			xl2=xray4(ip)
			yl1=yray4(ip-1)
			yl2=yray4(ip)
			zl1=zray4(ip-1)
			zl2=zray4(ip)
			dss=sqrt((xl1-xl2)*(xl1-xl2)+(yl1-yl2)*(yl1-yl2)+(zl1-zl2)*(zl1-zl2))

			xmid=(xl1+xl2)/2.
			ymid=(yl1+yl2)/2.
			zmid=(zl1+zl2)/2.
			!write(*,*)ip,xmid,ymid,zmid

			xxx=xmid*cosbase+ymid*sinbase
			yyy=-xmid*sinbase+ymid*cosbase
			xmid=xxx
			ymid=yyy

			if((xmid-xlim1)*(xmid-xlim2).gt.0) cycle
			if((ymid-(ylim1+0.5*dypl))*(ymid-(ylim2-0.5*dypl)).gt.0) cycle
			if((zmid-zlim1)*(zmid-zlim2).gt.0) cycle

!xmid=0
!ymid=0
!zmid=60

			ilevy=0
			do iy=1,nypl
				y1=ylim1+(iy-0.5)*dypl
				y2=ylim1+(iy+0.5)*dypl
				!write(*,*)' y1=',y1,' y2=',y2
				if((ymid-y1)*(ymid-y2).le.0.)exit
			end do

			if(iy.eq.0.or.iy.gt.nypl) cycle

			do ix=1,nxpl
				x1=xlim1+dxpl*(ix-1)
				x2=xlim1+dxpl*ix
				if((xmid-x1)*(xmid-x2).le.0.)exit
			end do
			if(ix.eq.0.or.ix.gt.nxpl) cycle


			do iz=1,nzpl
				z1=zlim1+dzpl*(iz-1)
				z2=zlim1+dzpl*iz
				if((zmid-z1)*(zmid-z2).le.0.)exit
			end do

			if(iz.eq.0.or.iz.gt.nzpl) cycle



			plotray(ips,ix,iy,iz)=plotray(ips,ix,iy,iz)+dss
			dsy(ips,iy)=dsy(ips,iy)+dss

		end do
	end do
	if(mod(nzt,100).eq.0) then
		write(*,*)' nzt=',nzt,' nray=',nray(1),nray(2)
	end if
	goto 728
729 continue
close(1)
close(2)
write(*,*)' total number of rays:',nray(1),nray(2)




open(15,file='../../DATA/'//re//'/'//ar//'/TIMES/numray'//it//'.dat')
write(15,*) nray(1),nray(2)
close(15)

!do i=1,nypl
	!if(dsy(i).eq.0.)cycle
	!write(*,*)' iy=',i,' dsy=',dsy(i)
!end do

do iiips=1,2
	if(nray(iiips).eq.0) cycle
	write(ppss,'(i1)')iiips
	open(12,file='../../TMP/plotray'//ppss//gr//'.dat',form='unformatted')
	do iy=1,nypl
		write(12)((plotray(iiips,ix,iy,iz),ix=1,nxpl),iz=1,nzpl)
	end do
end do

stop
end

