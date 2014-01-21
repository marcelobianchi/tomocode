program Visual_synini_3d
character*4 dsaa/'DSAA'/
character*8 ar,re,line
character*1 ps
!character*2 lv
!real hlev(100)

allocatable vini_3d(:,:,:),vref_syn(:,:,:)
common/center/fi0,tet0
common/pi/pi,per
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0


open(1,file='../../VISUAL/area.dat')
read(1,'(a8)')re
read(1,'(a8)')ar
825 close(1)

write(*,*)' region : ',re,' AREA : ',ar,' iter=',iter


!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
pause
554 read(1,*)fi0,tet0
close(1)

!******************************************************************

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/anomaly.dat')
read(1,*) n_anomaly
close(1)
write(*,*)' ar=',ar,'   kod of anom.=',n_anomaly

!******************************************************************
! Read the values of the reference model
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
write(*,*)' nrefmod=',nrefmod



!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=563)line
	if(line.eq.'3D_MODEL') goto 564
end do
563 continue
write(*,*)' cannot find 3D_MODEL in major_param.dat!!!'
pause
564 continue
read(1,*) xx1,xx2,dxx 
read(1,*) yy1,yy2,dyy 
read(1,*) zz1,zz2,dzz  
read(1,*) smaxx
read(1,*) ismth
close(1)
rsmth=ismth+0.5
nxx=(xx2-xx1)/dxx+1
nyy=(yy2-yy1)/dyy+1
nzz=(zz2-zz1)/dzz+1
write(*,*)' nxx=',nxx,' nyy=',nyy,' nzz=',nzz


allocate(vini_3d(nxx,nyy,nzz),vref_syn(nxx,nyy,nzz))


if(n_anomaly.eq.1)then
	call prep_board_dv(re,ar)
else if(n_anomaly.eq.2)then
	call read_hor_an (re,ar)
else if(n_anomaly.eq.3)then
	call read_vert_an(re,ar)
else if(n_anomaly.eq.4)then
	call read_vert_brd(re,ar)
end if


do ips=1,2
	write(ps,'(i1)')ips
		vini_3d=0
		vref_syn=0

	DO izz=1,nzz
		zzz=(izz-1)*dzz+zz1
		v0 = vrefmod(zzz,ips)
		if(mod(izz,5).eq.0) write(*,*)' izz=',izz,' zzz=',zzz

		do iyy=1,nyy
			yyy=(iyy-1)*dyy+yy1
			do ixx=1,nxx
				xxx=(ixx-1)*dxx+xx1
				if(n_anomaly.eq.1)then
					dv = dv_board(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.2)then
					dv = hor_anom(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.3)then
					dv=vert_anom(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.4)then
					dv=vert_brd(xxx,yyy,zzz,ips)
				end if
				vini_3d(ixx,iyy,izz)=dv
				vref_syn(ixx,iyy,izz)=v0
			end do
		end do

	end DO
	
	open(11,file='../../FIG_FILES/3D_MODEL/vref_syn_3d_'//ps//'.dat',form='binary')
	write(11)xx1,nxx,dxx
	write(11)yy1,nyy,dyy
	write(11)zz1,nzz,dzz
	!write(*,*)' nx=',nxx,' ny=',nyy,' nz=',nzz
	do izz=1,nzz
		write(11)((vref_syn(ixx,iyy,izz),ixx=1,nxx),iyy=1,nyy)
	end do
	close(11)
	
	open(11,file='../../FIG_FILES/3D_MODEL/vsyn_3d_'//ps//'.dat',form='binary')
	write(11)xx1,nxx,dxx
	write(11)yy1,nyy,dyy
	write(11)zz1,nzz,dzz
	!write(*,*)' nx=',nxx,' ny=',nyy,' nz=',nzz
	do izz=1,nzz
		write(11)((vini_3d(ixx,iyy,izz),ixx=1,nxx),iyy=1,nyy)
	end do
	close(11)

	!open(11,file='../../FIG_FILES/3D_MODEL/vsyn_3d_'//ps//'OD.dat',form='binary')
        !do iyy=1,nyy
        ! do ixx=1,nxx
   	!   do izz=1,nzz
	!	write(11) vini_3d(ixx,iyy,izz)
  	!   end do
	! end do
	!end do
	!close(11)

	!open(11,file='../../FIG_FILES/3D_MODEL/vref_syn_3d_'//ps//'OD.dat',form='binary')
        !do iyy=1,nyy
        ! do ixx=1,nxx
   	!   do izz=1,nzz
	!	write(11) vref_syn(ixx,iyy,izz)
  	!   end do
	! end do
	!end do
	1close(11)


end do

stop
end
