! This program visualize the rays corresponding to current calculations
! (from 'tmp' directory)
! Output:
! FIG_files/rays/rays_ver.dat : projection of rays in vertical section (as points)
! FIG_files/rays/ztr_ver.dat : projection of event coordinates in vertical section
! FIG_files/rays/rays_hor.dat : rays in a defined layer, z_up - z_low (as points)
! FIG_files/rays/ztr_hor.dat : sources in map view


character*8 ar,re,line
character*1 rm,it
real fmark(100),tmark(100)
real hmod(600),vmodp(600),vmods(600) ! ref. models in different zones

common/stations/ xst(9000),yst(9000),zst(9000)
common /ray_path/npray,xray(5000),yray(5000),zray(5000)
common/refmod/nrefmod,hmod,vmodp,vmods
common/grid/zgrmax,dzlay,dsmin
common/pi/pi,per

one=1
pi=asin(one)*2.e0
per=pi/180.e0
rz=6371.


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
close(1)
write(*,*)' re=',re,' ar=',ar

open(1,file='SET.DAT')
read(1,*)iiips		! P or S
read(1,*)iter		! iteration
read(1,*)fia,teta
read(1,*)fib,tetb
read(1,*)interval
read(1,*)n_freq_point
read(1,*)z_up,z_low
read(1,*)y_prof
close(1)
write(*,*)' iiips=',iiips
write(*,*)' iter=',iter
write(*,*)' fia=',fia
write(*,*)' teta=',teta
write(*,*)' fib=',fib
write(*,*)' tetb=',tetb
write(*,*)' interval=',interval
write(*,*)' n_freq_point=',n_freq_point
write(*,*)' z_up=',z_up
write(*,*)' z_low=', z_low
write(*,*)' y_prof=',y_prof
write(it,'(i1)')iter

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
write(*,*)fi0,tet0
close(1)

call SFDEC(fia,teta,0.,xa,ya,Z,fi0,tet0)
call SFDEC(fib,tetb,0.,xb,yb,Z,fi0,tet0)

dist=sqrt((xb-xa)*(xb-xa)+(yb-ya)*(yb-ya))
write(*,*)' dist=',dist
sinpov=(yb-ya)/dist
cospov=(xb-xa)/dist
!write(*,*)' sinpov=',sinpov,' cospov=',cospov

open(11,file='../../FIG_FILES/RAYS/mark.dat')
imark=0
dsmark=100.
do sss=0.,dist,dsmark
	x=xa+cospov*sss
	y=ya+sinpov*sss
	call decsf(x,y,0.,fi0,tet0,FI,TET,h)
	write(11,*)fi,tet,sss
	imark=imark+1
	fmark(imark)=fi
	tmark(imark)=tet
end do
close(11)

! Draw the position of the section on the surface (line)
open(11,file='../../FIG_FILES/RAYS/mark.bln')
write(11,*) imark+1
do i=1,imark
	write(11,*)fmark(i),tmark(i)
end do
call decsf(xb,yb,0.,fi0,tet0,FI,TET,h)
write(11,*)fi,tet,dist
close(11)


! Read the coordinates of the stations
open(2,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
open(12,file='../../FIG_FILES/RAYS/stat_ver.dat')
i=0
3	i=i+1
	read(2,*,end=4)xst(i),yst(i),zst(i)
	xx1=(xst(i)-xa)*cospov+(yst(i)-ya)*sinpov
	yy1=-(xst(i)-xa)*sinpov+(yst(i)-ya)*cospov
	if(abs(yy1).lt.y_prof)write(12,*)xx1,2
	goto 3
4	close(2)
nst=i-1
write(*,*)' nst=',nst
close(12)

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='binary')
open(2,file='../../TMP/ray_paths_p_'//it//'.dat',form='binary')

open(11,file='../../FIG_FILES/RAYS/rays_ver.dat')
open(12,file='../../FIG_FILES/RAYS/ztr_ver.dat')
open(16,file='../../FIG_FILES/RAYS/rays_hor.dat')
open(14,file='../../FIG_FILES/RAYS/ztr_hor.dat')




izt=0
nkrmax=0
nray=0
nrr=0
! Read the sources:
872	read(1,end=871)xzt,yzt,zzt,nkrat
	ind=1
	xx1=(xzt-xa)*cospov+(yzt-ya)*sinpov
	yy1=-(xzt-xa)*sinpov+(yzt-ya)*cospov
	if(abs(yy1).lt.y_prof) write(12,*)xx1,-zzt,abs(yy1)
	call decsf(xzt,yzt,0.,fi0,tet0,FI,TET,h)
	write(14,*)fi,tet,zzt
	nnn=0

	do ikr=1,nkrat


		read(1)ist,ips,tobs,tmod
		read(2)npray
		if(npray.eq.0)cycle
		nrr=nrr+1
		do i=1,npray
			read(2)xray(i),yray(i),zray(i)
                       ! write(*,*)' xray=',xray(i), ' yray=',yray(i),' zray=',zray(i)
		end do

		if(ips.ne.iiips) cycle

		if(mod(nrr,interval).eq.0) then
			do i=1,npray
				nnn=nnn+1
				if(mod(nnn+nray,n_freq_point).ne.0) cycle
				call decsf(xray(i),yray(i),0.,fi0,tet0,FI,TET,h)
				if((zray(i)-z_up)*(zray(i)-z_low).lt.0.) write(16,*)fi,tet
			end do
		end if

		!if(abs(yy1).gt.91) cycle

		

		!write(*,*)' npray=',npray
		!write(*,*)' xst=',xstat,' yst=',ystat
		!write(*,*)' xzt=',xzt,' yzt=',yzt,' zzt=',zzt
		do ip=1,npray
			nnn=nnn+1
			if(mod(nnn,n_freq_point).ne.0) cycle
			x=xray(ip)
			y=yray(ip)
			z=zray(ip)
			!write(*,*)x,y,z
			if ((x-xa)*(x-xb).gt.0) cycle
			if ((y-ya)*(y-yb).gt.0) cycle
			xx=(x-xa)*cospov+(y-ya)*sinpov
			yy=-(x-xa)*sinpov+(y-ya)*cospov
			if(abs(yy).lt.y_prof)write(11,*)xx,-z
		end do
		nray=nray+1

	end do
	goto 872
871	continue
close(1)
close(11)
close(12)
close(14)
close(16)

write(*,*)' nray=',nray


stop
end
