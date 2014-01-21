subroutine read_hor_an(re,ar)
character*8 ar,re
character*5 fig
character*2 lv
real xcur(9000),ycur(9000)

common/center/fi0,tet0
common/pi/pi,per
common/dv_hor_an/nan,anom(40,4000,2),val_p(40),val_s(40),zan1(40),zan2(40),nnod(40)
common/levels/nlev,hlev(100)

iscale=0
open(2,file='../../DATA/'//re//'/'//ar//'/FORMS/scaling.dat')
read(2,*,end=392)fmap1,fmap2
read(2,*)tmap1,tmap2
read(2,*)rotate
iscale=1
392 close(2)

sina=sin(rotate*per)
cosa=cos(rotate*per)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/anomaly.dat')
read(1,*) 
read(1,*) 
read(1,*) nform

do iform=1,nform
	read(1,*) 
	read(1,*) fig
	read(1,*) xpmin,xpmax,ypmin,ypmax
	read(1,*) val_p(iform),val_s(iform)
	read(1,*) zan1(iform),zan2(iform)

	open(2,file='../../DATA/'//re//'/'//ar//'/FORMS/'//fig//'.bln')
	read(2,*) ncurve
	xmin=10000
	xmax=-10000
	ymin=10000
	ymax=-10000
	do i=1,ncurve
		read(2,*)xcur(i),ycur(i)
		if(xcur(i).lt.xmin) xmin=xcur(i)
		if(xcur(i).gt.xmax) xmax=xcur(i)
		if(ycur(i).lt.ymin) ymin=ycur(i)
		if(ycur(i).gt.ymax) ymax=ycur(i)
		write(*,*)' xcur=',xcur(i),' ycur=',ycur(i)
	end do
	close(2)
	write(*,*)' xmin=',xmin,' xmax=',xmax
	write(*,*)' ymin=',ymin,' ymax=',ymax
	write(*,*)' zmin=',zmin,' zmax=',zmax
	!pause

	dfdx=(xpmax-xpmin)/(xmax-xmin)
	dtdy=(ypmax-ypmin)/(ymax-ymin)
	if(abs(xpmin-xpmax).lt.0.000001) then
		do i=1,ncurve
			anom(iform,i,1)=xcur(i)
			anom(iform,i,2)=ycur(i)
!			write(*,*),"loop1"
!			write(*,*)anom(iform,i,1),anom(iform,i,2)
		end do
		nnod(iform)=ncurve+1
		anom(iform,nnod(iform),1)=xcur(1)
		anom(iform,nnod(iform),2)=ycur(1)
	else
		do i=1,ncurve
!			anom(iform,i,1)=xpmin+dfdx*(xcur(i)-xmin)
!			anom(iform,i,2)=ypmin+dtdy*(ycur(i)-ymin)
			anom(iform,i,1)=xcur(i)
			anom(iform,i,2)=ycur(i)
		end do
		nnod(iform)=ncurve+1
!		anom(iform,nnod(iform),1)=xpmin+dfdx*(xcur(1)-xmin)
!		anom(iform,nnod(iform),2)=ypmin+dtdy*(ycur(1)-ymin)
		anom(iform,nnod(iform),1)=xcur(1)
		anom(iform,nnod(iform),2)=ycur(1)
		write(*,*),"loop2"
	end if

end do
! rotates, and adjusts extreme values of anomaly if neccessary:
if(iscale.eq.1) then
	xmin=99999
	xmax=-99999
	ymin=99999
	ymax=-99999
	do iform=1,nform
		write(*,*)' nnod=',nnod(iform)
		do i=1,nnod(iform)
			x1=anom(iform,i,1)
			y1=anom(iform,i,2)
			x2=x1*cosa-y1*sina
			y2=x1*sina+y1*cosa
			anom(iform,i,1)=x2
			anom(iform,i,2)=y2
!			write(*,*),"newx",anom(iform,i,1)
!			write(*,*),"newy",anom(iform,i,2)
			if(x2.lt.xmin) xmin=x2
			if(x2.gt.xmax) xmax=x2
			if(y2.lt.ymin) ymin=y2
			if(y2.gt.ymax) ymax=y2
		end do
	end do
!	write(*,*)' xmax=',xmax,' xmin=',xmin
!	write(*,*)' ymax=',ymax,' ymin=',ymin

!	do iform=1,nform
!		write(*,*)' nnod=',nnod(iform)
!		do i=1,nnod(iform)
!			x1=anom(iform,i,1)
!			y1=anom(iform,i,2)
!			write(*,*),"oldx",anom(iform,i,1)
!			write(*,*),"oldy",anom(iform,i,2)
!			x2=fmap1+((fmap2-fmap1)/(xmax-xmin))*(x1-xmin)
!			y2=tmap1+((tmap2-tmap1)/(ymax-ymin))*(y1-ymin)
!
!			anom(iform,i,1)=x2
!			anom(iform,i,2)=y2
!			write(*,*),"newx",anom(iform,i,1)
!			write(*,*),"newy",anom(iform,i,2)
!		end do
!	end do
end if

! Anomaly von long/lat in x/y
do iform=1,nform
	write(*,*)' nnod=',nnod(iform)
	do i=1,nnod(iform)
		fi=anom(iform,i,1)
		tet=anom(iform,i,2)
		write(*,*),"fi,tet",fi,tet
		call SFDEC(fi,tet,0.,X,Y,Z,fi0,tet0)
		anom(iform,i,1)=x
		anom(iform,i,2)=y
		write(*,*)x,y
	end do
end do
close(1)




nan=nform
!write(*,*),"should open file"
do ilev=1,nlev
	write(lv,'(i2)')ilev
!        write(*,*),ilev
	open(15,file='../../FIG_FILES/SYN_INI/horcounter_'//lv//'.bln')
	do iform=1,nform
		write(*,*) nform
		zzz=hlev(ilev)
		write(*,*)zzz
		if ((zzz-zan1(iform))*(zzz-zan2(iform)).gt.0) cycle
		write(15,*)nnod(iform)
		do i=1,nnod(iform)
			fi=anom(iform,i,1)
			tet=anom(iform,i,2)
			xx=anom(iform,i,1)
			yy=anom(iform,i,2)
			write(*,*),"xy ",fi,tet
!			call decsf(fi,tet,zzz,fi0,tet0,xx,yy,hh)
			write(15,*)xx,yy
!			write(*,*),"coord (write to file) ",xx,yy
		end do
	end do
	close(15)
end do
!write(*,*),"should close file"
write(*,*)' nan=',nan
return
end

