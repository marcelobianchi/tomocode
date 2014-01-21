subroutine read_hor_dh(ar)
character*8 ar
character*5 fig
real xcur(1000),ycur(1000)



common/dh_an/nan,anom(40,4000,2),val_dh(40),nnod(40)

write(*,*)' ar=',ar
open(1,file='../../DATA/'//ar//'/INI_PARAM/an_moho.dat')
read(1,*) 
read(1,*) 
read(1,*) nform

do iform=1,nform
	read(1,*) 
	read(1,*) fig
	read(1,*) xpmin,xpmax,ypmin,ypmax
	read(1,*) val_dh(iform)

	open(2,file='../../DATA/'//ar//'/FORMS/'//fig//'.bln')
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
	end do
	close(2)
	!write(*,*)' xmin=',xmin,' xmax=',xmax
	!write(*,*)' zmin=',zmin,' zmax=',zmax
	!pause

	dfdx=(xpmax-xpmin)/(xmax-xmin)
	dtdy=(ypmax-ypmin)/(ymax-ymin)
	if(abs(xpmin-xpmax).lt.0.000001) then
		do i=1,ncurve
			anom(iform,i,1)=xcur(i)
			anom(iform,i,2)=ycur(i)
			!write(*,*)anom(iform,i,1),anom(iform,i,2)
		end do
		nnod(iform)=ncurve+1
		anom(iform,nnod(iform),1)=xcur(1)
		anom(iform,nnod(iform),2)=ycur(1)
	else
		do i=1,ncurve
			anom(iform,i,1)=xpmin+dfdx*(xcur(i)-xmin)
			anom(iform,i,2)=ypmin+dtdy*(ycur(i)-ymin)
		end do
		nnod(iform)=ncurve+1
		anom(iform,nnod(iform),1)=xpmin+dfdx*(xcur(1)-xmin)
		anom(iform,nnod(iform),2)=ypmin+dtdy*(ycur(1)-ymin)
	end if

	write(*,*)' nnod=',nnod(iform)
end do
close(1)

nan=nform
write(*,*)' nan=',nan

return
end
