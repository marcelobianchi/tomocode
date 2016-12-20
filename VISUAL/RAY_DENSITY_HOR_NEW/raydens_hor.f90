character*4 dsaa/'DSAA'/
character*8 ar,re,line
character*2 lv
character*1 it,ps,gr
real hlev(100),ornt(10)
real ylev(90),xtop(10000,90), ztop(10000,90)
integer ntop(90),n_pop(10000,90)

allocatable plotray(:,:,:)
allocatable raydens(:,:)

one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0

open(1,file='SET.dat')
read(1,*)igr
read(1,*)anorm_aver
read(1,*)dz_lay2
close(1)
write(gr,'(i1)')igr
dz_lay=dz_lay2/2.

open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
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

orient=ornt(igr)
sinal=sin(orient*per)
cosal=cos(orient*per)
!write(*,*)' orient=',orient

open(2,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/sethor.dat')
read(2,*) nzmap  
read(2,*) (hlev(i),i=1,nzmap)  
read(2,*) fmap1,fmap2,dfmap,tmap1,tmap2,dtmap  
close(2)

nfmap=int_best((fmap2-fmap1)/dfmap+1.)
ntmap=int_best((tmap2-tmap1)/dtmap+1.)
write(*,*)' nfmap=',nfmap,' ntmap=',ntmap
allocate(raydens(nfmap,ntmap))

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

nxpl=int_best((xlim2-xlim1)/dxpl)
nypl=int_best((ylim2-ylim1)/dypl)
nzpl=int_best((zlim2-zlim1)/dzpl)


write(*,*)' nx=',nxpl,' ny=',nypl,' nz=',nzpl
write(*,*)' size of the blocks, km:'
write(*,*)' dx=',dxpl,' dy=',dypl,' dz=',dzpl
write(*,*)' average ray length in a block, km:'


allocate(plotray(nxpl,nypl,nzpl))





open(1,file='../../FIG_FILES/RAY_DENS/referencedepth.dat', status="UNKNOWN", ERR=19)
write (*,*) 'Writting the referencedepth file'
do izmap=1,nzmap
	zzz=hlev(izmap)
	write(*,*)  izmap, zzz
	write (1,*) izmap, zzz
end do
close(1)
19 continue

do iiips=1,2
	write(ps,'(i1)')iiips
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/levinfo'//ps//gr//'.dat')
	i=0
	722 i=i+1
		read(1,*,end=721)n,ylev(i)
		goto 722
	721 nlev=i-1
	write(*,*)' nlev=',nlev
	close(1)

	ntop=0
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr'//ps//gr//'.dat')

	do n=1,nlev
		read(1,*,end=556)ntop(n)
		!write(*,*)' y=',ylev(n),' ntop=',ntop(n)
		do i=1,ntop(n)
			read(1,*)xtop(i,n),ztop(i,n),n_pop(i,n)
		end do
	end do
	556		close(1)


	write (*,*) 'Reading file: plotray'//ps//gr//'.dat'
	open(1,file='../../TMP/plotray'//ps//gr//'.dat',form='unformatted', status="OLD", ERR=22)

	nonzer=0
	sumtotal=0
	do iy=1,nypl
		read(1,END=22)((plotray(ix,iy,iz),ix=1,nxpl),iz=1,nzpl)
	!	write(*,'(10f7.2)')((plotray(ix,iy,iz),ix=1,nxpl),iz=1,nzpl)
		do iz=1,nzpl
			do ix=1,nxpl
				if(plotray(ix,iy,iz).lt.1.)cycle
				nonzer=nonzer+1
				sumtotal=sumtotal+plotray(ix,iy,iz)
			end do
		end do
	end do
	close(1)
	averplot=sumtotal/nonzer
	write(*,*)' iiips=',iiips,' aver=',averplot

	do izmap=1,nzmap
		write(lv,'(i2)')izmap
		zzz=hlev(izmap)
		do iz=1,nzpl-1
			z1=zlim1+(iz-1)*dzpl
			z2=zlim1+(iz)*dzpl
			if((zzz-z1)*(zzz-z2).le.0.) exit
		end do

		raydens=-999.

		do itet=1,ntmap
			ttt=(itet-1)*dtmap+tmap1+tet0
			!write(*,*)' itet=',itet,' ttt=',ttt
			!ttt=-7.45
			do ifi=1,nfmap
				fff=(ifi-1)*dfmap+fmap1+fi0



				call SFDEC(fff,ttt,0.,x11,y11,Z,fi0,tet0)
				xcur=x11*cosal+y11*sinal
				ycur=-x11*sinal+y11*cosal

				if((xcur-xlim1)*(xcur-xlim2).gt.0) cycle
				if((ycur-ylim1)*(ycur-ylim2).gt.0) cycle

				do ix=1,nxpl-1
					x1=xlim1+(ix-1)*dxpl
					x2=xlim1+(ix)*dxpl
					if((xcur-x1)*(xcur-x2).le.0.) exit
				end do
				do iy=1,nypl-1
					y1=ylim1+(iy-1)*dypl
					y2=ylim1+(iy)*dypl
					if((ycur-y1)*(ycur-y2).le.0.) exit
				end do
				if (plotray(ix,iy,iz).gt.0.1) then
					raydens(ifi, itet)=plotray(ix,iy,iz)/(averplot*anorm_aver)
				else
!					write(*,*) 'zero file ... '
					raydens(ifi, itet) = -999.
				end if

			end do
		end do
		open(14,file='../../FIG_FILES/RAY_DENS/hor'//ps//'_'//lv//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nfmap,ntmap
		write(14,*)fmap1+fi0,fmap2+fi0
		write(14,*)tmap1+tet0,tmap2+tet0
		write(14,*)-999,999
		do itet=1,ntmap
			write(14,*)(raydens(ifi,itet),ifi=1,nfmap)
		end do
		close(14)


! Nodes:
		open(11,file='../../FIG_FILES/RAY_DENS/grid_hor'//ps//'_'//lv//'.dat')
		xold=0
		yold=0
		do n=1,nlev
			y=ylev(n)
			do i=1,ntop(n)
				x=xtop(i,n)
				if(abs(x-xold).lt.0.1.and.abs(y-yold).lt.0.1) cycle
				z=ztop(i,n)
				if((z-zzz-dz_lay)*(z-zzz+dz_lay).gt.0.) cycle
				xx=x*cosal-y*sinal
				yy=x*sinal+y*cosal
				call decsf(xx,yy,zz,fi0,tet0,FI,TET,h)
				write(11,*)fi,tet
				xold=x
				yold=y
			end do
		end do



	end do
	cycle

 22	write (*,*) 'Skipped'
end do
stop
end
