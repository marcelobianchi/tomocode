!USE DFPORT

character*4 dsaa/'DSAA'/
character*8 ar,md,line
character*20 scale_line
character*1 ps
character*2 ver
real fia0(100),teta0(100),fib0(100),tetb0(100)
integer kdot_rgb(3)

! For reading the kod_syn_all.dat
integer whichmodel, num_models,imodel, narg, iargc
character*3 parinput

common/center/fi0,tet0
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/pi/pi,per

allocatable v_ini(:,:),v_abs(:,:)

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0

!Reading the parameters from the kod_syn_all.dat
open(1, file='../../kod_syn_all.dat')
read(1,*) num_models
read(1,*)
read(1,*)
read(1,*)

!Get the number of parameters
narg=iargc()

if (num_models.GT.1) then
 if(narg.EQ.0) then
  do imodel=1,num_models
   read(1,'(a8)') ar
   read(1,'(a8)') md
   read(1,*)
   write (*,'(i3,3a,a17)') imodel,') ',ar//' '//md
  enddo
  write(*,'(a,$)') 'Which model do you want to compute for? '
  read (*,*) whichmodel

  rewind(1)
  read(1,*) num_models
  read(1,*)
  read(1,*)
  read(1,*)
 else
  call getarg (1,parinput)
  read(parinput,*) whichmodel
  write(*,*) 'Using model:',whichmodel
 endif
else
 whichmodel=1
endif

if (whichmodel.GT.num_models.OR.whichmodel.LE.0) then
 WRITE(*,*) 'Selected model ',whichmodel,' larger than possible ',num_models,'.'
 STOP
endif

do imodel=1,whichmodel
 read(1,'(a8)') ar
 read(1,'(a8)') md
 read(1,*)
enddo

write(*,*) 'Region=',ar,' Area=',md
close(1)

key_preview=0
!open(1,file='../../preview_key.txt')
!read(1,*,end=771)key_preview
!771 close(1)

!if(key_preview.ne.0) then
!	i=system('mkdir ..\..\PICS\'//ar//'\'//md)
!	open(1,file='../../DATA/'//ar//'/config.txt')
!	read(1,*) 
!	read(1,*) npix_map_x,npix_map_y
!	read(1,*) 
!	read(1,*) 
!	read(1,*) npix_x0,npix_y
!	read(1,*)tick_x,tick_y
!	read(1,*)
!	read(1,*)
!	read(1,*)
!	read(1,*)
!	read(1,*)
!	read(1,*)
!	read(1,*)scale_line
!	read(1,*)amp_min,amp_max
!	close(1)
!
!	i=system('copy ..\..\COMMON\visual_exe\visual.exe layers.exe')
!	i=system('copy ..\..\COMMON\scales_scl\'//scale_line//' '//scale_line)
!
!end if


!******************************************************************
open(1,file='../../DATA/'//ar//'/'//md//'/INI_PARAM/major_param.dat')
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

! Read the values of the reference model
open(1,file='../../DATA/'//ar//'/'//md//'/INI_PARAM/ref_syn.dat')
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
!write(*,*)' nrefmod=',nrefmod


open(1,file='../../DATA/'//ar//'/'//md//'/INI_PARAM/anomaly.dat')
read(1,*) n_anomaly
close(1)
write(*,*)' ar=',ar,'   kod of anom.=',n_anomaly


if(n_anomaly.eq.1)then
	call prep_board_dv(ar,md)
else if(n_anomaly.eq.2)then
	call read_hor_an (ar,md)
else if(n_anomaly.eq.3)then
	call read_vert_an(ar,md)
else if(n_anomaly.eq.4)then
	call read_vert_brd(ar,md)
end if



open(2,file='../../DATA/'//ar//'/'//md//'/INI_PARAM/setver.dat')
open(66,file='../../FIG_FILES/VERT/referencever.dat')
read(2,*)nver
do ii=1,nver
	read(2,*) fia0(ii),teta0(ii),fib0(ii),tetb0(ii)
	write(66,*)  fia0(ii),teta0(ii),fib0(ii),tetb0(ii),ii
end do
read(2,*) 
read(2,*) dxsec
read(2,*) zmin,zmax,dzsec
close(2)
close(66)
!dxsec=2
!dzsec=2
xshift=0

do ips=1,2
	write(ps,'(i1)')ips
	do iver=1,nver
		write(ver,'(i2)')iver
		fia=fia0(iver)
		teta=teta0(iver)
		fib=fib0(iver)
		tetb=tetb0(iver)
		call SFDEC(fia,teta,0.,xa,ya,Z,fi0,tet0)
		call SFDEC(fib,tetb,0.,xb,yb,Z,fi0,tet0)
		!write(*,*)' xa=',xa,' ya=',ya
		!write(*,*)' xb=',xb,' yb=',yb
		dist=sqrt((xb-xa)*(xb-xa)+(yb-ya)*(yb-ya))
		npix_x = int(npix_y * dist/(zmax-zmin))
		write(*,*)' section:',ver,' dist=',dist,' npix_x=',npix_x
		sinpov=(yb-ya)/dist
		cospov=(xb-xa)/dist
		nxsec=dist/dxsec+1
		dxsec=dist/(nxsec-1)
		nzsec=(zmax-zmin)/dzsec+1
		dzsec=(zmax-zmin)/(nzsec-1)

		allocate (v_ini(nxsec,nzsec),v_abs(nxsec,nzsec))

		write(*,*)' nxsec=',nxsec,' nzsec=',nzsec
		v_ini=0

		do ix=1,nxsec
			sss=(ix-1)*dxsec+xshift
			!write(*,*)' ix=',ix,' sss=',sss,' dist=',dist
			!sss=60
			!if(mod(ix,10).eq.0)write(*,*)' ix=',ix,' sss=',sss
			xcur=xa+((xb-xa)/dist)*sss
			ycur=ya+((yb-ya)/dist)*sss
			!call decsf(xcur,ycur,0.,fi0,tet0,fi,tet,h)



			!write(*,*)' xcur=',xcur,' ycur=',ycur
			!write(*,*)' fi=',fi,' tet=',tet
			do iz=1,nzsec
				zcur=zmin+iz*dzsec
				v0=vrefmod(zcur,ips)
				!zcur=50
				!write(*,*)' avmoho=',avmoho,' z_moho=',z_moho
				if(n_anomaly.eq.1)then
					dv = dv_board(xcur,ycur,zcur,ips)
				else if(n_anomaly.eq.2)then
					dv = hor_anom(xcur,ycur,zcur,ips)
				else if(n_anomaly.eq.3)then
					dv=vert_anom(xcur,ycur,zcur,ips)
				else if(n_anomaly.eq.4)then
					dv=vert_brd(xcur,ycur,zcur,ips)
				end if
				v_ini(ix,iz)=dv
				v_abs(ix,iz)=v0*(1+dv/100)
				!write(*,*)' zcur=',zcur,' dv=',dv
			end do

		end do
		open(14,file='../../FIG_FILES/SYN_INI/syn_dv'//ver//ps//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nxsec,nzsec
		write(14,*)0,dist
		write(14,*)-zmax,-zmin
		write(14,*)-9999,999
		do iz=nzsec,1,-1
			write(14,*)(v_ini(ix,iz),ix=1,nxsec)
		end do
		close(14)

		open(14,file='../../FIG_FILES/SYN_INI/syn_abs'//ver//ps//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nxsec,nzsec
		write(14,*)0+xshift,dist+xshift
		write(14,*)-zmax,-zmin
		write(14,*)-9999,999
		do iz=nzsec,1,-1
			write(14,*)(v_abs(ix,iz),ix=1,nxsec)
		end do
		close(14)


		if(key_preview.eq.0) goto 441


		!*********************************************************
		!*********************************************************
		!*********************************************************
		!*********************************************************
		!*********************************************************
		!*********************************************************

		if(npix_x0.ne.0) then
			npix_x=npix_x0
		else
			npix_x = int(npix_y * dist/(zmax-zmin))
		end if
		write(*,*)' section:',ver,' dist=',dist,' npix_x=',npix_x

		open(14,file='config.txt')
		write(14,*)npix_x,npix_y
		write(14,*)'_______ Size of the picture in pixels (nx,ny)'
		write(14,*)0,dist
		write(14,*)'_______ Physical coordinates along X (xmin,xmax)'
		write(14,*)-zmax,-zmin
		write(14,*)'_______ Physical coordinates along Y (ymin,ymax)'
		write(14,*)tick_x,tick_y
		write(14,*)'_______ Spacing of ticks on axes (dx,dy)'
		write(14,51)ar,md,ver,ps
		51 format('..\..\PICS\',a8,'\',a8,'\vert_syn',a2,a1,'.png')
		write(14,*)'_______ Path of the output picture'
		if (ips.eq.1) then
			write(14,3431)	iver, iver
			3431 format(' Synthetic P anomalies, section=',i1,'A - ',i1,'B')
		else
			write(14,3432)	iver, iver
			3432 format(' Synthetic S anomalies, section=',i1,'A - ',i1,'B')
		end if
		write(14,*)'_______ Title of the plot on the upper axe'
		write(14,*)	1
		write(14,*)'_______ Number of layers'

		59 format('********************************************')

		write(14,59)
		write(14,*)	1
		write(14,*)'_______ Key of the layer (1: contour, 2: line, 3:dots)'
		write(14,52)ver,ps
		52 format('..\..\FIG_files\vert\syn_dv',a2,a1,'.grd')
		write(14,*)'_______ Location of the GRD file'
		write(14,53)scale_line
		53 format(a20)
		write(14,*)'_______ Scale for visualization'
		write(14,*)	amp_min,amp_max
		write(14,*)'_______ scale diapason'


		close(14)


		i=system('layers.exe')


		441		continue
		deallocate(v_ini,v_abs)

	end do
end do

!pause
if(key_preview.eq.0) stop

open(2,file='../../DATA/'//ar//'/sethor.dat')
read(2,*)  
read(2,*)   
read(2,*) fmap1,fmap2,dfmap,tmap1,tmap2,dtmap  
close(2)

open(14,file='config.txt')
write(14,*)npix_map_x,npix_map_y
write(14,*)'_______ Size of the picture in pixels (nx,ny)'
write(14,*)fmap1+fi0,fmap2+fi0
write(14,*)'_______ Physical coordinates along X (xmin,xmax)'
write(14,*)tmap1+tet0,tmap2+tet0
write(14,*)'_______ Physical coordinates along Y (ymin,ymax)'
write(14,'(2i3)')1,1
write(14,*)'_______ Spacing of ticks on axes (dx,dy)'
write(14,151)ar,md
151 format('..\..\PICS\',a8,'\',a8,'\profiles.png')
write(14,*)'_______ Path of the output picture'
write(14,*)	' Profiles'
write(14,*)'_______ Title of the plot on the upper axe'
write(14,*)	1
write(14,*)'_______ Number of layers'

open(1,file='../../DATA/'//ar//'/map/polit_bound.bln',status='old',err=491)
close(1)
write(14,59)
write(14,*)	2
write(14,*)'_______ Key of the layer (1: contour, 2: line, 3:dots)'
write(14,54)ar
54 format('..\..\DATA\',a8,'\map\polit_bound.bln')
write(14,*)'_______ Location of the BLN file'
write(14,*)	3
write(14,*)'_______ Thickness of line in pixels'
write(14,*)	100,0,0
write(14,*)'_______ RGB color'
491 continue

open(1,file='../../DATA/'//ar//'/map/coastal_line.bln',status='old',err=492)
close(1)
write(14,59)
write(14,*)	2
write(14,*)'_______ Key of the layer (1: contour, 2: line, 3:dots)'
write(14,55)ar
55 format('..\..\DATA\',a8,'\map\coastal_line.bln')
write(14,*)'_______ Location of the BLN file'
write(14,*)	5
write(14,*)'_______ Thickness of line in pixels'
write(14,*)	0,0,255
write(14,*)'_______ RGB color'
492 continue

do iver=1,nver
	write(ver,'(i2)')iver

	write(14,59)
	write(14,*)	2
	write(14,*)'_______ Key of the layer (1: contour, 2: line, 3:dots)'
	write(14,154)ver
154 format('..\..\FIG_files\vert\mark_',a2,'.bln')
	write(14,*)'_______ Location of the BLN file'
	write(14,*)	2
	write(14,*)'_______ Thickness of line in pixels'
	write(14,*)	0,0,0
	write(14,*)'_______ RGB color'

	write(14,59)
	write(14,*)	3
	write(14,*)'_______ Key of the layer (1: contour, 2: line, 3:dots)'
	write(14,156)ver
156 format('..\..\FIG_files\vert\mark_',a2,'.dat')
	write(14,*)'_______ Location of the DAT file'
	write(14,*)	1
	write(14,*)'_______ Symbol (1: circle, 2: square)'
	write(14,*)	6
	write(14,*)'_______ Size of dots in pixels'
	write(14,*)	0,0,0
	write(14,*)'_______ RGB color'

end do

close(14)


i=system('layers.exe')



stop
end
