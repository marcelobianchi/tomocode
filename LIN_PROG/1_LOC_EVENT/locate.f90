program locate_1D
!USE DFPORT
real trmin(500),tobmn(500),almn(500)
integer ngdmn(500)
integer ist222(500),ips222(500)
real tob222(500),dtold(500)

real tall(50),hall(50),aall(50)
integer kmin(2000),nall(2000),kodes(10,50000)
real dkode(50000)

real zstart(10),zst_it(10,10), w_qual(10)

real dx_it(10),dy_it(10),dz_it(10)
real res_it1(10),res_it2(10),wps_it(10)

real azim(500)

character*8 ar,re,line
character*1 rm

common/stations/ xst(9000),yst(9000),zst(9000)
common/krat/nkrat,istkr(500),tobkr(500),ipskr(500),qualkr(500),trfkr(500),ngood(500),alkr(500),diskr(500)
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/center/fi0,tet0
common/pi/pi,per

common/loc_param/wgs,res_loc1,res_loc2,dist_limit,n_pwr_dist,ncyc_av,w_P_S_diff


one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0
iprint=0
Rz=6371.0

kod_true_model=0


! Read codes of the model




koe=0
open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)
read(1,*)
read(1,*,end=221)koe
221 close(1)
write(*,*)' area=',re,' model=',ar,' koe=',koe

call read_z_lim(re,ar)
!zmax=z_lim(fi,tet)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
call pause()

554 read(1,*)fi0,tet0
close(1)

w_qual=1
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=543)line
	if(line.eq.'LIN_LOC_') goto 544
end do
543 continue
write(*,*)' cannot find LIN_LOC_PARAM in major_param.dat!!!'
call pause()

544 continue
read(1,*)krat_min
read(1,*)dist_max
read(1,*)wgs
read(1,*)dist_limit	!=100
read(1,*)n_pwr_dist	!=1
read(1,*)ncyc_av	!=10
read(1,*)
read(1,*)	! For output:
read(1,*)bad_max	!=30
read(1,*)res_1_km
read(1,*)sss_max
read(1,*)
read(1,*)ifreq
read(1,*)
read(1,*)niter_loc
write(*,*)niter_loc
do it=1,niter_loc
	read(1,*)
	read(1,*)dx_it(it),dy_it(it),dz_it(it)
	read(1,*)res_it1(it)
	read(1,*)res_it2(it)
	read(1,*)wps_it(it)
end do
close(1)
write(*,*)'nkrat_min=',krat_min

gap_max=360.


call prepare_ref(re,ar)

! Read the coordinates of the stations
open(1,file='../../DATA/'//re//'/INIDATA/stations_local.dat')
open(12,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
nst=0
33	read(1,*,end=44)fi,tet,zstat
	call SFDEC(fi,tet,zstat,X,Y,Z,fi0,tet0)
	nst=nst+1
	xst(nst)=x
	yst(nst)=y
	zst(nst)=z
	write(12,*)xst(nst),yst(nst),zst(nst)
	!write(*,*)xst(nst),yst(nst),zst(nst)
	goto 33
44	close(1)
close(12)
write(*,*)' nst=',nst
!call pause()

!open(1,file='../../data/'//re//'/inidata/rays.dat')
open(1,file='../../DATA/'//re//'/INIDATA/rays_local.dat')


if(kod_true_model.eq.1) then
	open(2,file='../../DATA/'//re//'/INIDATA/srces_true0.dat')
end if

open(15,file='../../DATA/'//re//'/'//ar//'/TIMES/srces_true.dat')
open(16,file='../../FIG_FILES/RAYS/hor_shift0.bln')


open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_loc0.dat',form='unformatted')
open(12,file='../../DATA/'//re//'/'//ar//'/TIMES/ztr0.dat')

izt=0
nray=0
nrp=0
nrs=0
nztgood=0
dtot=0
ntot=0


! Read the sources:
992	continue
	write(*,*) 'New Event nev=',izt
	read(1,*,end=991)fini,tini,zini,nkrat

	call SFDEC(fini,tini,zini,xold,yold,zold,fi0,tet0)
	if(zold.le.0.) zold=0

	if(kod_true_model.eq.1) then
		read(2,*,end=993)fold0,told0,hold0
	else
993		fold0=fini
		told0=tini
		hold0=zini
	end if
	call SFDEC(fold0,told0,hold0,xold0,yold0,zold0,fi0,tet0)

	!write(*,*)fini,tini,zold,nkrat
	!write(*,*)xold,yold,zold,nkrat 
	!read(1)imt,idy,ihr,imn		
	!write(*,*)imt,idy,ihr,imn		
	!xbl=xbl-100.5
	!ybl=ybl+80.7

	izt=izt+1
	!write(*,*)' nkrat=',nkrat
	!write(*,*)izt,fini,tini,zold,nkrat

! Read all the records:

	do i=1,nkrat
		read(1,*)ips,ist,tobs
		!write(*,*)ist,ips,tobs
		iqua=1
		!read(1,end=991)ist,ips,iqua,tobs
		istkr(i)=ist	!ist: code of station, 
		ipskr(i)=ips
		tobkr(i)=tobs	! tobs: observerd arrival time
		qualkr(i)=w_qual(iqua)
		call decsf(xst(ist),yst(ist),zst(ist),fi0,tet0,fst,tst,hst)
		call hipc(fini,tini,fst,tst,fi0,tet0, hd,az)
		azim(i)=az
	end do
	!write(*,*)fini,tini,zold 

	do i=2,nkrat
		do j=1,i-1
			if (azim(j).gt.azim(i)) then
				az=azim(j)
				azim(j)=azim(i)
				azim(i)=az
			end if
		end do
	end do
	
	gap=0
	do i=1,nkrat-1
		!write(*,*)azim(i) 
		gp=abs(azim(i+1)-azim(i))
		if (gp.gt.gap) gap=gp
	end do
	!write(*,*)azim(nkrat)
	gp=abs(360+azim(1)-azim(nkrat))
	if (gp.gt.gap) gap=gp
	

	!if(izt.lt.449) goto 992


	!if (gap.gt.gap_max) goto 992
	if(koe.eq.1.and.mod(izt,2).eq.0) goto 992
	if(koe.eq.2.and.mod(izt,2).eq.1) goto 992
	!if(zold.gt.100) zold=50.
	!write(*,*)izt,xold,yold,zold,nkrat 
	!do i=1,nkrat
		!write(*,*)istkr(i),ipskr(i),tobkr(i)
	!end do
	!call pause()

	if(nkrat.lt.krat_min) then
	  write(*,*) ' Abort nev nkrat=',nkrat,' too low nkratmin=',krat_min
	  goto 992
	 endif

	dismin=9999999
	do i=1,nst
		hordist=sqrt((xst(i)-xold)*(xst(i)-xold)+(yst(i)-yold)*(yst(i)-yold))
		if(hordist.lt.dismin) dismin=hordist
	end do
	!write(*,*)' dismin 1111=',dismin
	if(dismin.gt.dist_max) then
	   write(*,*) ' Abort distance dst=',dismin,' larger than dist_max =',dist_max
	   goto 992
	 endif

	xmin=xold	!+300
	ymin=yold	!-300
	zmin=zold	!+500

	call decsf(xmin,ymin,zmin,fi0,tet0,fff,ttt,h)
	zlim=z_lim(fff,ttt)
	if(h.ge.zlim) h=zlim-1

	write(*,*) ' Inverting ',nkrat,' picks @ ',dismin,' distance'
	do iter=1,niter_loc

		res_loc1=res_it1(iter)
		res_loc2=res_it2(iter)
		w_P_S_diff=wps_it(iter)
		dx_loc=dx_it(iter)
		dy_loc=dy_it(iter)
		dz_loc=dz_it(iter)

		!write(*,*)fini,tini,zini,nkrat
		!write(*,*)xmin,ymin,zmin

		call goal_new(xmin,ymin,zmin, disp,aver,nk,amax)

		!write(*,*)' amax=',amax
		!write(*,*)res_loc1,res_loc2,w_P_S_diff

		!write(*,*)xmin,ymin,zmin,amax
		!call pause()

		nkode=1
		kodes(1,nkode)=0
		kodes(2,nkode)=0
		kodes(3,nkode)=0
		dkode(nkode)=amax
		ixmin1=0
		iymin1=0
		izmin1=0

	282 continue
		index=0
		do iix=1,5
			ix=ixmin1+iix-3
			dx=dx_loc*ix
			do iiy=1,5
				iy=iymin1+iiy-3
				dy=dy_loc*iy
				do iiz=1,5
					iz=izmin1+iiz-3
					do ik=1,nkode
						if(kodes(1,ik).eq.ix.and.kodes(2,ik).eq.iy.and.kodes(3,ik).eq.iz) goto 281
					end do
					dz=dz_loc*iz
					zzz=zmin+dz

					call decsf(xmin+dx,ymin+dx,zzz,fi0,tet0,fff,ttt,zsf)
					zlim=z_lim(fff,ttt)

					if(zlim.lt.zsf) cycle

					if(zsf.lt.0) cycle


					if(zsf.ge.zlim) zzz=zlim-1

					call goal_new(xmin+dx,ymin+dy,zzz, disp,aver,nk,ank)
					!write(*,'(3i3,3f6.1,f7.3)')ix,iy,iz,xmin+dx,ymin+dy,zzz,ank

					nkode=nkode+1
					kodes(1,nkode)=ix
					kodes(2,nkode)=iy
					kodes(3,nkode)=iz
					dkode(nkode)=ank
					if(ank.le.amax) cycle
					index=1
					ixmin=ix
					iymin=iy
					izmin=iz
					amax=ank
					!write(*,*)ix,iy,iz,' ank=',ank
	281				continue
				end do
			end do
		end do
		!write(*,*)ixmin,iymin,izmin,amax
		if(index.eq.1) then
			ixmin1=ixmin
			iymin1=iymin
			izmin1=izmin
			goto 282
		end if

		xmin=xmin+dx_loc*(ixmin1)
		ymin=ymin+dy_loc*(iymin1)
		zmin=zmin+dz_loc*(izmin1)
		!write(*,*)' after iteration:',iter
		!write(*,*)xmin,ymin,zmin,amax
	end do

	call goal_new(xold,yold,zold, disp,aver,nk,aold)

	!write(*,*)' xold=',xold,' yold=',yold,' zold=',zold,' aold=',aold
	!do i=1,nkrat
		!write(*,'(2i4,3f8.3)')ipskr(i),istkr(i),tobkr(i)-trfkr(i)-aver,diskr(i)
	!end do

	call goal_new(xmin,ymin,zmin, disp,aver,nk,ank)
	!write(*,*)' z=',zmin,' ank=',ank,' d=',disp,' nk=',nk
	!write(*,*)' xmin=',xmin,' ymin=',ymin,' zmin=',zmin,' ank=',ank
	!do i=1,nkrat
		!write(*,'(2i4,3f8.3)')ipskr(i),istkr(i),tobkr(i)-trfkr(i)-aver,diskr(i)
	!end do


	dismin=9999999
	do i=1,nst
		hordist=sqrt((xst(i)-xmin)*(xst(i)-xmin)+(yst(i)-ymin)*(yst(i)-ymin))
		if(hordist.lt.dismin) dismin=hordist
	end do
	!write(*,*)' dismin 2222=',dismin
	!call pause()
	if(dismin.gt.dist_max) goto 992


	!write(*,*)'aver=',aver
	do i=1,nkrat
		tobkr(i)=tobkr(i)-aver
		dt=tobkr(i)-trfkr(i)
		!write(*,347)istkr(i),ipskr(i),diskr(i),dtold(i),dt
347	format(' ist=',i4,' ips=',i3,' dist=',f7.1,' dt_old=',f9.3,' dt_new=',f9.3)
	end do

! Determine the GOOD and BAD residuals:

	nbad=0
	ngood=1
	do i=1,nkrat
		ist=istkr(i)
		xs=xst(ist)
		ys=yst(ist)
		dhor=sqrt((xs-xmin)*(xs-xmin)+(ys-ymin)*(ys-ymin))
		dist=sqrt(dhor*dhor+zmin*zmin)
		if(dist.gt.sss_max) dist=sss_max
		res_limit=dist*res_1_km
		if(ipskr(i).eq.2)res_limit=res_limit*wgs
		dt=tobkr(i)-trfkr(i)
		!write(*,*)ipskr(i),istkr(i),dt,res_limit
		if(abs(dt).lt.res_limit) cycle
		ngood(i)=0
		nbad=nbad+1
	end do
	!call pause()
	nk=nkrat-nbad
	abad=nbad
	akrat=nkrat
	ratio_bad=(abad/akrat)

	!write(*,*)' nbad=',nbad,' ngood=',nk
	if(ratio_bad*100.gt.bad_max) then
		!write(*,*)' BAD event!!!'
		!write(*,488)izt,xmin,ymin,zmin,ank
		!write(*,488)izt,xold0,yold0,zold0
		!call pause()
		goto 992
	end if
	if(nk.lt.krat_min) goto 992

	!write(*,*)' disp=',disp,' nk=',nk,' ank=',ank
!	if(nk.lt.krat_min) goto 992


1515 continue
	call decsf(xold,yold,0.,fi0,tet0,fold,told,h)
	call decsf(xmin,ymin,0.,fi0,tet0,fnew,tnew,h)
	write(11)xmin,ymin,zmin,nk
	!write(*,*)xmin,ymin,zmin,nk
	write(15,*)fold0,told0,zold0
	write(12,*)fnew,tnew,zmin
	!write(14,*)fold,told,zold,nk
	write(16,*)2
	write(16,*)fold0,told0
	write(16,*)fnew,tnew
	nk1=0

!open(31,file='test_event.dat')
!write(31,*)' fi=',fnew
!write(31,*)' tet=',tnew
!write(31,*)' dep=',zmin
!write(31,773)
!write(*,773)
773 format(' ips ','|',' sta ','|',' qua ','|','   dt   ','|','  dist  ','|','  angle')
	do i=1,nkrat
		if(ngood(i).eq.0) cycle
		write(11)istkr(i),ipskr(i),tobkr(i),trfkr(i)
		!write(*,*)istkr(i),ipskr(i),tobkr(i),trfkr(i)
		!write(*,'(2i4,3f8.3)')ipskr(i),istkr(i),qualkr(i),tobkr(i)-trfkr(i),diskr(i)
!write(31,772)ipskr(i),istkr(i),qualkr(i),tobkr(i)-trfkr(i),diskr(i),alkr(i)
!write(*,772)ipskr(i),istkr(i),qualkr(i),tobkr(i)-trfkr(i),diskr(i),alkr(i)
772 format(i5,'|',i5,'|',f5.1,'|',f8.3,'|',f8.2,'|',f8.2)
		dtot=dtot+abs(tobkr(i)-trfkr(i))
		ntot=ntot+1
		nk1=nk1+1
		nray=nray+1
		if(ipskr(i).eq.1) then
			nrp=nrp+1
		else
			nrs=nrs+1
		end if
	end do
!close(31)
	if(nk.ne.nk1) call pause()



	nztgood=nztgood+1
	!if(mod(nztgood,1).eq.0.and.index.eq.0) then
	if(mod(nztgood,ifreq).eq.0) then
!		write(*,*)' nzt=',nztgood,' nray=',nray,' np=',nrp,' ns=',nrs
!		write(*,*)' nkold=',nkold,' dispold=',dispold
		!write(*,*)' averbest=',averbest
		!write(*,341)izt,jyr,jmc,jdy,zmin,nkrat,amag,timzt
		!write(15,341)izt,jyr,jmc,jdy,zmin,nkrat,amag,timzt
341 format(i4,' y=',i4,' m=',i3,' d=',i3,' z=',f7.2,' kr=',i4,' mag=',f7.2,' t=',f12.3)
		!write(*,*)' month=',imt,' day=',idy,' hr=',ihr
		!write(*,*)' dismin 2222=',dismin
		write(*,488)izt,xold,yold,zold,aold
		write(*,489)nztgood,xmin,ymin,zmin,ank
	488	format(i6,' Old: x=',f8.2,' y=',f8.2,' z=',f8.2,' ank=',f7.2)
	489	format(i6,' New: x=',f8.2,' y=',f8.2,' z=',f8.2,' ank=',f7.2)
		dcur=dtot/ntot
		write(*,*)' nkrat=',nkrat,' nk=',nk,' ntot=',ntot
		!write(*,*)' ntot=',ntot,' dcur=',dcur
		write(*,*)
		!if(ank.lt.0.6)call pause()
		!stop
		!call pause()
	end if
	!if(ntot.gt.nraymax)goto 991

	goto 992
991 continue
close(1)
write(*,*)' nztgood=',nztgood
stop
end


