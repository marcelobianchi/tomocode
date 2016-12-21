program select
character*8 re,ar,line

real start_res, max_val
real fstloc(200),tstloc(200),zstloc(200)
real fstwor(10000),tstwor(10000),zstwor(10000)
real tall(20),hall(20),aall(20)
real tobkr(1000),trefkr(1000),tobkrr(1000),trefkrr(1000)
integer ipskr(1000),istkr(1000),ipskrr(1000),istkrr(1000)
integer wst, cor_y_n, nkrr

common/reftable/izttab,ntab(2,200),hzttab(2,200),ttab(2,200,5000),rtab(2,200,5000),etab(2,200,5000),atab(2,200,5000)
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/pi/pi,per

one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0
rz=6371.


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)
read(1,*)
read(1,*,err=33)koe

33 close(1)

!re='Ugd_tele'
!ar='real_011'

write(*,*) ' re= ',re,' ar=',ar		! code of the area


!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=501)line
	if(line.eq.'AREA_CEN') goto 502
end do
501 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
call pause()
502 read(1,*)fi0,tet0
close(1)
write(*,*)' fi0',fi0,' tet0',tet0
!******************************************************************

!************** Sofia, Feb. 2013  **********************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=521)line
	if(line.eq.'RESIDUAL') goto 522
end do
521 continue
write(*,*)' cannot find RESIDUAL_CORRECTION in major_param.dat!!!'
write(*,*)' .. or may be there is no file rays_tele_dir_initial.dat in the INIDATA folder .. '
call pause()
522 read(1,*) cor_y_n, max_val
close(1)
write(*,*)' cor_y_n',cor_y_n,' max_val',max_val
!************** Sofia --> end ;-P ****************************************************


open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=511)line
	if(line.eq.'SELECT_P') goto 512
end do
511 continue
write(*,*)' cannot find SELECT PARAMETERS in major_param.dat!!!'
call pause()
512 continue
	! parameters for the local data selection
	read(1,*)
	read(1,*) kod_local			!using of local events (1 - yes, 0 - no)
	read(1,*) krat_min_local	!min number of registered phases
	read(1,*) dist_to_stat_max  !max distance to the nearest station
	read(1,*) kod_resid_corr_local	!correction for average residual for each event (1 - yes, 0 - no)
        read(1,*) key_reloc_local           !relocation key (1-yes, 0-no)
	read(1,*)
	! parameters for the teleseismic data selection (direct scheme)
	read(1,*) kod_dir			!using of tele events (direct scheme) (1 - yes, 0 - no)
	read(1,*) epimin,epimax		!min and max epicentral distance for tele events
	read(1,*) krat_min_dir		!min number of registered phases
	read(1,*) kod_resid_corr_dir	!correction for average residual for each event (1 - yes, 0 - no)
	read(1,*) key_reloc_dir           !relocation key (1-yes, 0-no)
        read(1,*)
	! parameters for the teleseismic data selection (inverse scheme)
	read(1,*) kod_inv			!using of tele events (inverse scheme) (1 - yes, 0 - no)
	read(1,*) ar_rad			!Radius of the study area in degrees
	read(1,*) krat_min_inv,krat_max		!min and max number of registered phases
	read(1,*) kod_resid_corr_inv	!correction for average residual for each event (1 - yes, 0 - no)
        read(1,*) key_reloc_inv           !relocation key (1-yes, 0-no)

close(1)


call prepare_ref(re,ar)
write (*,*) 'Reading Stations ' 
open (1,file='../../DATA/'//re//'/INIDATA/stations_local.dat')
open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
	ist=1
	10	read(1,*,end=11)fstloc(ist),tstloc(ist),zstloc(ist)
		call sfdec(fstloc(ist),tstloc(ist),zstloc(ist),xst,yst,zzst,fi0,tet0)
		write(11,*)xst,yst,zzst
		!write(*,*)fstloc(ist),tstloc(ist),zstloc(ist)
		ist=ist+1
		goto 10
	11 close(1)
	nstloc=ist-1
	write(*,*)'nstloc=',nstloc
	wst=0
if (kod_inv.eq.1) then
	wst=1
	open(1,file='../../DATA/ISC_DATA/stations_world.dat')
	110	read(1,*,end=111)fstwor(wst),tstwor(wst),zzz
		zstwor(wst)=-zzz/1000.
		call sfdec(fstwor(wst),tstwor(wst),zstwor(wst),xst,yst,zzst,fi0,tet0)
		write(11,*)xst,yst,zzst
		wst=wst+1
		goto 110
	111 close(1)
	nstworld=wst-1
	write(*,*)'nstworld=',nstworld
end if
!call pause()

open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p0.dat',form='unformatted')
	nray=0
	nztl=0
	nrlp=0
	nrls=0
	if (kod_local.eq.0) goto 31

	write(*,*)'******************************************************************************'
	write(*,*)'select local data'
	write(*,*)'******************************************************************************'

	open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_loc0.dat',form='unformatted')
	open(19,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_loctmp.dat')

	30	read(1,end=31)xzt,yzt,zzt,nkrat
		!write(*,*)xzt,yzt,zzt,nkrat
	!	nray=nray+nkrat
		resav_p=0
		resav_s=0
		nkr_p=0
		nkr_s=0

		call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,zz)

		distmin=1000000.
		do ikr=1,nkrat
			read(1)ist,ips,tobs,trf
			!write(*,*)ist,ips,tobs,trf
			fst=fstloc(ist)
			tst=tstloc(ist)
			zst=zstloc(ist)
			call sfdec(fst,tst,zst,xst,yst,zst,fi0,tet0)
			tout=trf

			dist=sqrt((xzt-xst)**2+(yzt-yst)**2)

			!epi=epic_dist(fst,tst,fzt,tzt)
			!dist=epi*per*rz
			if (dist.lt.distmin) distmin=dist
			
			!diskr=sqrt(dist*dist+zz*zz)
			!if (diskr.lt.0.01) then 
			!	tout=0.
			!	goto 35
			!end if

			!call ref_time(dist,zz,ips, tout,zmax,dip_angle)

			!35 continue
			if (ips.eq.1)then
				resav_p=resav_p+(tobs-tout)
				nkr_p=nkr_p+1
			else
				resav_s=resav_s+(tobs-tout)
				nkr_s=nkr_s+1
			end if
						
			trefkr(ikr)=tout
			tobkr(ikr)=tobs
			ipskr(ikr)=ips
			istkr(ikr)=ist
		end do
		resav_p=resav_p/nkr_p
		resav_s=resav_s/nkr_s

		if (nkrat.lt.krat_min_local) goto 30
		if (distmin.gt.dist_to_stat_max) goto 30

		nztl=nztl+1
		nray=nray+nkrat
		write(11)xzt,yzt,zzt,nkrat,key_reloc_local
		write(19,*)xzt,yzt,zzt,nkrat,key_reloc_local,nztl
		if (mod(nztl,100).eq.0) write(*,*)nztl,fzt,tzt,zz,nkrat

		do ikr=1,nkrat
			
			if (kod_resid_corr_local.eq.1) then
				if (ipskr(ikr).eq.1) tobkr(ikr)=tobkr(ikr)-resav_p
				if (ipskr(ikr).eq.2) tobkr(ikr)=tobkr(ikr)-resav_s
			end if

			if(ipskr(ikr).eq.1) nrlp=nrlp+1
			if(ipskr(ikr).eq.2) nrls=nrls+1

			write(11)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
			!write(*,*)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
		end do

	goto 30

31 close(1)	 

!close(11)
nray_loc=nray

nray=0
nrdp=0
nrds=0
nzt_dir=0
istmax=0
nzt1=0



! Sofia, Feb. 2013
open(121,file='../../DATA/'//re//'/INIDATA/rays_tele_dir_neu.dat')
open(123,file='../../DATA/'//re//'/INIDATA/rejected_rays.dat')
if (cor_y_n.eq.0) goto 121
	write(*,*)'******************************************************************************'
	write(*,*)'*************** BEFORE starting the actual Data selection, remove outliers!!' 
	open(122,file='../../DATA/'//re//'/INIDATA/rays_tele_dir_initial.dat')

	90	read(122,*,end=121)fzt,tzt,zz,nkrat
	        !write(*,*)' 0) Beben: ',fzt,tzt,zz,nkrat

		epizt=epic_dist(fzt,tzt,fi0,tet0)
		call sfdec(fzt,tzt,zz,xzt,yzt,zzt,fi0,tet0)
		!call decsf(xzt,yzt,zzt,fi0,tet0,fff,ttt,zzz)
		write(*,*)' 0) epidist zu center: ',epizt
		nkr=0
                nkrr=0
		do ikr=1,nkrat
			read(122,*)ips,ist,tobs
			if (ist.gt.istmax) istmax=ist
			!write(*,*)' 0) picktime from station: ', ips,ist,tobs
			
			fst=fstloc(ist)
			tst=tstloc(ist)
			zst=zstloc(ist)

			epi=epic_dist(fst,tst,fzt,tzt)
			dist=epi*per*rz
			!write(*,*)' 0) stat - eq , ',fst,tst,fzt,tzt,'dist[km]=',dist,'dist[deg]=',epi

			diskr=sqrt(dist*dist+zz*zz)

			if (diskr.lt.0.01) then 
				tout=0.
				goto 125
			end if

			call ref_time(dist,zz,ips, tout,zmax,dip_angle)
			!write(*,*)' 0) tout ',tout
			125 continue

                        start_res=tobs-tout
			if (start_res.lt.max_val) then
				nkr=nkr+1
				trefkr(nkr)=tout
				tobkr(nkr)=tobs
				ipskr(nkr)=ips
				istkr(nkr)=ist
                                write(*,*)" 0) residual - nkr: ",start_res, nkr
                        else
				nkrr=nkrr+1
				trefkrr(nkrr)=tout
				tobkrr(nkrr)=tobs
				ipskrr(nkrr)=ips
				istkrr(nkrr)=ist
                                write(*,*)" 0) event rejected: ",start_res, nkrr
			end if
		end do
	
		write(121,*)fzt,tzt,zz,nkr
                write(*,*)" 0) should write 1): ",fzt,tzt,zz,nkr

		do ikr=1,nkr
			write(121,*)ipskr(ikr),istkr(ikr),tobkr(ikr),trefkr(ikr)
			write(*,*)" 0) should write 2): ",ipskr(ikr),istkr(ikr),tobkr(ikr),trefkr(ikr)
		end do

		if (nkrr.gt.0) then 
			write(123,*)fzt,tzt,zz,nkrr
                	write(*,*)" 0) should write to rejected 1): ",fzt,tzt,zz,nkrr	
			do ikr=1,nkrr
				write(123,*)ipskrr(ikr),istkrr(ikr),tobkrr(ikr),trefkrr(ikr)
				write(*,*)" 0) should write to rejected 2): ",ipskrr(ikr),istkrr(ikr),tobkrr(ikr),trefkrr(ikr)
			end do
		end if



	goto 90
	close(1)

121 continue
close(121)
close(123)

nkr=0
write(*,*)" 0) END OF PRESELECTION LOOP "
nray_loc=nray
nray=0
nrdp=0
nrds=0
nzt_dir=0
istmax=0
nzt1=0
if (kod_dir.eq.0) goto 21
	write(*,*)'******************** now the actual selection can start **********************************'
	write(*,*)'******************************************************************************'
	write(*,*)'select teleseismic data (DIRECT)'
	write(*,*)'******************************************************************************'

        open(21,file='../../DATA/'//re//'/'//ar//'/TIMES/events_tele_dir.dat')
        if (cor_y_n.eq.1) then
	    open(1,file='../../DATA/'//re//'/INIDATA/rays_tele_dir_neu.dat')
            write(*,*)' input is residual_corrected file'
        elseif (cor_y_n.eq.0) then 
	    open(1,file='../../DATA/'//re//'/INIDATA/rays_tele_dir.dat')
            write(*,*)' input is initial ray file'
        end if
! *****************  Sofia - end ;-P  ******************************************

	20	read(1,*,end=21)fzt,tzt,zz,nkrat
	        !write(*,*)' Beben: ',fzt,tzt,zz,nkrat
		resav_p=0
		resav_s=0
		nkr_p=0
		nkr_s=0

		epizt=epic_dist(fzt,tzt,fi0,tet0)
		call sfdec(fzt,tzt,zz,xzt,yzt,zzt,fi0,tet0)
		!call decsf(xzt,yzt,zzt,fi0,tet0,fff,ttt,zzz)
		!write(*,*)' epidist zu center: ',epizt
		nkr=0
		do ikr=1,nkrat
			read(1,*)ips,ist,tobs
			if (ist.gt.istmax) istmax=ist
			!write(*,*)' picktime from station: ', ips,ist,tobs
			!if (ips.eq.2) cycle
			if (nkrat.lt.krat_min_dir) cycle
			if (epizt.lt.epimin) cycle
			if (epizt.gt.epimax) cycle
			!if (ist.eq.50) cycle			
			
			nkr=nkr+1
			fst=fstloc(ist)
			tst=tstloc(ist)
			zst=zstloc(ist)

			epi=epic_dist(fst,tst,fzt,tzt)
			dist=epi*per*rz
			!write(*,*)'stat - eq , ',fst,tst,fzt,tzt,'dist[km]=',dist,'dist[deg]=',epi

			diskr=sqrt(dist*dist+zz*zz)

			if (diskr.lt.0.01) then 
				tout=0.
				goto 25
			end if

			call ref_time(dist,zz,ips, tout,zmax,dip_angle)
                        !write(*,*)'tout ',tout
			25 continue
			if (ips.eq.1)then
				resav_p=resav_p+(tobs-tout)
				nkr_p=nkr_p+1
			else
				resav_s=resav_s+(tobs-tout)
				nkr_s=nkr_s+1
			end if
			
			trefkr(nkr)=tout
			tobkr(nkr)=tobs
			ipskr(nkr)=ips
			istkr(nkr)=ist
		end do
		resav_p=resav_p/nkr_p
		resav_s=resav_s/nkr_s
                !write(*,*)'resav P ',resav_p

		if (nkr.lt.krat_min_dir) goto 20
		if (epizt.lt.epimin) goto 20 
		if (epizt.gt.epimax) goto 20	
		
		nzt1=nzt1+1	
               if (koe.eq.1.and.mod(nzt1,2).ne.0)goto 20
		if (koe.eq.2.and.mod(nzt1,2).eq.0)goto 20
		nray=nray+nkr
		nzt_dir=nzt_dir+1
		write(11)xzt,yzt,zzt,nkr,key_reloc_dir
		write(21,*)fzt,tzt,zz
		if (mod(nzt_dir,20).eq.0) write(*,*)nzt_dir,fzt,tzt,zz,nkr

		do ikr=1,nkr
			if (kod_resid_corr_dir.eq.1) then
				if (ipskr(ikr).eq.1) tobkr(ikr)=tobkr(ikr)-resav_p
				if (ipskr(ikr).eq.2) tobkr(ikr)=tobkr(ikr)-resav_s
			end if

			if(ipskr(ikr).eq.1) nrdp=nrdp+1
			if(ipskr(ikr).eq.2) nrds=nrds+1

			write(11)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
			!write(*,*)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
		end do

		!write(*,*)resav_p,resav_s
		!call pause()

	goto 20
	close(1)
	close(21)

21 continue
	nray_dir=nray
	write(*,*)'istmax=',istmax


nray=0
nrip=0
nris=0
nzt_inv=0
if (kod_inv.eq.0) goto 41

	write(*,*)'******************************************************************************'
	write(*,*)'select teleseismic data (INVERSE)'
	write(*,*)'******************************************************************************'

	open(1,file='../../DATA/ISC_DATA/rays_world.dat',form='unformatted')
	40	read(1,end=41)fzt,tzt,zz,nkrat
		!write(*,*)fzt,tzt,zz,nkrat

		resav_p=0
		resav_s=0
		nkr_p=0
		nkr_s=0
		epizt=epic_dist(fzt,tzt,fi0,tet0)
		call sfdec(fzt,tzt,zz,xzt,yzt,zzt,fi0,tet0)
	
		nkr=0
		do ikr=1,nkrat
			read(1)ips,wst,tobs
			!write(*,*)ips,wst,tobs
			if (ips.gt.2) cycle
			if (nkrat.lt.krat_min_inv) cycle			
			if (epizt.gt.ar_rad) cycle
			if (nkrat.gt.krat_max) cycle
			!call pause()

			nkr=nkr+1
			fst=fstwor(wst)
			tst=tstwor(wst)
			zst=zstwor(wst)

			epi=epic_dist(fst,tst,fzt,tzt)
			dist=epi*per*rz
			!write(*,*) fst,tst,fzt,tzt,'dist=',dist

			diskr=sqrt(dist*dist+zz*zz)

			if (diskr.lt.0.01) then 
				tout=0.
				goto 45
			end if

			call ref_time(dist,zz,ips, tout,zmax,dip_angle)

			45 continue

			if (abs(tobs-tout).gt.10)then
				nkr=nkr-1
				cycle
			end if
			if (ips.eq.1)then
				resav_p=resav_p+(tobs-tout)
				nkr_p=nkr_p+1
			else
				resav_s=resav_s+(tobs-tout)
				nkr_s=nkr_s+1
			end if
			
			trefkr(nkr)=tout
			tobkr(nkr)=tobs
			ipskr(nkr)=ips
			istkr(nkr)=wst+nstloc
		end do
		resav_p=resav_p/nkr_p
		resav_s=resav_s/nkr_s

		if (nkr.lt.krat_min_inv) goto 40
		if (epizt.gt.ar_rad) goto 40

		nray=nray+nkr
		nzt_inv=nzt_inv+1
		write(11)xzt,yzt,zzt,nkr,key_reloc_inv
		if (mod(nzt_inv,10).eq.0) write(*,*)nzt_inv,fzt,tzt,zz,nkr

		do ikr=1,nkr
			if (kod_resid_corr_inv.eq.1) then
				if (ipskr(ikr).eq.1) tobkr(ikr)=tobkr(ikr)-resav_p
				if (ipskr(ikr).eq.2) tobkr(ikr)=tobkr(ikr)-resav_s
			end if

			if(ipskr(ikr).eq.1) nrip=nrip+1
			if(ipskr(ikr).eq.2) nris=nris+1

			write(11)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
			!write(*,*)istkr(ikr),ipskr(ikr),tobkr(ikr),trefkr(ikr)
		end do

	goto 40
	close(1)

41 continue
	nray_inv=nray
close(11)

write(*,*)'nzt_loc=',nztl,'nray_loc=',nray_loc,nrlp,nrls
write(*,*)'nzt_dir=',nzt_dir,'nray_dir=',nray_dir,nrdp,nrds
write(*,*)'nzt_inv=',nzt_inv,'nray_inv=',nray_inv,nrip,nris

stop
end
