! USE IFPORT
character*8 ar0,ar1,re1,re0,line
character*1 ps,itt,it0,rm,gr
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/reftable/izttab,ntab(2,200),hzttab(2,200),ttab(2,200,5000),rtab(2,200,5000),etab(2,200,5000),atab(2,200,5000)
common/ray/ nodes,xray(10000),yray(10000),zray(10000)
real xstn(10000),ystn(10000),zstn(10000),statinv(2,10000)
real istkr(500),ipskr(500),toutkr(500),trefkr(500)
integer n_gist(300)
real nmin,nmax
common/nanom/n_anomaly
common/pi/pi,per
common/center/fi0,tet0
!common/ray_param/ds_ini,ds_part_min,val_bend_min,bend_max0
common/ray_param/ds_ini,ds_ini_fine,ds_part_min,ds_part_fine,dist_trace,val_bend_min,bend_max0
common/noise_kod/ kod_noise,iter,red_ps(2),h_shift,z_shift
common/crust_model/nmod


one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0

open(1, file='../../kod_syn.dat')
read(1,'(a8,1x,a8)')re1,ar1	! synthetic model
read(1,'(a8,1x,a8)')re0,ar0	! real data model from which the rays are taken	
close(1)

!******************************************************************
open(1,file='../../DATA/'//re1//'/'//ar1//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
call pause()
554 read(1,*)fi0,tet0
close(1)

!******************************************************************

!******************************************************************
open(1,file='../../DATA/'//re1//'/'//ar1//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=583)line
	if(line.eq.'LOCATE_P') goto 584
end do
583 continue
write(*,*)' cannot find LOCATE _PARAMETERS in major_param.dat!!!'
call pause()
584 continue

read(1,*)	! Parameters for bending:
read(1,*)ds_ini,ds_ini_fine
read(1,*)ds_part_min,ds_part_min_fine
read(1,*)dist_trace
read(1,*)val_bend_min
read(1,*)bend_max0
read(1,*)
read(1,*)
read(1,*)maxloc_dist
do i=1,10
	read(1,*)
end do
read(1,*)ifreq


close(1)


w_qual=1
!******************************************************************


open(1,file='../../DATA/'//re1//'/'//ar1//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=558)line
	if(line.eq.'MOHO MOD') goto 559
end do
558 continue
write(*,*)' cannot find MOHO MODEL in major_param.dat!!!'
call pause()
559 read(1,*)model_crust
close(1)

nmod=model_crust
write(*,*)'model_crust=',nmod



open(1,file='../../DATA/'//re1//'/'//ar1//'/INI_PARAM/anomaly.dat')
read(1,*) n_anomaly
close(1)
write(*,*)' ar1=',ar1,'   kod of anom.=',n_anomaly

call read_crust(model_crust)
call prepare_ref(re1,ar1)
call prepare_noise(re1,ar1)
write(itt,'(i1)') iter


if(n_anomaly.eq.1)then
	call prep_board_dv(re1,ar1)
else if(n_anomaly.eq.2)then
	call read_hor_an (re1,ar1)
else if(n_anomaly.eq.3)then
	call read_vert_an(re1,ar1)
else if(n_anomaly.eq.4)then
	call read_vert_brd(re1,ar1)
end if


open(1,file='../../DATA/'//re1//'/'//ar1//'/INI_PARAM/ref_syn.dat')
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

open(1,file='../../DATA/'//re1//'/'//ar1//'/TIMES/table.dat',form='unformatted')
izt=0
nrmax=0
34	continue
	izt=izt+1
	do ips=1,2
		read(1,end=35)hzttab(ips,izt),ntab(ips,izt)
		if(ntab(ips,izt).gt.nrmax) nrmax=ntab(ips,izt)
		!write(*,*)' i=',izt,' z=',hzttab(ips,izt),' ips=',ips,' nref=',ntab(ips,izt)
		do i=1,ntab(ips,izt)
			read(1)etab(ips,izt,i),ttab(ips,izt,i),atab(ips,izt,i),rtab(ips,izt,i)
			!write(*,*)etab(ips,izt,i),atab(ips,izt,i)
		end do
		!call pause()
	end do
goto 34
35 close(11)
izttab=izt-1
!write(*,*)' izttab=',izttab,' nrmax=',nrmax
!call pause()


open(21,file='../../FIG_FILES/1DMOD/ref_true.bln')
write(21,*)nrefmod
do i=1,nrefmod
	write(21,*)vmodp(i),-hmod(i)
end do
write(21,*)nrefmod
do i=1,nrefmod
	write(21,*)vmods(i),-hmod(i)
end do
close(21)



! Read the coordinates of the stations
open(2,file='../../DATA/'//re0//'/'//ar0//'/TIMES/stat_xy.dat')
open(12,file='../../DATA/'//re1//'/'//ar1//'/TIMES/stat_xy.dat')
i=0
3	i=i+1
	read(2,*,end=4)xstn(i),ystn(i),zstn(i)
	write(12,*)xstn(i),ystn(i),zstn(i)
	!write(*,*)xstn(i),ystn(i),zstn(i)
	goto 3
4	close(2)
nst=i-1
close(12)
write(*,*)' nst=',nst



!call SRAND(1.7)

write(*,*)' itt=',itt,' red_ps=',red_ps
open(1,file='../../DATA/'//re0//'/'//ar0//'/TIMES/rays_p'//itt//'.dat',form='unformatted')
open(10,file='../../DATA/'//re1//'/'//ar1//'/TIMES/rays_loc.dat')
open(11,file='../../DATA/'//re1//'/'//ar1//'/TIMES/rays_p0.dat',form='unformatted')
open(12,file='../../DATA/'//re1//'/INIDATA/srces_true.dat')
open(13,file='../../DATA/'//re1//'/'//ar1//'/TIMES/ztr0.dat')
open(14,file='../../FIG_FILES/STAT/noise_distr.dat')


nzt=0
nray=0
nr=0
disp_tot1=0
disp_tot2=0
disp=0
dd=0
ddp=0
dds=0
dp=0
ds=0
np=0
ns=0
hsh_tot=0
zsh_tot=0
shift_tot=0
n_gist=0
21	continue
	read(1,end=22)xzt,yzt,zzt,nkrat,key_reloc
	!write(*,*)xzt,yzt,zzt,nkrat,key_reloc

	nzt=nzt+1
	!if(nzt.gt.200) goto 22
	d_ztr=sqrt(xzt*xzt+yzt*yzt)
	horsh=h_shift*rand()
	hsh_tot=hsh_tot+horsh
	zsh=z_shift*(2*rand()-1)
	zsh_tot=zsh_tot+abs(zsh)
	alp=360.0*rand()
	xsh=horsh*cos(per*alp)
	ysh=horsh*sin(per*alp)
	shift_tot=shift_tot+sqrt(horsh**2+zsh**2)
	!write(*,*)horsh,xsh,ysh,zsh
	xzt_new=xzt+xsh
	yzt_new=yzt+ysh
	zzt_new=zzt+zsh
	if (zzt_new.lt.0) zzt_new=0.
	write(11)xzt_new,yzt_new,zzt_new,nkrat,key_reloc
	!write(11)xzt,yzt,zzt,nkrat
	call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,hzt)
	write(12,*)fzt,tzt,zzt
	call decsf(xzt_new,yzt_new,zzt_new,fi0,tet0,fzt_new,tzt_new,hzt)
	write(10,*)fzt_new,tzt_new,zzt_new,nkrat
	write(13,*)fzt_new,tzt_new,zzt_new

	avres_p=0
	avres_s=0
	nkr_p=0
	nkr_s=0

	do ikrat=1,nkrat
		read(1)ist,ips,tobs,tref
		!write(*,*)ist,ips,tobs,tref
		xst=xstn(ist)
		yst=ystn(ist)
		zst=zstn(ist)
		!write(*,*)xst,yst,zst
		nray=nray+1

	
		if (d_ztr.gt.maxloc_dist) then
			call trace_bending(xst,yst,zst,xzt,yzt,zzt,ips,key_reloc,	tout)
		else
			call trace_bending(xzt,yzt,zzt,xst,yst,zst,ips,key_reloc,	tout)
		end if
		!tref=tout
		!write(*,*)' tout=',tout,' tref=',tref,' dt=',tout-tref


		if(kod_noise.eq.1) then
			dt_rand=our_noise(nray,0.0,ips)
		else if(kod_noise.eq.2) then
			dt_rand=(tobs-tref)*red_ps(ips)
		else
			dt_rand=0.
		end if
		if (ips.eq.1) then
			jj=0
			do j=-80,80,1
				jj=jj+1
				nmin=(j-1)*0.005
				nmax=j*0.005
				if ((dt_rand-nmin)*(dt_rand-nmax).gt.0) cycle
				n_gist(jj)=n_gist(jj)+1
				exit
			end do
		end if
					
		!write(*,*)' dt_rand=',dt_rand
		tout=tout+dt_rand
		dd=dd+abs(dt_rand)
		if (ips.eq.1) ddp=ddp+abs(dt_rand)
		if (ips.eq.2) dds=dds+abs(dt_rand)

		!write(11)ist,ips,tout,tref
		!write(*,*)ist,ips,tout,tref
		!write(10,*)ips,ist,tout
		istkr(ikrat)=ist
		ipskr(ikrat)=ips
		toutkr(ikrat)=tout
		trefkr(ikrat)=tref
		disp=disp+abs(tout-tref)
		if(ips.eq.1)dp=dp+abs(tout-tref)
		if(ips.eq.2)ds=ds+abs(tout-tref)
		if(ips.eq.1)np=np+1
		if(ips.eq.2)ns=ns+1
		if (d_ztr.gt.maxloc_dist) then
			if (ips.eq.1) then
				avres_p=avres_p+(tout-tref)
				nkr_p=nkr_p+1
			else
				avres_s=avres_s+(tout-tref)
				nkr_s=nkr_s+1
			end if
		end if

	end do
		
	if (nkr_p.gt.0) avres_p=avres_p/nkr_p
	if (nkr_s.gt.0)	avres_s=avres_s/nkr_s
	!write(*,*)'avres_p=',avres_p,'avres_s=',avres_s
	do ikrat=1,nkrat
		ist=istkr(ikrat)
		ips=ipskr(ikrat)
		tout=toutkr(ikrat)
		tref=trefkr(ikrat)

		if (d_ztr.gt.maxloc_dist) then
			if (ips.eq.1) tout=tout-avres_p
			if (ips.eq.2) tout=tout-avres_s
		end if
		write(11)ist,ips,tout,tref
		write(10,*)ips,ist,tout
		!write(*,*)ist,ips,tout,tref

	end do

		
		

	dcur=disp/nray
	ddcur=dd/nray
	dpcur=dp/np
	dscur=ds/ns
	if (mod(nzt,ifreq).eq.0) write(*,*)' nzt=',nzt,' dp=',dpcur,' ds=',dscur,' er=',ddcur
	!call pause()


	goto 21
22 close(1)
close(11)
close(12)
close(13)
hsh_tot=hsh_tot/nzt
zsh_tot=zsh_tot/nzt
shift_tot=shift_tot/nzt

ddpfin=ddp/np
ddsfin=dds/ns
write(*,*)' noiseP =',ddpfin,' noiseS =',ddsfin
write(*,*)' Shift hor =',hsh_tot,' ver =',zsh_tot,' total =',shift_tot
write(*,*)' nzt=',nzt,' nray=',nray

jj=0
do j=-80,80,1
	jj=jj+1
	nmin=(j-1)*0.005
	nmax=j*0.005
	write(14,*) nmin,nmax,n_gist(jj)
end do

close(14)

stop
end

