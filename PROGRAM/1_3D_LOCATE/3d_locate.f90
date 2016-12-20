program trace
!use DFPORT
character*8 ar,re,line
character*1 ps,itt,it0,rm,gr
PARAMETER (nkrmax=1000)

real xstn(10000),ystn(10000),zstn(10000),statinv(2,10000)
!real tobkr(nkrmax),qualkr(nkrmax),trfkr(nkrmax),alkr(nkrmax),diskr(nkrmax),dtold(nkrmax)
!integer istkr(nkrmax),ipskr(nkrmax),ngood(nkrmax)

real dt3(nkrmax), w_qual(10)
real aaa(nkrmax,4),atmp(nkrmax,4),bbb(nkrmax),btmp(nkrmax),xxx(4)
real www(4),vvv(4,4),dttest(nkrmax)
real tob_best(nkrmax),trf_best(nkrmax),dtold(nkrmax)

integer deal_stat

allocatable rays_all(:,:,:),rays_best(:,:,:),nod_all(:),nod_best(:)

common/loc_param/wgs,res_loc1,res_loc2,dist_limit,n_pwr_dist,ncyc_av,w_P_S_diff
common/krat/nkrat,istkr(nkrmax),tobkr(nkrmax),ipskr(nkrmax),qualkr(nkrmax),trfkr(nkrmax),ngood(nkrmax),alkr(nkrmax),diskr(nkrmax)

common/refmod/nrefmod,hhmod(600),vmodp(600),vmods(600)
common/pi/pi,per
common/ray_param/ds_ini,ds_ini_fine,ds_part_min,ds_part_fine,dist_trace,val_bend_min,bend_max0

common/crust1/fa1,fb1,ta1,tb1,dfi1,dtet1,nfi1,ntet1,dcr_1(360,180)

common/ray/ nodes,xray(10000),yray(10000),zray(10000)
common/center/fi0,tet0
common/inimodel/model_type
common/nkr_max/nnkrmax
common/tele/kod_tele
common/crust_model/nmod

nnkrmax=nkrmax



one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0
Rz=6371.0

r_hor=0.1
r_ver=0.01
r_time=0.1


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter			! iterations number
close(1)

write(itt,'(i1)')iter
write(it0,'(i1)')iter-1

write(*,*)' ar=',ar,' it=',itt

!goto 300
!
call read_3D_mod_v(re,ar,iter-1)
call read_z_lim(re,ar)

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
close(1)
write(*,*)nornt

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
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


!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
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
read(1,*)	! Parameters for location:
read(1,*)maxloc_dist
read(1,*)dist_limit	!=100
read(1,*)n_pwr_dist	!=1
read(1,*)ncyc_av	!=10
read(1,*)
read(1,*)res_loc1
read(1,*)res_loc2
read(1,*)w_P_S_diff
read(1,*)stepmax
read(1,*)stepmin
read(1,*)
read(1,*)ifreq
close(1)



open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/refmod.dat')
read(1,*,end=81)vpvs
i=0
82	i=i+1
	read(1,*,end=81)hhmod(i),vmodp(i),vs
	if(vpvs.lt.0.000001) then
		vmods(i)=vs
	else
		vmods(i)=vmodp(i)/vpvs
	end if
	!write(*,*)hhmod(i),vmodp(i),vmods(i)
goto 82
81	close(1)
nrefmod=i-1
!write(*,*)' nrefmod=',nrefmod

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ini_model.dat')
read(1,*,end=576)model_type
576 close(1)

if (model_type.eq.5) then
    call read_ini_model_3D(re,ar)
else if (model_type.eq.1) then
    call read_ini_model(re,ar)
end if
call prepare_ref(re,ar)
call read_crust(model_crust)

! Read the coordinates of the stations
open(2,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
i=0
3	i=i+1
	read(2,*,end=4)xstn(i),ystn(i),zstn(i)
	!write(*,*)xstn(i),ystn(i),zstn(i)
	goto 3
4	close(2)
nst=i-1

write(*,*)' nst=',nst

statinv=0
if(iter.ne.1) then
	do igr=1,nornt

		write(gr,'(i1)')igr
		open(2,file='../../DATA/'//re//'/'//ar//'/RESULT/stcor_p_'//it0//gr//'.dat')
		do ist=1,nst
			read(2,*,end=332)stcor
			statinv(1,ist) = statinv(1,ist)+stcor
		end do
		close(2)

332		open(2,file='../../DATA/'//re//'/'//ar//'/RESULT/stcor_s_'//it0//gr//'.dat')
		do ist=1,nst
			read(2,*,end=333)stcor
			statinv(2,ist) = statinv(2,ist)+stcor
		end do
		close(2)
	end do
	statinv = statinv / nornt
333	continue
end if

if(iter.ne.1) then
	do igr=1,nornt
		write(gr,'(i1)')igr
		iun=30+igr
		open(iun,file='../../DATA/'//re//'/'//ar//'/RESULT/ztcor_'//it0//gr//'.dat')
	end do
end if

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it0//'.dat',form='unformatted')

open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//itt//'.dat',form='unformatted')
open(14,file='../../DATA/'//re//'/'//ar//'/TIMES/srces'//itt//'.dat')
open(12,file='../../TMP/ray_paths_p_'//itt//'.dat',form='unformatted')

allocate(rays_all(3,5000,2000),rays_best(3,5000,2000),nod_all(2000),nod_best(2000),stat=deal_stat)

nzt=0
nray=0
dis_tot1=0
dis_tot2=0
ntot=0
21	continue
	kod_tele=0
	read(1,end=22)xini,yini,zini,nkrat,key_reloc

	!write(*,*)xini,yini,zini,nkrat,key_reloc
	kod_loc=1
	dx_corr=0
	dy_corr=0
	dz_corr=0
	dt_corr=0

	if(iter.ne.1) then
		do igr=1,nornt
			iun=30+igr
			read(iun,*)dx,dy,dz,dt
			!write(*,*)igr,dx,dy,dz,dt
			dx_corr=dx_corr+dx
			dy_corr=dy_corr+dy
			dz_corr=dz_corr+dz
			dt_corr=dt_corr+dt
		end do
		dx_corr=dx_corr/nornt
		dy_corr=dy_corr/nornt
		dz_corr=dz_corr/nornt
		dt_corr=dt_corr/nornt
		!write(*,*)' sum:',dx_corr,dy_corr,dz_corr,dt_corr

	end if

	xzt=xini - dx_corr
	yzt=yini - dy_corr
	zzt=zini - dz_corr

	d_ztr=sqrt(xzt*xzt+yzt*yzt)
	nzt=nzt+1
	if(zzt.lt.0) zzt=0
	!write(*,*)'d_ztr=',d_ztr,'maxloc_dist=',maxloc_dist
	
	!write(*,*)xzt,yzt,zzt,nkrat

	!write(11)xzt,yzt,zzt,nkrat
	!call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,zzz)
	!write(14,*)fzt,tzt,zzz
	!epizt=epic_dist(fzt,tzt,fi0,tet0)
	!epizt=epizt*per*Rz

	if(nkrat.eq.0) goto 21

	nkr=0
	do ikrat=1,nkrat
		read(1)ist,ips,tobs_old,tref
		!write(*,*)ist,ips,tobs_old,tref
		nray=nray+1
		tobs=tobs_old - dt_corr - statinv(ips,ist)

		istkr(ikrat)=ist
		ipskr(ikrat)=ips
		tobkr(ikrat)=tobs
		trfkr(ikrat)=tref
		dtold(ikrat)=tobs_old-tref
		qualkr(ikrat)=1

	end do
	
	!if (nzt.lt.566) goto 21
        !if (nray.lt.20198) goto 21

	!allocate(rays_all(3,5000,nkrat),rays_best(3,5000,nkrat),nod_all(nkrat),nod_best(nkrat),stat=deal_stat)
        rays_all=0; rays_best=0; nod_all=0; nod_best=0
        
        !write(*,*)deal_stat
	itstep=0
	dstot=0
	dscur=0

	goal_best=0
	step_cur=stepmax
	if(mod(nzt,ifreq).eq.0)write(*,*)xzt,yzt,zzt,nkrat
	!call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,zzz)

	!zsf=Rz-sqrt(xzt**2+yzt**2+(Rz-zzt)**2)
	!write(*,*)fzt,tzt,zzz,zsf

331	continue
	itstep=itstep+1
        !write(*,*)'itstep=',itstep
	vzt1=velocity(xzt,yzt,zzt,1)
	vzt2=velocity(xzt,yzt,zzt,2)



	nk=0
	ddd1=0
	ddd2=0

	do ikrat=1,nkrat
               !kod_tele=0
		ist=istkr(ikrat)
		ips=ipskr(ikrat)
		xst=xstn(ist)
		yst=ystn(ist)
		zst=zstn(ist)
		dshor=sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
		dsver=abs(zzt-zst)
		dist=sqrt(dshor*dshor+dsver*dsver)
		diskr(ikrat)=dist
		if (dist.gt.maxloc_dist) kod_tele=1

		s0=1/vzt1
		if(ips.ne.1) s0=1/vzt2

		tobs=tobkr(ikrat)
		tref=trfkr(ikrat)
		resid=tobs-tref
		!write(*,*)ikrat, ' ips=',ips,' resid=',resid
		!write(*,*)'event: ',xzt,yzt,zzt
		!write(*,*)'station: ',xst,yst,zst

		!xst=0.
		!yst=0.
		!zst=0.

		!xzt=10.
		!yzt=20.
		!zzt=15.

		if (d_ztr.gt.maxloc_dist) then
			call trace_bending(xst,yst,zst,xzt,yzt,zzt,ips,key_reloc,	tout)
			nod_all(ikrat)=nodes
			do i=1,nodes
				rays_all(1,i,ikrat)=xray(nodes-i+1)
				rays_all(2,i,ikrat)=yray(nodes-i+1)
				rays_all(3,i,ikrat)=zray(nodes-i+1)
				 if (isnan(tout)) then 
				 !if (nodes.gt.100) then 
                                    write(*,*)xst,yst,zst,xzt,yzt,zzt,ips,	tout
                                    write(*,*) 'nray=',nray
                                    write(*,*)xray(nodes-i+1),yray(nodes-i+1),zray(nodes-i+1)
                                !call pause()
                               end if
			end do
			!call pause()
		else
			call trace_bending(xzt,yzt,zzt,xst,yst,zst,ips,key_reloc,	tout)
                      nod_all(ikrat)=nodes
			do i=1,nodes
				rays_all(1,i,ikrat)=xray(i)
				rays_all(2,i,ikrat)=yray(i)
				rays_all(3,i,ikrat)=zray(i)
                                if (isnan(tout)) then 
				 !if (nodes.gt.100) then 
                                     write(*,*)xst,yst,zst,xzt,yzt,zzt,ips,	tout
                                     write(*,*) 'nray=',nray
                                    write(*,*)xray(nodes-i+1),yray(nodes-i+1),zray(nodes-i+1)
                                    !call pause()
                               end if
			end do
		end if

		!call pause()
		ddd1=ddd1+abs(dtold(ikrat))
		ddd2=ddd2+abs(tobs-tout)
		!write(*,*)ikrat,' tobs=',tobs,' tout=',tout,' tref=',tref ,' nodes=',nodes
		!call pause()
	
		!nod_all(ikrat)=nodes
		!do i=1,nodes
                      ! write(*,*)xray(i),yray(i),zray(i)
			!rays_all(1,i,ikrat)=xray(i)
			!rays_all(2,i,ikrat)=yray(i)
			!rays_all(3,i,ikrat)=zray(i)
		!end do

		trfkr(ikrat)=tout

		if(d_ztr.gt.maxloc_dist.or.key_reloc.eq.0) cycle
		x1=xray(1)
		y1=yray(1)
		z1=zray(1)
		x2=xray(2)
		y2=yray(2)
		z2=zray(2)


		hor=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
		tot=sqrt(hor*hor+(z2-z1)*(z2-z1))
		!write(*,*)' hor=',hor,' tot=',tot

		cosbe=(x2-x1)/hor
		sinbe=(y2-y1)/hor
		!write(*,*)' dx=',x2-x1,' dy=',y2-y1,' dz=',z2-z1

		cosal=(z2-z1)/tot
		sinal=hor/tot

		px=cosbe*sinal*s0
		py=sinbe*sinal*s0
		pz=cosal*s0
		nk=nk+1
		aaa(nk,1)=px
		aaa(nk,2)=py
		aaa(nk,3)=pz
		aaa(nk,4)=1
		bbb(nk)=tobs-tout

		nkr=nkr+1

		dt3(ikrat)=tobs-tout

	end do
	red=100*(ddd1-ddd2)/ddd1

	!call pause()

	if (d_ztr.gt.maxloc_dist.or.key_reloc.eq.0) then
		x_best=xzt
		y_best=yzt
		z_best=zzt
		tob_best=tobkr
		trf_best=trfkr
		rays_best=rays_all
		nod_best=nod_all
		kod_loc=0
		goto 555
	end if

	if(nkr.eq.0) goto 21

	do i1=1,nkrat
		bbb(i1)=dt3(i1)
		if(ipskr(i1).ne.2) cycle
		do i2=1,nkrat
			if(i2.eq.i1) cycle
			if(istkr(i1).ne.istkr(i2)) cycle
			if(ipskr(i2).ne.1) then
				if(mod(nzt,ifreq).eq.0)write(*,*)' i1=',i1,' ips1=',ipskr(i1),' i2=',i2,' ips2=',ipskr(i2)
			end if
			ipskr(i1)=3

			dt3(i1)=dt3(i1)-dt3(i2)
			bbb(i1)=dt3(i1)
			do ii=1,4
				aaa(i1,ii)=(aaa(i1,ii)-aaa(i2,ii))
			end do
			exit
		end do
	end do
	!call pause()

	call dispers(dt3,	disp3,aver3,nkr3,ank)
	!write(*,*)disp3,aver3,nkr3,ank
	!call pause()
	do i1=1,nkrat
		tobkr(i1)=tobkr(i1) -aver3
		dt3(i1)=tobkr(i1) - trfkr(i1)
		ddd=diskr(i1)
		if (ddd.lt.maxloc_dist) then
			if(ddd.lt.dist_limit) then
				wdist = 1
			else
				wdist = (dist_limit/(ddd))**n_pwr_dist
			end if
		else
			wdist=1
		end if

		ccc=1
		if(ipskr(i1).eq.3) ccc = w_P_S_diff
		if(ipskr(i1).ne.3) bbb(i1) = bbb(i1)-aver3
		do ii=1,4
			aaa(i1,ii)=aaa(i1,ii)*wdist*ccc
		end do
		bbb(i1) = bbb(i1)*wdist*ccc
	end do


	atmp=aaa
	btmp=bbb

	nk=0
	aaa=0
	bbb=0
	do ik=1,nkrat
		if(ipskr(ik).eq.3) ipskr(ik)=2
		if(abs(dt3(ik)).gt.res_loc2) cycle
		nk=nk+1
		bbb(nk)=btmp(ik)
		do i4=1,4
			aaa(nk,i4)=atmp(ik,i4)
		end do
		!write(*,*)' ist=',istkr(i),' ips=',ipskr(i)
	end do

	if(ank.gt.goal_best) then
		x_best=xzt
		y_best=yzt
		z_best=zzt
		tob_best=tobkr
		trf_best=trfkr
		goal_best=ank
		s_best=dstot
		rays_best=rays_all
		nod_best=nod_all
	else
		dscur=dscur/2.
		dxcur=dxcur/2.
		dycur=dycur/2.
		dzcur=dzcur/2.

		xzt=xzt-dxcur
		yzt=yzt-dycur
		zzt=zzt-dzcur

		dstot=dstot-dscur
		step_cur=dscur
		if(dscur.gt.stepmin) goto 331

	end if

	do i4=1,4
		nk=nk+1
		reg=r_hor
		if(i4.eq.3)reg=r_ver
		if(i4.eq.4)reg=r_time
		aaa(nk,i4)=reg
	end do

	atmp = aaa
	btmp = bbb
	m=nk
	n=4
	mp=nkrmax
	np=4

    call SVDCMP(aaa,M,N,MP,NP,www,vvv)
    call SVBKSB(aaa,www,vvv,M,N,MP,NP,bbb,xxx)
	dt=xxx(4)
	!write(*,*)' dx=',xxx(1),' dy=',xxx(2),' dz=',xxx(3),' dt=',dt
	disp1=0
	disp2=0
	do i=1,nk-4
		dt=0
		do j=1,4
			dt=dt+atmp(i,j)*xxx(j)
		end do
		disp1=disp1+abs(btmp(i))
		disp2=disp2+abs(btmp(i)-dt)
		!write(*,'(3f8.4)')bbb(i),bbb(i)-dt,dt
	end do
	disp1=disp1/nk
	disp2=disp2/nk
	red=100*(disp1-disp2)/disp1
	!write(*,*)' disp1=',disp1,' disp2=',disp2,' red=',red

	shift0=sqrt(xxx(1)*xxx(1)+xxx(2)*xxx(2)+xxx(3)*xxx(3))
	!write(*,*)' shift0=',shift0

	scale=1.
	if(shift0.gt.step_cur)scale=step_cur/shift0

	dxcur=-xxx(1)*scale
	dycur=-xxx(2)*scale
	dzcur=-xxx(3)*scale

	xnew=xzt+dxcur
	ynew=yzt+dycur
	znew=zzt+dzcur

        !write(*,*)xnew,ynew,znew

	call decsf(xnew,ynew,0.,fi0,tet0,fi,tet,h)
	deplim = z_lim(fi,tet)


	if(znew.gt.deplim.or.znew.lt.0.) then
		nk=nk-4
		do i=1,nk
			aaa(i,3)=1
		end do
		do i4=1,3
			nk=nk+1
			reg=r_hor
			if(i4.eq.3)reg=r_time
			aaa(nk,i4)=reg
		end do
		atmp = aaa
		btmp = bbb
		m=nk
		n=3
		mp=nkrmax
		np=3

		call SVDCMP(aaa,M,N,MP,NP,www,vvv)
		call SVBKSB(aaa,www,vvv,M,N,MP,NP,bbb,xxx)
		dt=xxx(3)
		!write(*,*)' dx=',xxx(1),' dy=',xxx(2),' dt=',dt
		disp1=0
		disp2=0
		do i=1,nk-3
			dt=0
			do j=1,3
				dt=dt+atmp(i,j)*xxx(j)
			end do
			disp1=disp1+abs(btmp(i))
			disp2=disp2+abs(btmp(i)-dt)
			!write(*,'(3f8.4)')bbb(i),bbb(i)-dt,dt
		end do
		disp1=disp1/(nk-3)
		disp2=disp2/(nk-3)
		red=100*(disp1-disp2)/disp1
		!write(*,*)' disp1=',disp1,' disp2=',disp2,' red=',red

		shift0=sqrt(xxx(1)*xxx(1)+xxx(2)*xxx(2))

		scale=1.
		if(shift0.gt.step_cur)scale=step_cur/shift0

		dxcur=-xxx(1)*scale
		dycur=-xxx(2)*scale
		dzcur=0

		xnew=xzt+dxcur
		ynew=yzt+dycur
		znew=zzt+dzcur
		
	end if

	dscur=sqrt(dxcur*dxcur+dycur*dycur+dzcur*dzcur)

	xzt=xnew
	yzt=ynew
	zzt=znew

	dstot=dstot+dscur
	!write(*,*)'dstot=',dstot


	if(dscur.gt.stepmin) goto 331

555 continue

	if(mod(nzt,ifreq).eq.0)write(*,*)x_best,y_best,z_best
	write(11)x_best,y_best,z_best,nkrat,key_reloc

	call decsf(x_best,y_best,0.,fi0,tet0,fzt,tzt,h)
	write(14,*) fzt,tzt,z_best


	disp1=0
	disp2=0
	do i=1,nkrat
		!write(*,*)istkr(i),ipskr(i),' dt=',tob_best(i)-trf_best(i),dtold(i)
		disp1=disp1+abs(dtold(i))
		disp2=disp2+abs(tob_best(i)-trf_best(i))
		dis_tot1=dis_tot1+abs(dtold(i))
		dis_tot2=dis_tot2+abs(tob_best(i)-trf_best(i))
		ntot=ntot+1
		write(11)istkr(i),ipskr(i),tob_best(i),trf_best(i)
		!write(*,*)istkr(i),ipskr(i),tob_best(i),trf_best(i)
		write(12)nod_best(i)
                !write(*,*)nod_best(i)
		do inod=1,nod_best(i)
			write(12)(rays_best(i3,inod,i),i3=1,3)
			!write(*,*)(rays_best(i3,inod,i),i3=1,3)
      			!write(*,*)(rays_best(i3,inod,i),i3=1,3)

		end do
 
!               write(*,*)nod_all(i)
!
!		do inod=1,nod_all(i)
!			write(*,*)(rays_all(i3,inod,i),i3=1,3)
!      			!write(*,*)(rays_best(i3,inod,i),i3=1,3)
!
!		end do
!
!		call pause()
	end do
	disp1=disp1/nkrat
	disp2=disp2/nkrat
	s_best=sqrt((xini-x_best)**2+(yini-y_best)**2+(zini-z_best)**2)
	red=100*(dis_tot1-dis_tot2)/dis_tot1
	!write(*,*)'dis_tot1=',dis_tot1,' dis_tot2=',dis_tot2
	if (kod_loc.eq.1) then
		if(mod(nzt,ifreq).eq.0)write(*,*)nzt,' ds=',s_best,' G=',goal_best,' red=',red
		if(mod(nzt,ifreq).eq.0)write(*,*)' ********************************************'
	else
		if(mod(nzt,ifreq).eq.0)write(*,*)nzt,' tele event - not relocated'
		if(mod(nzt,ifreq).eq.0)write(*,*)' ********************************************'
	end if

23 continue
!	call pause()
!    write(*,*)'adin'
!    !deallocate(rays_all,STAT=deal_stat)
!    if (deal_stat.ne.0) then
!        STOP ''
!    end if
!    write(*,*)'dwa',deal_stat
!    !deallocate(rays_best)
!    write(*,*)'tri'
!    deallocate(nod_all,nod_best)
!    write(*,*)'chet'

        !call pause()

goto 21
22 close(1)
close(11)
close(12)
close(14)
deallocate(rays_all,rays_best,nod_all,nod_best,stat=deal_stat)


write(*,*)' nzt=',nzt,' nray=',nray

300 continue
stop
end

