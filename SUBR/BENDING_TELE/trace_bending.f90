subroutine trace_bending(xzt,yzt,zzt,xst,yst,zst,ips,key_reloc,	tout)
common/ray/ nodes,xray(10000),yray(10000),zray(10000)
common/ray_min/ nrmin,xrmin(10000),yrmin(10000),zrmin(10000)
common/ray_part/ nrpart,xpart(10000),ypart(10000),zpart(10000),spart(10000),kod_part(10000)
common/shift/ dxray(10000),dyray(10000),dzray(10000)
common/ray_param/ds_ini,ds_ini1,ds_bend,ds_bend1,dist0,val_bend_min
common/ray_parameterezation/length,nod1,nod2
common/crust_par/avmoho,vcr1_p,vcr2_p,vcr1_s,vcr2_s
common/crust1/fa1,fb1,ta1,tb1,dfi1,dtet1,nfi1,ntet1,dcr_1(360,180)
common/pi/pi,per
common/center/fi0,tet0

real xrmin2(10000),yrmin2(10000),zrmin2(10000)

real t_part(1000)
integer indexx(1000)

Rz=6371.0

!ds_part_min = 5
!val_bend_min = 0.02
!bend_max0 = 10

!dist=sqrt((xzt-xst)**2+(yzt-yst)**2)
if (key_reloc.eq.1) then
    call steight_line(xzt,yzt,zzt,xst,yst,zst,ips, tout)
    !write(*,*) 'streight time=',tout
else 
    call trace_1D(xzt,yzt,zzt,xst,yst,zst,ips, tout)
    !write(*,*) '1D time = ',tout,'nodes= ',nodes
end if

aaa=0
if (isnan(tout)) then
	open (21,file='ray_path.bln')
	write(21,*)nodes
	write(21,*)0,-zray(1)
	do in=2,nodes
		x1=xray(in)
		y1=yray(in)
		z1=zray(in)
		dd=sqrt((x1-xray(1))**2+(y1-yray(1))**2)
		write(21,*)dd,-z1
		!write(*,*)x1,y1,z1
	end do
	close(21)
	!pause
end if


totdist=length
npart_max = int_best(totdist / ds_bend)

finedist=dist0
if (totdist.lt.dist0) finedist=totdist
npart_max_fine=int_best(finedist / ds_bend1)

val_bend0 = val_bend_min * npart_max
!write(*,*)' npart_max=',npart_max,' val_bend0=',val_bend0

if(npart_max_fine.eq.0) then
	xmid=(xzt+xst)/2
	ymid=(yzt+yst)/2
	zmid=(zzt+zst)/2
	vmid=velocity(xmid,ymid,zmid,ips)
	tout=totdist/vmid
	nodes=2
	xray(1)=xzt
	yray(1)=yzt
	zray(1)=zzt
	xray(2)=xst
	yray(2)=yst
	zray(2)=zst
	return
end if

tmin=tout
nrmin=nodes
xrmin=xray
yrmin=yray
zrmin=zray

!goto 500

do nparts=1,npart_max

243	continue
	indexx=0
	dpart=1./nparts
	nodes=nrmin
	xray=xrmin
	yray=yrmin
	zray=zrmin



	do ip=1,nparts

		!if(indexx(ip).eq.0) cycle

		part1=(ip-1)*dpart
		part2=ip*dpart


		!call remeshing()

		call part_ray(part1,part2)
		!do i=1,nodes
		!	write(*,*)xray(i),yray(i),zray(i)
		!end do
		!pause


!write(*,*)' nodes=',nodes,' nrpart=',nrpart
		call part_bending_hor(ips,0., tini)

!write(*,*)' part=',ip,' tini=',tini,' s=',spart(nrpart)

		tmin=tini
		t_part(ip)=tmin
		!indexx(ip)=0
		!cycle

! Vertical bending:
		do icase=1,2
			val_bend=-(-1)**icase*(val_bend0/nparts)
			ind=1

331			continue
			val = val_bend * ind
			call part_bending_z(ips,val, tout)
			!write(*,*)' ind=',ind,' val_bend=',val_bend
			!write(*,*)' ver shift: ip=',ip,' val=',val,' dt=',tout-tmin

			if(tout.lt.tmin) then
				tmin=tout
				!xrmin=xray
				!yrmin=yray
				!zrmin=zray
				do ipp=1,nrpart
					kod=kod_part(ipp)
					!write(*,*)ipp,kod,dxray(ipp),dyray(ipp),dzray(ipp)
					if(kod.eq.0) cycle
					xrmin(kod)=xray(kod)+dxray(ipp)
					yrmin(kod)=yray(kod)+dyray(ipp)
					zrmin(kod)=zray(kod)+dzray(ipp)
				end do
				ind=ind+1
				goto 331	! Try a larger shift
			else
				if(ind.gt.1) then
					nodes=nrmin
					xray=xrmin
					yray=yrmin
					zray=zrmin
					indexx(ip)=1
					t_part(ip)=tmin
					call part_ray(part1,part2)
					exit
				end if 
			end if
		end do
		!write(*,*)' tmin=',tmin

! Horizontal bending:
		do icase=1,2
			val_bend=(-1)**icase*(val_bend0/nparts)
			ind=1


334			continue
			val = val_bend * ind
			call part_bending_hor(ips,val, tout)
			if(tout.lt.tmin) then
				!write(*,*)ip,' val=',val,' dt=',tout-tmin,' t=',tout
				tmin=tout
				do ipp=1,nrpart
					kod=kod_part(ipp)
					if(kod.eq.0) cycle
					xrmin(kod)=xray(kod)+dxray(ipp)
					yrmin(kod)=yray(kod)+dyray(ipp)
					zrmin(kod)=zray(kod)+dzray(ipp)
				end do
				ind=ind+1
				goto 334		! Try a larger shift
			else
				if(ind.gt.1) then
					nodes=nrmin
					xray=xrmin
					yray=yrmin
					zray=zrmin
					indexx(ip)=1
					t_part(ip)=tmin
					call part_ray(part1,part2)
					exit
				end if 
			end if
		end do		! 2 attempts 

	end do
	ind_tot=0
	ttot=0
	do ip=1,nparts
		!write(*,*)' indexx(ip)=',indexx(ip)
		ttot=ttot+t_part(ip)
		if(indexx(ip).eq.1) ind_tot=1
	end do
		
	aaa=0
	if (aaa.eq.1) then
		open (21,file='ray_path.bln')
		write(21,*)nodes
		write(21,*)0,-zray(1)
		do in=2,nodes
			x1=xray(in)
			y1=yray(in)
			z1=zray(in)
			dd=sqrt((x1-xray(1))**2+(y1-yray(1))**2)
			write(21,*)dd,-z1
		end do
		close(21)
		!pause
	end if


end do

call remeshing()
aaa=0
if (aaa.eq.1) then
	open (21,file='ray_path.bln')
	write(21,*)nodes
	write(21,*)0,-zray(1)
	do in=2,nodes
		x1=xray(in)
		y1=yray(in)
		z1=zray(in)
		dd=sqrt((x1-xray(1))**2+(y1-yray(1))**2)
		write(21,*)dd,-z1
		write(*,*)x1,y1,z1
	end do
	close(21)
	pause
end if


500 continue
!**********************************************************************************************
!trace of the ray part located close to the study area
!**********************************************************************************************


fine_part=dist0 / length
if (fine_part.gt.1.) fine_part=1.0

npart_min=int_best(finedist / ds_bend)
npart_max=npart_max_fine

val_bend0 = val_bend_min * npart_max

do nparts=npart_min,npart_max

543	continue
	indexx=0
	dpart=fine_part/nparts
	nodes=nrmin
	xray=xrmin
	yray=yrmin
	zray=zrmin



	do ip=1,nparts

		!if(indexx(ip).eq.0) cycle

		part1=(ip-1)*dpart
		part2=ip*dpart

		call part_ray(part1,part2)


!write(*,*)' nodes=',nodes,' nrpart=',nrpart
		call part_bending_hor(ips,0., tini)

!write(*,*)' part=',ip,' tini=',tini,' s=',spart(nrpart)

		tmin=tini
		t_part(ip)=tmin

! Vertical bending:
		do icase=1,2
			val_bend=-(-1)**icase*(val_bend0/nparts)
			ind=1

531			continue
			val = val_bend * ind
			call part_bending_z(ips,val, tout)
			!write(*,*)' ind=',ind,' val_bend=',val_bend
			!write(*,*)' ver shift: ip=',ip,' val=',val,' dt=',tout-tmin

			if(tout.lt.tmin) then
				tmin=tout
				do ipp=1,nrpart
					kod=kod_part(ipp)
					if(kod.eq.0) cycle
					xrmin(kod)=xray(kod)+dxray(ipp)
					yrmin(kod)=yray(kod)+dyray(ipp)
					zrmin(kod)=zray(kod)+dzray(ipp)
				end do
				ind=ind+1
				goto 531	! Try a larger shift
			else
				if(ind.gt.1) then
					nodes=nrmin
					xray=xrmin
					yray=yrmin
					zray=zrmin
					indexx(ip)=1
					t_part(ip)=tmin
					call part_ray(part1,part2)
					exit
				end if 
			end if
		end do
		!write(*,*)' tmin=',tmin

! Horizontal bending:
		do icase=1,2
			val_bend=(-1)**icase*(val_bend0/nparts)
			ind=1


534			continue
			val = val_bend * ind
			call part_bending_hor(ips,val, tout)
			if(tout.lt.tmin) then
				do ipp=1,nrpart
					kod=kod_part(ipp)
					if(kod.eq.0) cycle
					xrmin(kod)=xray(kod)+dxray(ipp)
					yrmin(kod)=yray(kod)+dyray(ipp)
					zrmin(kod)=zray(kod)+dzray(ipp)
				end do
				ind=ind+1
				goto 534		! Try a larger shift
			else
				if(ind.gt.1) then
					nodes=nrmin
					xray=xrmin
					yray=yrmin
					zray=zrmin
					indexx(ip)=1
					t_part(ip)=tmin
					call part_ray(part1,part2)
					exit
				end if 
			end if
		end do		! 2 attempts 

	end do
	
end do

call remeshing()

aaa=0
if (aaa.eq.1) then
	open (21,file='ray_path.bln')
	write(21,*)nodes
	write(21,*)0,-zray(1)
	do in=2,nodes
		x1=xray(in)
		y1=yray(in)
		z1=zray(in)
		dd=sqrt((x1-xray(1))**2+(y1-yray(1))**2)
		write(21,*)dd,-z1
		write(*,*)x1,y1,z1
	end do
	close(21)
	pause
end if

! time along the ray
ttt=0
sss=0
depth=0.
do inode=1,nodes-1
	x1=xray(inode)
	x2=xray(inode+1)
	xm=(x1+x2)/2.

	y1=yray(inode)
	y2=yray(inode+1)
	ym=(y1+y2)/2.

	z1=zray(inode)
	z2=zray(inode+1)
	zm=(z1+z2)/2.
	if (z1.gt.depth) depth=z1

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
	!write(*,*)xm,ym,zm,ips
	vvv=velocity (xm,ym,zm,ips)
	!write(*,*)' vvv=',vvv
	ttt=ttt+ds/vvv
	sss=sss+ds

	!write(*,*)' ds=',ds,' sss=',sss
	!write(*,*)' t=',ttt,' s=',sss,' vvv=',vvv
end do

! calculating correction for the Moho depth
x1=xray(1)
x2=xray(2)

y1=yray(1)
y2=yray(2)

z1=zray(1)
z2=zray(2)

dist=sqrt((x1-x2)**2+(y1-y2)**2+(z1-z2)**2)
zsf1=Rz- sqrt(x1**2+y1**2+(Rz-z1)**2)
zsf2=Rz- sqrt(x2**2+y2**2+(Rz-z2)**2)

if ((dist**2-(zsf1-zsf2)**2).lt.epsilon(dist))then
    sinalfa=0.
else
    sinalfa=sqrt(dist**2-(zsf1-zsf2)**2)/dist
end if
vzt=velocity(x1,y1,z1,ips)
px=sinalfa/vzt

!write(*,*)'px=',px
dtcr_st = 0
dtcr_zt = 0
if (depth.gt.avmoho) then

	sig1=1/vcr1_p
	sig2=1/vcr2_p
	if(ips.eq.2) then
		sig1=1/vcr1_s
		sig2=1/vcr2_s
	end if

	sq_sig01=sig1*sig1-px*px
	if(sq_sig01.le.epsilon(sq_sig01)) then
		sq_sig1=0
	else
		sq_sig1=sqrt(sq_sig01)
	end if

	sq_sig02=sig2*sig2-px*px
	if(sq_sig02.le.epsilon(sq_sig02)) then
		sq_sig2=0
	else
		sq_sig2=sqrt(sq_sig02)
	end if

    call decsf(xst,yst,zst,fi0,tet0,fst,tst,zzst)
    call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,zzzt)
	
	if (zst.lt.avmoho) then
	    dh_st = depth_moho(fst,tst)-avmoho
	    dtcr_st= dh_st * (sq_sig1 - sq_sig2)
	else 
	    dtcr_st=0
	end if

	if(zzt.lt.avmoho) then
		dh_zt=depth_moho(fzt,tzt)-avmoho
		dtcr_zt=dh_zt*(sq_sig1 - sq_sig2)
	else 
		dtcr_zt=0
	end if
	!write(*,*)'sq_sig1=',sq_sig1,' sq_sig2=',sq_sig2
	!write(*,*)' dh_st=',dh_st,' dh_zt=',dh_zt
	!write(*,*)' dtcr_st=',dtcr_st,' dtcr_zt=',dtcr_zt
end if


tout=ttt + dtcr_st + dtcr_zt
!write(*,*)' tout=',tout
!if ((tout-100.)*(tout-2000.).gt.0) pause
!pause

return

end
