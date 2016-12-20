subroutine trace_1D(xzt,yzt,zzt,xst,yst,zst,ips, tout)

real dray1(10000),hray1(10000)
real dray2(10000),hray2(10000)
real dtmp(10000),htmp(10000)
real tall(20),hall(20),aall(20)


common/center/fi0,tet0
common/pi/pi,per
common/ray_1D/npath,dray(10000),hray(10000),tmray(10000)
common/ray_param/ds_ini1,ds_ini2,ds_bend,ds_bend1,dist0,val_bend_min
common/ray/ nodes,xray(10000),yray(10000),zray(10000)
common/ray_parameterezation/length,nod1,nod2

Rz=6371.0
dzlay=2.


z1=Rz- sqrt(xzt**2+yzt**2+(Rz-zzt)**2)
z2=Rz- sqrt(xst**2+yst**2+(Rz-zst)**2)
zz1=z1
zz2=z2
xx1=xzt
yy1=yzt
xx2=xst
yy2=yst
if (zz1.lt.zz2) then
	zz1=z2
	zz2=z1
	xx1=xst
	yy1=yst
	xx2=xzt
	yy2=yzt
	kase=1
end if
!write(*,*) 'start point: ',xx1,yy1,zz1,zzt
!write(*,*) 'finish point: ',xx2,yy2,zz2,zst

disthor=sqrt((xzt-xst)**2+(yzt-yst)**2)

call decsf(xzt,yzt,zzt,fi0,tet0,fzt,tzt,hzt)
call decsf(xst,yst,zst,fi0,tet0,fst,tst,hst)
hd1=epic_dist(fzt,tzt,fst,tst)
epidist=Rz*hd1*per

!ezt=epic_dist(fzt,tzt,fi0,tet0)
!dzt=ezt*per

call ref_time(epidist,zz1,ips, time,depth,alfa)
!write(*,*) 'ref_time:', time,depth,alfa


441 continue

vv1=vrefmod(zz1,ips)

sina=sin(alfa*per)
rr1=Rz-zz1
px=rr1*sina/vv1
z_deep=Rz
zsurf=zz2

npath1=0
dcur=0

call halftrace(px,zsurf,zz1,ips,dzlay,time1,dist1,hmax1)
!write(*,*)'up: ',zsurf,zz1,time1,dist1*Rz,hmax1

npath2=npath
dray2=dray
hray2=hray
!write(*,*)'*****************************************'
!write(*,*)npath2
!do i=1,npath2
!	write(*,*)dray2(i),hray2(i)
!end do

dist2=0
if (alfa.lt.90) then
	call halftrace(px,zz1,z_deep,ips,dzlay, time2,dist2,hmax2)
	!write(*,*)'down: ',zz1,z_deep,time2,dist2*rz,hmax2

	npath1=npath1+1
	dray1(npath1)=0
	hray1(npath1)=zz1
	do i=1,npath-1
		npath1=npath1+1
		dray1(npath1)=dray(i)
		hray1(npath1)=hray(i)
	end do

	dist22=hd1*per - dist1

	npath1=npath1+1
	dray1(npath1)=dist22/2.
	hray1(npath1)=hmax2
	do i=npath-1, 1,-1
		npath1=npath1+1
		dray1(npath1)=dist22-dray(i)
		hray1(npath1)=hray(i)
	end do
	dcur=dist22
end if

!write(*,*)'dist_tot=',(dist2*2+dist1)*rz
hd2=(dist2*2 +dist1)/per

npath=npath2
dray=dray2
hray=hray2

do i=npath,1,-1
	npath1=npath1+1
	dray1(npath1)=dcur+(dist1-dray(i))
	hray1(npath1)=hray(i)
end do
npath1=npath1+1
dray1(npath1)=dcur+dist1
hray1(npath1)=zz2

if (kase.eq.1) then
	do i=1,npath1
		dtmp(i)=hd1*per-dray1(npath1-i+1)
		htmp(i)=hray1(npath1-i+1)
		!write(*,*)dray2(i)*rz,hray2(i)
	end do
	dray1=dtmp
	hray1=htmp
end if

!write(*,*)ds_ini1,ds_ini2,dist0

ds_ini=ds_ini2
np=1
dray2(1)=dray1(1)
hray2(1)=hray1(1)
!write(*,*)dray2(1)*rz,hray2(1)


do ii=2,npath1
	d1=dray2(np)*Rz
	z1=hray2(np)
	d2=dray1(ii)*Rz
	z2=hray1(ii)
	!write(*,*)d1,d2,np,ii
	if (d2.le.d1)cycle
	ds=sqrt((d2-d1)**2+(z2-z1)**2)

	if (ds.lt.ds_ini) cycle
	stot=stot+ds
	!write(*,*)ds,stot
	
	if (stot.gt.dist0) ds_ini=ds_ini1
	ns=ds/ds_ini
	if (ns.eq.0) ns=1
	dz=(z2-z1)/ns
	dd=(d2-d1)/ns
	!write(*,*)ds_ini,ds,ns,stot
	do iii=1,ns
		np=np+1
		if (np.gt.10000) then
			write(*,*) 'np > 10000'
			call pause()
			cycle
		end if
		dray2(np)=(d1+iii*dd)/Rz
		hray2(np)=z1+iii*dz
		!write(*,*)dray2(np)*rz,hray2(np)
	end do
end do
if (abs(hray2(np)-hray1(npath1)).gt.0.001) then
	np=np+1
	dray2(np)=dray1(npath1)
	hray2(np)=hray1(npath1)
	!write(*,*)dray2(np)*rz,hray2(np)

end if
npath2=np
!write(*,*)'np=',np

! Conversation to the decart coordinates

sint_zt=sin(tzt*per)
cost_zt=cos(tzt*per)

x1=cos((fst-fzt)*per) * cos(tst*per)
y1=sin((fst-fzt)*per) * cos(tst*per)
z1=sin(tst*per)
!write(*,*)' x1 =',x1,' y1 =',y1,' z1 =',z1

x2=-x1*sint_zt + z1*cost_zt
y2=y1
z2=x1*cost_zt + z1*sint_zt
!write(*,*)' x2 =',x2,' y2 =',y2,' z2 =',z2

sqr=sqrt(x2*x2+y2*y2)
sinf=x2/sqr
cosf=y2/sqr

!write(*,*)' sinf=',sinf,' cosf=',cosf


!write(*,*)' srce:',fzt,tzt,zzt
do ii=1,npath2
	dd=dray2(ii)

	hh=hray2(ii)

	!dd=hd*per

	x3=0
	y3=sin(dd)
	z3=cos(dd)
	!write(*,*)' y3=',y3,' z3=',z3
	!call pause()

	x33=x3*cosf + y3*sinf
	y33=-x3*sinf + y3*cosf
	z33=z3
	!write(*,*)' x33=',x33,' y33=',y33,' z33=',z33

	x4=-x33*sint_zt + z33*cost_zt
	y4=y33
	z4=x33*cost_zt + z33*sint_zt

	!write(*,*)' x4 =',x4,' y4 =',y4,' z4 =',z4

	if (abs(x4/sqrt(x4*x4+y4*y4)).gt.1-epsilon(x4)) then
	    f21=0.
	else 
	    f21=acos(x4/sqrt(x4*x4+y4*y4) )/per
	end if
	!write(*,*)sqrt(x4*x4+y4*y4),x4/sqrt(x4*x4+y4*y4)

	!write(*,*)' f21=',f21
	if(y4.lt.0) f21=360-f21

	fi=f21+fzt
	!write(*,*)' fi=',fi
	if(fi.gt.360) fi=fi-360
	if(fi.lt.-360) fi=fi+360


	tet=ASIN(Z4)/PER

	!write(*,*)' fi=',fi,' tet=',tet,' hh=',hh

	call sfdec(fi,tet,hh,xx,yy,zz,fi0,tet0)

	!write(*,*)'xx=',xx,'yy=',yy,'zz=',zz

	xray(ii)=xx
	yray(ii)=yy
	zray(ii)=zz

end do

!call pause()

! time along the ray
ddd=0
ttt=0
sss=0
nod1=0
nod2=0
nodes=npath2
!write(*,*)'nodes=',nodes
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

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
	!write(*,*)xm,ym,zm,ips
	vvv=velocity (xm,ym,zm,ips)
	!write(*,*)' vvv=',vvv
	ttt=ttt+ds/vvv
	sss=sss+ds
	if(sss.le.dist0) then
		nod1=nod1+1
	else
		nod2=nod2+1
	end if

	!write(*,*)' ds=',ds,' sss=',sss
	!write(*,*)' t=',ttt,' s=',sss,' vvv=',vvv
end do

tout=ttt
length=sss

return
end