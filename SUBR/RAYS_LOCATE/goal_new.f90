subroutine	goal_new(xzt,yzt,zzt, disp,aver,nk,ank)

real tall(30),hall(30),aall(30)
real trfkr1(500),alkr1(500)
real dtk(500),dtk1(500),dtall(20,500),trfal(20,500),alall(20,500),dtmp(500)
integer kmin(500),nall(500),ngmin(500)

common/krat/nkrat,istkr(500),tobkr(500),ipskr(500),qualkr(500),trfkr(500),ngood(500),alkr(500),diskr(500)
common/iprint/iprint
common/center/f0,t0
common/stations/ xstat(9000),ystat(9000),zstat(9000)

common/loc_param/wgs,res_loc1,res_loc2,dist_limit,n_pwr_dist,ncyc_av,w_P_S_diff


one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0
Rz=6371.0
!write(*,*)' resmax=',res_max_loc


!dhzt=dh_crust_sm(xzt,yzt)
!write(*,*)' dhzt=',dhzt,' zzt=',zzt
!pause
!if(dhzt.ne.0.)pause


dist=sqrt(xzt**2+yzt**2)
zztsf=Rz - sqrt(dist**2+(Rz-zzt)**2)
if(zztsf.lt.0.) zztsf=0.

vzt1=vrefmod(zztsf,1)
vzt2=vrefmod(zztsf,2)
vst1=vrefmod(0.,1)
vst2=vrefmod(0.,2)
rzt=rz-zzt
rst=rz

do ikr=1,nkrat
	ips=ipskr(ikr)
	ist=istkr(ikr)
	xst=xstat(ist)
	yst=ystat(ist)
	zst=zstat(ist)

	dist=sqrt(xst**2+yst**2)
	zstsf=Rz - sqrt(dist**2+(Rz-zst)**2)

	vzt=vzt1
	vst=vst1
	if(ips.eq.2) then
		vzt=vzt2
		vst=vst2
	end if

	call decsf(xzt,yzt,zzt,f0,t0,fzt,tzt,hzt)
	call decsf(xst,yst,zst,f0,t0,fst,tst,hst)

	!write(*,*)xzt,yzt,zzt,f0,t0,fzt,tzt,hzt
	!write(*,*)xst,yst,zst,f0,t0,fst,tst,hst

	epi=epic_dist(fzt,tzt,fst,tst)
	!write(*,*)'epi=',epi
	dist=epi*per*Rz
	!write(*,*)'dist=',dist
	!dist=sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
	diskr(ikr)=sqrt(dist*dist+(zzt-zst)**2)

	if(diskr(ikr).lt.0.01) then
		tttt=0
		aaaa=90
		goto 441
	end if
	!write(*,*)' ikr=',ikr

	call refmod_all(dist,zztsf,ips, nal,tall,hall,aall)
	

	!write(*,*)' dis=',dist,(tall(i),i=1,nal)
!	pause


! Select the first arrival
	tmin=999999
	do ii=1,nal
		if(tall(ii).gt.tmin) cycle
		tmin=tall(ii)
		imin=ii
	end do
	hhhh=hall(imin)
	aaaa=aall(imin)
	tttt=tmin
	!write(*,*)'refmod_all:',tttt,aaaa,hhhh

	!write(*,*)dist,zztsf,ips
	!call ref_time(dist,zztsf,ips, tttt,hhhh,aaaa)
	!write(*,*)'ref_time:',tttt,aaaa,hhhh
	!pause


441	continue

	px=sin(aaaa*per)/vzt
	alkr(ikr)=aaaa
	!write(*,*)' px=',px,' aaaa=',aaaa

! Correction for the station elevation	
	slowst=1/vst
	sqr=slowst*slowst-px*px
	if(sqr.le.0) then
		dtstat=0
	else
		dtstat=-zstsf*sqrt(sqr)
	end if
	!write(*,*)' zst=',zst,vst,' px=',px,' dtstat=',dtstat
	!pause
	tttt = tttt + dtstat

	trfkr(ikr)=tttt
	alkr(ikr)=aaaa
	!write(*,*)' dis=',diskr(ikr),ipskr(ikr),' tob=',tobkr(ikr),' trf=',trfkr(ikr)
	dtk(ikr)=(tobkr(ikr)-tttt)
	!pause

end do

do ikr=1,nkrat
	if(ipskr(ikr).eq.1) cycle
! 3 means that a differential time Tp-Ts is used
	do ik2=1,nkrat
		if(ipskr(ik2).eq.2) cycle
		if(istkr(ik2).ne.istkr(ikr)) cycle
		ipskr(ikr) = 3
		ikr2=ik2
		dtk(ikr)=dtk(ikr)-dtk(ikr2)
		goto 771
	end do
771	continue
end do
!do ikr=1,nkrat
!	write(*,*)' ips=',istkr(ikr),ipskr(ikr),' dis=',diskr(ikr),' dt=',dtk(ikr)
!end do
call dispers(dtk, disp,aver,nk,ank)
!write(*,*)' ank=',ank
!pause

do i=1,nkrat
	if(ipskr(i).eq.3) ipskr(i)=2
end do

return
end