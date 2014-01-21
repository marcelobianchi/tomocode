subroutine halftrace(p, z_up, z_bot, ips, dzstep, time,dist,hmax)
common/pi/pi,per


real zzz(8000),rrr(8000),vvv(8000)
real htmp(8000),vtmp(8000)				! ref. models in different zones
real hhmod(8000),vmod(8000)				! ref. models in different zones


common/refmod/nrefmod0,hmod0(600),vmodp0(600),vmods0(600)
common/ray_1D/npath,dray(10000),hray(10000),tray(10000)

rz=6371.

r_bot = rz - z_bot
r_up  = rz - z_up
hmax=z_bot

nrefmod=nrefmod0
if(nrefmod*(nrefmod-601).ge.0.) then
	write(*,*)' nrefmod=',nrefmod
	write(*,*)' velocity model is not ready'
	pause
end if

do i=1,nrefmod
	hhmod(i)=hmod0(i)
	vmod(i)=vmodp0(i)
	if(ips.eq.2)vmod(i)=vmods0(i)
end do

il=1
htmp(il)=hhmod(1)
vtmp(il)=vmod(1)
!write(*,*)' dzstep=',dzstep

do i=2,nrefmod
	h1=hhmod(i-1)
	h2=hhmod(i)
	v1=vmod(i-1)
	v2=vmod(i)
	if(h2-h1.gt.dzstep) then
		nzst=(h2-h1)/dzstep+1
		dzst=(h2-h1)/nzst
		dvst=(v2-v1)/nzst
		do ii=1,nzst
			il=il+1
			htmp(il)=h1+ii*dzst
			vtmp(il)=v1+ii*dvst
			if(h1+ii*dzst .gt. z_bot) then
				il=il+1
				htmp(il)=h2
				vtmp(il)=v2
				goto 739
			end if
		end do
	else
		il=il+1
		htmp(il)=h2
		vtmp(il)=v2
!		write(*,*)il,h2,v2
		if(h2 .gt. z_bot ) goto 739
	end if
end do

739 continue
hhmod=htmp
vmod=vtmp
nrefmod=il

!do i=1,nrefmod
	!write(*,*)hhmod(i),vmod(i)
!end do
!pause
zend=hhmod(nrefmod)

!write(*,*)' z_up=',z_up
v_up=vrefmod(z_up,ips)
r_up=rz-z_up

nref=nrefmod
do i=1,nref
	rrr(i)=rz-hhmod(i)
	vvv(i)=vmod(i)
end do

!do i=1,nref
!	write(*,*)rrr(i),vvv(i)
!end do
!pause
!write(*,*)' ep1=',ep1,' time1=',time1

883 continue

ep2=0.
time2=0.



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute the maximal depth of the ray
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do ilay=1,nref-1
	v1=vvv(ilay)
	v2=vvv(ilay+1)
	r1=rrr(ilay)
	r2=rrr(ilay+1)

	if(r2.gt.r_up) cycle

	!write(*,*)r1,r2,v1,v2

	if(r1.le.r_bot) then
		rmax=r_bot
		goto 3
	end if

	p2=r2/v2
	!write(*,*)' p=',p,' p2=',p2
!	pause
	if(p2.gt.p) cycle
	if(r2.eq.r1.and.p2.lt.p) exit
	aaa=(v1-v2)/(r1-r2)
	bbb=(v2*r1-v1*r2)/(r1-r2)
	rmax=p*bbb/(1.-p*aaa)
	if((r1-rmax)*(r2-rmax).le.0.)goto 3
end do
3 continue

hmax=rz-rmax
!write(*,*)' hmax=',hmax,' z_up=',z_up


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tracing from r_up to r_bot
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
884	continue

npath=0
do il=1,nref-1
	v1=vvv(il)
	v2=vvv(il+1)
	r1=rrr(il)
	r2=rrr(il+1)
!write(*,*)' ini: r1=',r1,' r2=',r2
	if(r2.ge.r_up) cycle
	if(r1.le.rmax) exit
	if(r1.le.r_bot) exit
	if((r1-r_up)*(r2-r_up).lt.0.)then
		v1=v_up
		r1=r_up
	end if
	if((r1-r_bot)*(r2-r_bot).lt.0.)then
		r2=r_bot
		v2=vrefmod(z_bot,ips)
	end if
	if(abs(r2-r1).lt.1.e-8) cycle
!write(*,*)' aft: r1=',r1,' r2=',r2
	!call ibw(r1,r2,v1,v2,p,dt,dep)
	call ray_lay(r1,r2,v1,v2,p,dt,dep)
!	write(*,*)
!	write(*,*)' v1=',v1,' v2=',v2,' p=',p
!	write(*,*)' r1=',r1,' r2=',r2,' dt=',dt,' dep=',dep
	time2=time2+dt
	ep2=ep2+dep
	if((r1-rmax)*(r2-rmax).lt.0.)r2=rmax

	zcur=rz-r2
	npath=npath+1
	dray(npath)=ep2
	hray(npath)=zcur
	tray(npath)=time2
!	write(*,*)zcur,dep,ep2*rz,time2
	if(zcur.gt.z_bot) goto 334

!write(*,*)' dep=',dep,' dt=',dt
!write(*,*)
end do


!write(*,*)' ep2=',ep2,' time2=',time2

334 continue

time=time2
ep=ep2
dist=ep

return
end
