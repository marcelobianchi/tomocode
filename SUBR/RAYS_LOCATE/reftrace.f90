subroutine reftrace(alfa,zzt,zstat,ips,izgib, time,dist,hmax)


! zstat : starting level
! alfa : dipping angle at the starting level
! zzt : finishing level. It is always above that zstart
! izgib : marker. When 0, the ray goes


real zzz(300),rrr(300),vvv(300)
real hmod(600),vmodp(600),vmods(600) ! ref. models in different zones

common/refmod/nrefmod,hmod,vmodp,vmods
common/pi/pi,per

!write(*,*)alfa,zstat,zzt,izgib



rz=6371.

dist=-999.
time=-999.

if (alfa.gt.90..and.zzt.le.zstat) then
	hmax=zzt
	return
end if


rstat=rz-zstat
vstat=vrefmod(zstat,ips)
vzt=vrefmod(zzt,ips)
rzt=rz-zzt

zup=zstat
vup=vstat
zlow=zzt
vlow=vzt

if(zzt.lt.zstat) then
	zup=zzt
	vup=vzt
	zlow=zstat
	vlow=vstat
end if

rup=rz-zup
rlow=rz-zlow


nref=nrefmod+1
do i=1,nref-1
	rrr(i)=rz-hmod(i)
	vvv(i)=vmodp(i)
	if(ips.eq.2) vvv(i)=vmods(i)
end do
rrr(nref)=0
vvv(nref)=15.

!do i=1,nref
!	write(*,*)rrr(i),vvv(i)
!end do

sina=sin(alfa*per)

p=rzt*sina/vzt

!write(*,*)' p=',p,' alfa=',alfa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tracing from rup to rlow
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!write(*,*)' rup=',rup,' rlow=',rlow

ep1=0.
time1=0.

if(abs(alfa-90).lt.1.e-9) then
	rmax=rlow
	hmax=rz-rmax
	rlow=rup
	vlow=vup
	goto 883
end if


if(abs(rup-rlow).lt.1.e-9) goto 883

do il=1,nref-1
	v1=vvv(il)
	v2=vvv(il+1)
	r1=rrr(il)
	r2=rrr(il+1)
!write(*,*)' ini: r1=',r1,' r2=',r2
	if(r2.gt.rup) cycle
	if(r1.le.rlow) exit
	if((r1-rup)*(r2-rup).lt.0.)then
		v1=vup
		r1=rup
	end if
	if((r1-rlow)*(r2-rlow).le.0.)then
		v2=vlow
		r2=rlow
	end if
!write(*,*)' aft: r1=',r1,' r2=',r2
	p2=r2/v2
	sina2=p*v2/r2
	!p2=r1/v1
	if(r2.eq.r1.and.p2.gt.p) cycle
	if(r2.eq.r1.and.p2.le.p) then
		hmax=rz-r2
		exit
	end if
	call ray_lay(r1,r2,v1,v2,p,dt,dep)
	!if(il.eq.16)write(*,*)' h1=',rz-r1,' h2=',rz-r2
	!if(il.eq.16)write(*,*)' v1=',v1,' v2=',v2,' p=',p
	!write(*,*)il,' dt=',dt,' dep=',dep
	time1=time1+dt
	ep1=ep1+dep
	!if(p2.le.p) then
	!if (sina2.ge.1) then
	!	aaa=(v1-v2)/(r1-r2)
		!bbb=(v2*r1-v1*r2)/(r1-r2)
		!hmax=rz-p*bbb/(1.-p*aaa)
		!write(*,*)' h1=',rz-r1,' h2=',rz-r2
		!write(*,*)' v1=',v1,' v2=',v2
		!write(*,*)' p=',p,' p2=',p2
		!write(*,*)' hmax=',hmax, 'sina2=',sina2
		!return
	!end if
	!write(*,*)' il=',il,' h1=',rz-r1,' h2=',rz-r2
	!if(il.eq.16)write(*,*)il,v1,v2,ep1*rz,dep*rz
	!pause
end do
!write(*,*)' ep1=',ep1,' time1=',time1

883 continue

ep2=0.
time2=0.
hmax=zlow

if(alfa.gt.90+1.e-9)goto 334
if (izgib.eq.0.and.zstat.gt.zzt) goto 334


!write(*,*)' rup=',rup,' rlw=',rlow
!write(*,*)' vup=',vup,' vlw=',vlow


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Compute the maximal depth of the ray
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do ilay=1,nref-1
	v1=vvv(ilay)
	v2=vvv(ilay+1)
	r1=rrr(ilay)
	r2=rrr(ilay+1)
	p2=r2/v2
	sina1=p*vzt/rzt
	sina2=p*v2/r2
	!write(*,*)ilay,p2,p,v1,v2
	!write(*,*)r1,r2
	!write(*,*)sina2,sina1
	!if(p2.gt.p) cycle
	!if(r2.eq.r1.and.p2.lt.p) exit
	if (sina2.lt.1) cycle
	if (r2.eq.r1.and.sina2.gt.1) exit
	aaa=(v1-v2)/(r1-r2)
	bbb=(v2*r1-v1*r2)/(r1-r2)
	rmax=p*bbb/(1.-p*aaa)
	if((r1-rmax)*(r2-rmax).le.0.)goto 3
end do
ilay=ilay-1
3 continue
hmax=6371.-rmax
vmax=vrefmod(hmax,ips)
!write(*,*)'hmax= ',hmax
!pause



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Tracing from rlow to rmax
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
884	continue

do il=1,nref-1
	v1=vvv(il)
	v2=vvv(il+1)
	r1=rrr(il)
	r2=rrr(il+1)
!write(*,*)' ini: r1=',r1,' r2=',r2
	if(r2.ge.rlow) cycle
	if(r1.le.rmax) exit
	if((r1-rlow)*(r2-rlow).lt.0.)then
		v1=vlow
		r1=rlow
	end if
	if ((r1-rmax)*(r2-rmax).lt.0.)then
		v2=vmax
		r2=rmax
	end if
	if(r2.eq.r1) cycle
!write(*,*)' aft: r1=',r1,' r2=',r2
	call ray_lay(r1,r2,v1,v2,p,dt,dep)
	!write(*,*)' v1=',v1,' v2=',v2,' p=',p
	!write(*,*)' r1=',r1,' r2=',r2,' dt=',dt,' dep=',dep
	time2=time2+dt
	ep2=ep2+dep
!write(*,*)' dep=',dep,' dt=',dt
!write(*,*)
end do

!write(*,*)' ep2=',ep2,' time2=',time2

334 continue

time=time2*2.+time1
ep=ep2*2.+ep1
if(abs(alfa-90).lt.1.e-9)ep=ep2
if(abs(alfa-90).lt.1.e-9)time=time2
dist=ep*rz

!write(*,*)' time1=',time1,' ep1=',ep1
!write(*,*)' time2=',time2,' ep2=',ep2
!write(*,*)' alfa=',alfa,' dist=',dist,' time=',time

!pause
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!write(*,*)' time=',time,' dist=',dist
return
end


      SUBROUTINE IBW_old(R2,R1,V2,V1,P,T,D)
!     This subroutine computes transit times and delta increase
!     of a ray travelling from radius R2 down to R1 (or to the
!     turning point Rt if R1&lt;Rt&lt;R2), for linear velocity behaviour
!     from V2 (at R2) to V1. Slowness is P. Output: time T (sec)
!     and distance D (rad). Velocity in km/sec, radii in km.
!     Equations from G.Nolet, Linearized inversion of (teleseismic)
!     data. In: R.Cassinis,'The inverse problem in geophysical
!     interpretation',Plenum Press,NY,1981.
      T=0.
      D=0.
!     return if S wave in fluid layer
      IF(V1*V2.EQ.0.) RETURN
      SINT2=P*V2/R2
      IF(SINT2.GT.1.0) RETURN
      A=(V2-V1)/(R2-R1)
      IF(P.EQ.0.) GO TO 40
      AP=A*P
      if(r1.gt.0.) then
        SINT1=P*V1/R1
      else
        sint1=1.0
      endif
      IF(SINT1.GT.1.) SINT1=1.
      TETA1=ASIN(SINT1)
      TETA2=ASIN(SINT2)
      HT1=.5*TETA1
      HT2=.5*TETA2
      TGT1=TAN(HT1)
      TGT2=TAN(HT2)
      Q1=SNTET(AP,TETA1,TGT1)
      Q2=SNTET(AP,TETA2,TGT2)
      IF(AP.EQ.0.) GO TO 20
      T=(log(TGT2)-log(TGT1)-Q2+Q1)/A
      GO TO 30
   20 C1=1./TAN(TETA1)
      C2=1./TAN(TETA2)
      T=P*(C2-C1)
   30 D=TETA1-TETA2-AP*(Q2-Q1)

		write(*,*)' ap=',ap
		write(*,*)' q1=',q1,' q2=',q2,' q1-q2=',q1-q2
		write(*,*)' d=',d
      RETURN
  40  IF(A.EQ.0.) GO TO 50
      T=(log(A*(R2-R1)+V1)-log(V1))/A
      RETURN
   50 T=(R2-R1)/V1
      RETURN
      END
!--------------------------------------------------------------
      FUNCTION SNTET (AP,TETA,TGT)
		common/pi/pi,per
!     Computes value of J(teta)
      IF(AP.EQ.0.) GO TO 40
      AP2=AP*AP
      IF(AP2-1.)10,20,30
   10 ALFA=ACOS(-AP)
      SQAP=SQRT(1.-AP2)
      SNTET=-.5*log((1.+SIN(ALFA+TETA))/(1.-SIN(ALFA-TETA)))/SQAP
	  write(*,*)' case 10'
	  sinat=SIN(ALFA-TETA)
	  write(*,*)' ALFA-TETA=',(alfa-teta)/per
	  write(*,*)' (1.+SIN(ALFA+TETA))/(1.-SIN(ALFA-TETA))=',(1.+SIN(ALFA+TETA))/(1.-SIN(ALFA-TETA))
	  write(*,*)' alfa=',alfa/per,' teta=',teta/per
	  write(*,*)' sntet=',sntet,' sqap=',sqap
      RETURN
   20 SNTET=TAN(.785398132+AP*.5*TETA)
	  write(*,*)' case 20'
      RETURN
   30 SQAP=SQRT(AP2-1.)
	  write(*,*)' case 30'
      APTG=-AP*TGT+1.
      SNTET=2.*ATAN(APTG/SQAP)/SQAP
      RETURN
   40 SNTET=0.
	  write(*,*)' case 40'
      RETURN
      END
!------------------------------------------------------------------
      SUBROUTINE CDELT(SLAT,SLON,ELAT,ELON,DEL)
      COLAT(A)=1.57079632-ATAN(.993277*TAN(A))
!     Calculates epicentral distance in degrees
      SCOLAT=COLAT(SLAT)
      ECOLAT=COLAT(ELAT)
      CODEL=SIN(SCOLAT)* SIN(ECOLAT)*(COS(ELON)*COS(SLON)+&
      SIN(ELON)*SIN(SLON))+COS(SCOLAT)*COS(ECOLAT)
      DEL=ACOS(CODEL)
      RETURN
      END
!--------------------------------------------------------------
