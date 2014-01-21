subroutine ray_lay(r2,r1,v2,v1,p,time,dist)
!     This subroutine computes transit times and delta increase
!     of a ray travelling from radius R2 down to R1 (or to the
!     turning point Rt if R1&lt;Rt&lt;R2), for linear velocity behaviour
!     from V2 (at R2) to V1. Slowness is P. Output: time T (sec)
!     and distance D (rad). Velocity in km/sec, radii in km.
!     Equations from G.Nolet, Linearized inversion of (teleseismic)
!     data. In: R.Cassinis,'The inverse problem in geophysical
!     interpretation',Plenum Press,NY,1981.
common/pi/pi,per
rz=6371

rr=(r2-r1)/r1
vv=(v2-v1)/v1
kod_sing=0
!if(abs(vv/rr)-1..lt.0.01) then
!	kod_sing=1
!	rzz=rz+1000.
!	p = p * rzz / rz
!	r1=r1+1000.
!	r2=r2+1000.
!end if
!write(*,*)' kod_sing=',kod_sing


time=0
dist=0
if(abs(v1*v2).lt.0.000001) return

sint2=p*v2/r2
if(sint2.gt.1.) return

dvdr=(v2-v1)/(r2-r1)

if(abs(p).lt.0.000001) then
	if(abs(v2-v1).gt.0.000001) then
		time=(log(v2)-log(v1))/dvdr
	else
		time=(r2-r1)/v1
	end if
	return
end if

ap = dvdr*p
if(r1.gt.0.0000001) then
	sint1=p*v1/r1
else
	sint1=1.
end if

if(sint1.gt.1.) sint1=1

teta1=asin(sint1)
teta2=asin(sint2)

if (abs(v2-v1).lt.0.000001) then
	time= r2*sin(teta1-teta2)/(sin(teta1)*v1)
	dist=teta1-teta2
	!write(*,*)time,dist
	!pause
	if(kod_sing.eq.1) then
		dist=dist*rzz/rz
		p = p * rz / rzz
		r1=r1-1000.
		r2=r2-1000.
	end if

	return
end if

tgt1=TAN(teta1 / 2.)
tgt2=TAN(teta2 / 2.)

if(ap.eq.0.) then
	sntet=0
	c1=1. / TAN(TETA1)
	c2=1. / TAN(TETA2)
	time=p*(c2-c1)
else
	ap2=ap*ap
	sqap=SQRT(abs(1.-ap2))
	if(ap2.eq.1.) then
		sntet1=TAN(.785398132 + ap * .5 * teta1)
		sntet2=TAN(.785398132 + ap * .5 * teta2)
	else if(ap2.lt.1.) then
		alfa=acos(-ap)
		sntet1=-.5*log((1.+SIN(alfa+teta1))/(1.-SIN(alfa-teta1)))/sqap
		sntet2=-.5*log((1.+SIN(alfa+teta2))/(1.-SIN(alfa-teta2)))/sqap
	else if(ap2.gt.1) then
		aptg1 = -ap * tgt1 + 1.
		aptg2 = -ap * tgt2 + 1.
		sntet1=2.*ATAN(aptg1/sqap)/sqap
		sntet2=2.*ATAN(aptg2/sqap)/sqap
	end if
	time = (log(tgt2) - log(tgt1) - sntet2 + sntet1) / dvdr
	dist = teta1-teta2-ap*(sntet2-sntet1)
end if

if(kod_sing.eq.1) then
	dist=dist*rzz/rz
	p = p * rz / rzz
	r1=r1-1000.
	r2=r2-1000.
end if


return
end