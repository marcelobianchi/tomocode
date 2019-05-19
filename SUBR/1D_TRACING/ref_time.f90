subroutine ref_time(dist,depzt,ips, time,depth,alfa)
! Calculation of ray parameters in a reference model 
! Input: depz : depth of source and 
! distkm: epicentral distance  in km

common/reftable/iztref,nref(2,200),hztref(2,200),tref(2,200,5000),rref(2,200,5000),eref(2,200,5000),aref(2,200,5000)
common/deriv/dtdd,dtdz
common/pi/pi,per

time=-900
depth=-900
alfa=-900
!write(*,*)depzt,hztref(ips,iztref)
!pause
if(depzt.gt.hztref(ips,iztref)) then
        write(*,*) 'Warning on reftime (',depzt,hztref(1,iztref),').'
        return
endif

if(depzt.lt.hztref(ips,1)) then
	ih=1
	h1=hztref(ips,1)
	h2=hztref(ips,2)
else
	do ih=1,iztref-1
		h1=hztref(ips,ih)
		h2=hztref(ips,ih+1)
		if((depzt-h1)*(depzt-h2).le.0.) exit
	end do
end if



hd=dist

e1=eref(ips,ih,nref(ips,ih))
!write(*,*)' distkm=',distkm,' hd=',hd,' e1=',e1
if(hd.gt.e1) return
e2=eref(ips,ih+1,nref(ips,ih+1))
!write(*,*)' distkm=',distkm,' hd=',hd,' e2=',e2
if(hd.gt.e2) return
!pause

!write(*,*)' eref=',eref(ips,ih,nref(ips,ih)),eref(ips,ih+1,nref(ips,ih+1))
!write(*,*)' nref=',nref(ips,ih),nref(ips,ih+1)

if(hd.gt.eref(ips,ih,nref(ips,ih))) return
if(hd.gt.eref(ips,ih+1,nref(ips,ih+1))) return

! Compute standart travel time and depth of the ray turning

tmin=10000000.
do i=2,nref(ips,ih)
	e1=eref(ips,ih,i-1)
	e2=eref(ips,ih,i)
	if(e1.gt.hd+3.)exit
	if(hd.le.e1.or.hd.gt.e2)cycle
	t1=tref(ips,ih,i-1)
	t2=tref(ips,ih,i)
	r1=rref(ips,ih,i-1)
	r2=rref(ips,ih,i)
	a1=aref(ips,ih,i-1)
	a2=aref(ips,ih,i)
	al1=a1+((a2-a1)/(e2-e1))*(hd-e1)
	tm1=t1+((t2-t1)/(e2-e1))*(hd-e1)
	rturn1=r1+((r2-r1)/(e2-e1))*(hd-e1)
	if(tm1.lt.tmin) then
		tmin=tm1
		amin=al1
		rmin=rturn1
		dtdd1=(t2-t1)/((e2-e1)*6371.*per)
	end if
	cycle
end do
tm1=tmin
al1=amin
rturn1=rmin

tmin=10000000.
do i=2,nref(ips,ih+1)
	e1=eref(ips,ih+1,i-1)
	e2=eref(ips,ih+1,i)
	if(e1.gt.hd+3.)exit
	if(hd.le.e1.or.hd.gt.e2)cycle
	t1=tref(ips,ih+1,i-1)
	t2=tref(ips,ih+1,i)
	r1=rref(ips,ih+1,i-1)
	r2=rref(ips,ih+1,i)
	a1=aref(ips,ih+1,i-1)
	a2=aref(ips,ih+1,i)
!	write(*,*)' e1=',e1,' e2=',e2
!	write(*,*)' t1=',t1,' t2=',t2
	al2=a1+((a2-a1)/(e2-e1))*(hd-e1)
	tm2=t1+((t2-t1)/(e2-e1))*(hd-e1)
	rturn2=r1+((r2-r1)/(e2-e1))*(hd-e1)
	if(tm2.lt.tmin) then
		tmin=tm2
		amin=al2
		rmin=rturn2
		dtdd2=(t2-t1)/((e2-e1)*6371.*per)
	end if
	cycle
end do
tm2=tmin
al2=amin
rturn2=rmin

!write(*,*)' al1=',al1,' al2=',al2
!write(*,*)' tm1=',tm1,' tm2=',tm2
time=tm1+((tm2-tm1)/(h2-h1))*(depzt-h1)
alfa=al1+((al2-al1)/(h2-h1))*(depzt-h1)
depth=rturn1+((rturn2-rturn1)/(h2-h1))*(depzt-h1)
dtdd=dtdd1+((dtdd2-dtdd1)/(h2-h1))*(depzt-h1)
dtdz=(tm2-tm1)/(h2-h1)
!write(*,*)'alfa= ',alfa
!write(*,*)' time=',time

return
end