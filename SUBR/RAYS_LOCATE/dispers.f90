subroutine dispers(dtk,	disp,aver,nk,ank)
PARAMETER (nkrmax=1000)

real dtk(nkrmax)
integer ngood2(nkrmax)
common/loc_param/wgs,res_loc1,res_loc2,dist_limit,n_pwr_dist,ncyc_av,w_P_S_diff
common/krat/nkrat,istkr(nkrmax),tobkr(nkrmax),ipskr(nkrmax),qualkr(nkrmax),trfkr(nkrmax),ngood(nkrmax),alkr(nkrmax),diskr(nkrmax)
common/iprint/iprint
common/tele/kod_tele
!common/nkr_max/nkrmax

!nkrmax=nnkrmax


!n_pwr_dist=1
!ncyc_av=10
!w_P_S_diff=2

ngood=1

! Preliminary average residual :
aver0=0
avw=0
do i=1,nkrat
	!write(*,*)' ips=',ipskr(i),' dt=',dtk(i)
	if(ipskr(i).eq.3) cycle

	ddd=diskr(i)
	if(ddd.lt.dist_limit.or.kod_tele.eq.1) then
		bbb = 1
	else
		bbb = (dist_limit/ddd)**n_pwr_dist
	end if
	aver0=aver0+dtk(i)*bbb
	!write(*,*)' dist=',ddd,' b=',bbb,' dt=',dtk(i)
	avw=avw+bbb
end do
aver0=aver0/avw
!write(*,*)' aver0=',aver0,' dist_limit=',dist_limit
!write(*,*)' res_loc1=',res_loc1,' res_loc2=',res_loc2

! Average residual :
313 continue
do icyc_av=1,ncyc_av
	aver=0
	avw=0
	do i=1,nkrat
		if(ipskr(i).eq.3) cycle

		qqq=qualkr(i)

		ddd=diskr(i)
		if(ddd.lt.dist_limit.or.kod_tele.eq.1) then
			bbb = 1
		else
			bbb = (dist_limit/ddd)**n_pwr_dist
		end if

		dt=(dtk(i)-aver0)

		if(abs(dt).gt.res_loc2) then
			aaa=0
		else if(abs(dt).lt.res_loc1) then
			aaa=1
		else
			aaa=(abs(dt)-res_loc2)/(res_loc1-res_loc2)
		end if

		aver=aver + dt * aaa*bbb*qqq
		avw=avw + aaa*bbb*qqq
		!write(*,*)ddd,bbb,dt,aaa

	end do
	if(avw.lt.0.0001) then

		imax=0
		do ikr1=1,nkrat
			if(ipskr(ikr1).eq.3) cycle
			nk=0
			do ikr2=1,nkrat
				if(abs(dtk(ikr2)-dtk(ikr1)).lt.res_loc2) nk=nk+1
			end do 
			!write(*,*)' dt=',dtk(ikr1),' nk=',nk
			if(nk.lt.imax) cycle
			imax=nk
			ikrmax=ikr1
		end do
		aver0=dtk(ikrmax)
		!write(*,*)' ikrmax=',ikrmax,' aver0=',aver0
		!pause
		goto 313
	end if
	aver=aver/avw
	aver0 = aver0 + aver
	!write(*,*)' aver0=',aver0,' aver/avw=',aver/avw,' avw=',avw
end do
aver=aver0

! Check again the residuals: if there is a good resid. among the rejected ones
ngood=1
nk=0
do i=1,nkrat
	dt=abs(dtk(i)-aver)

	if(abs(dt).gt.res_loc2) then
		ngood(i)=0
	else
		nk=nk+1
	end if
end do
!write(*,*)' nk=',nk,' aver=',aver


848	continue

! Compute ank :

ank=0
wdist=0
do i=1,nkrat


	qqq=qualkr(i)


	dt=abs(dtk(i)-aver)
	if(ipskr(i).eq.3)dt=abs(dtk(i)) 

	if(dt.gt.res_loc2) then
		aaa=0
	else if(dt.lt.res_loc1) then
		aaa=1
	else
		aaa=(dt-res_loc2)/(res_loc1-res_loc2)
	end if


	ddd=diskr(i)

	ccc=1
	if(ipskr(i).eq.3) ccc = w_P_S_diff


	if(ddd.lt.dist_limit.or.kod_tele.eq.1) then
		bbb = 1
	else
		bbb = (dist_limit/(ddd))**n_pwr_dist
	end if

	dank=aaa * bbb * ccc * qqq
	ank = ank + dank
	wdist = wdist + bbb * ccc * qqq
	!write(*,'(2i4,5f9.3)')istkr(i),ipskr(i),dt,aaa,ccc,dank,ank
end do
ank = ank / wdist

! Compute the dispersion :
disp=0
nk=0
do i=1,nkrat
	if(ngood(i).eq.0) cycle
	nk=nk+1
	dt=dtk(i)
	if(ipskr(i).ne.3) dt=(dtk(i)-aver)
	disp=disp+abs(dt)
	!write(*,'(2i4,5f9.3)')istkr(i),ipskr(i),dt
end do
disp=disp/nk
!write(*,*)' disp=',disp

return
end