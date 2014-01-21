character*8 ar,re
character*1 it
integer k_bad(2,50000)

!open(1,file='../../VISUAL/area.dat')
!read(1,'(a8)')re
!read(1,'(a8)')ar
!read(1,*) iter
!read(1,*) ngr1,ngr2
!read(1,*) add_perc
!read(1,*) kod_av_bias
!read(1,*) kod_apriori
!read(1,*) 
!ind_srce=0
!read(1,*,end=825) ind_srce
!ind_cut=0
!read(1,*,end=825) ind_cut
!825 close(1)

open(1,file='set.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the model
read(1,*)norm		! norm for dispersion
read(1,*)niter		! Iteration
read(1,*)alim_p
read(1,*)alim_s
close(1)

!open(1,file='../../VISUAL/area.dat')
!read(1,'(a8)')re
!read(1,'(a8)')ar
!read(1,*) iter
close(1)

write(*,*)' ar=',ar,' re=',re,' niter=',niter

k_bad=0
nbad=0

open(21,file='../../FIG_FILES/STAT/'//re//'_'//ar//'.dat')

do iter=0,niter
	write(it,'(i1)')iter
	nzt=0
	nray=0

	dtot_p=0
	dtot_s=0

	ntot_p=0
	ntot_s=0

	open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='binary')
!	open(11,file='../../FIG_FILES/STAT/resid_'//re//'_'//ar//'.dat')
	open(11,file='../../FIG_FILES/STAT/resid_'//re//'_'//ar//'_'//it//'.dat')
	write(*,*)' file=', '../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat'



	! Read the sources:
	872	read(1,end=871)xzt,yzt,zzt,nkrat,key_reloc_local
		nzt=nzt+1
		write(11,*) nzt, xzt, yzt, zzt, nkrat
		!write(*,*) nzt, xzt, yzt, zzt, nkrat
		nray=nray+nkrat
		ddd_p=0
		nnn_p=0
		ddd_s=0
		nnn_s=0
		do ikr=1,nkrat
			read(1)ist,ips,tobs,tmod
			dt=tobs-tmod
			!write(*,*)ips,ist,dt,tobs,tmod
			write(11,'(2i4,3f8.3,i4)')ips,ist,dt,tobs,tmod,nzt
! for first iter: remove too big reiduals from the results
			if(iter.eq.1) then
				alim=alim_p
				if(ips.eq.2) alim=alim_s
				if(abs(dt).gt.alim) then
					nbad=nbad+1
					k_bad(1,nbad)=nzt
					k_bad(2,nbad)=ikr
					cycle
				end if
			else
				do i=1,nbad
					if(k_bad(1,i).eq.nzt.and.k_bad(2,i).eq.ikr) goto 874
				end do
			end if
			!write(*,*)' ips=',ips,' dt=',dt
! two different ways to calculate the the variance (define which one to use in the set.dat file)
			if(ips.eq.1) then
				if(norm.eq.2) then
					ddd_p = ddd_p + dt*dt
				else if(norm.eq.1) then
					ddd_p = ddd_p + abs(dt)
				end if
				nnn_p = nnn_p + 1
			else
				if(norm.eq.2) then
					ddd_s = ddd_s + dt*dt
				else if(norm.eq.1) then
					ddd_s = ddd_s + abs(dt)
				end if
				nnn_s = nnn_s + 1
			end if
874			continue
		end do
		dcur_p=0
		dcur_s=0
		if(norm.eq.2) then
			dcur_p=sqrt(ddd_p/nnn_p)
			if(nnn_s.ne.0) dcur_s=sqrt(ddd_s/nnn_s)
		else if(norm.eq.1) then
			dcur_p=ddd_p/nnn_p
			if(nnn_s.ne.0) dcur_s=ddd_s/nnn_s
		end if

		write(11,*)' dcur_p=',dcur_p,' dcur_s=',dcur_s

		!write(*,*)' dcur_p=',dcur_p,' nnn_p=',nnn_p
		!write(*,*)' dcur_s=',dcur_s,' nnn_s=',nnn_s

		
		dtot_p=dtot_p+ddd_p
		ntot_p=ntot_p+nnn_p
		dtot_s=dtot_s+ddd_s
		ntot_s=ntot_s+nnn_s
		goto 872
871	close(1)
	close(11)
	if(norm.eq.2) then
		dtot_p=sqrt(dtot_p/ntot_p)
		dtot_s=sqrt(dtot_s/ntot_s)
	else if(norm.eq.1) then
		dtot_p=(dtot_p/ntot_p)
		dtot_s=(dtot_s/ntot_s)
	end if
	if(iter.eq.1) then
		write(*,*)' nbad=',nbad
		dtot1_p=dtot_p
		dtot1_s=dtot_s
	end if
	red_p=100*(dtot1_p-dtot_p)/dtot1_p
	red_s=100*(dtot1_s-dtot_s)/dtot1_s
	write(*,*)'Nray=', nray, 'Nzt=',nzt
	write(*,*)' iter=',iter,' dtot_p=',dtot_p,' red=',red_p
	write(*,*)' iter=',iter,' dtot_s=',dtot_s,' red=',red_s
	write(*,*)'___________________________________________________'
	write(21,*)' iter=',iter,' dtot_p=',dtot_p,' red=',red_p
!	write(21,*)' iter=',iter,' dtot_s=',dtot_s,' red=',red_s
!	write(21,*)'___________________________________________________'


end do
close(21)
close(11)
stop
end
