subroutine read_crust(nmodel)
character*2 cr_type(2000),cr_t,crt_ft(180,90)
character*8 cinf
real d_crust(2000)


common/crust1/fa1,fb1,ta1,tb1,dfi1,dtet1,nfi1,ntet1,dcr_1(360,180)
common/crust_local/floc1,floc2,tloc1,tloc2,dfloc,dtloc,nfloc,ntloc, dcr_loc(360,180)
common/crust_model/nmod

nfi1=0
ntet1=0

if(nmodel.eq.0) then
	return
else if(nmodel.eq.2.or.nmodel.eq.3) then


	! read crust 2x2:
	open(1,file='../../DATA/COMMON/CRUST/cr_2/CNtype2_key.txt')
	do i=1,5
		read(1,*)
	end do
	icr=0
	1	read(1,*,end=2)cr_t
		icr=icr+1
		cr_type(icr)=cr_t
		do i=1,3
			read(1,*,end=2)
		end do
		read(1,*,end=2)(aa,i=1,7),cinf,dcr
		d_crust(icr)=dcr
		!write(*,*)icr,cr_t,dcr
		goto 1
	2 close(1)
	ncr=icr
	!write(*,*)' ncr=',ncr

	fa1=-179
	fb1=179
	dfi1=2
	nfi1=180
	ta1=-89
	tb1=89
	dtet1=2
	ntet1=90
	open(1,file='../../DATA/COMMON/CRUST/cr_2/CNtype2.txt')
	read(1,*)
	do itet=ntet1,1,-1
		read(1,'(i4,180(3x,a2))')ii,(crt_ft(ifi,itet),ifi=1,nfi1)
		do ifi=1,nfi1
			do icr=1,ncr
				if(cr_type(icr).eq.crt_ft(ifi,itet)) goto 5
			end do
			write(*,*)' cannot find: crt_ft(ifi,itet)=',crt_ft(ifi,itet)
			call pause()
	5		continue
			dcr_1(ifi,itet)=d_crust(icr)
			!write(*,*)' cr=',crt_ft(ifi,itet),' d=',dcr_2(ifi,itet)
			!call pause()
		end do
	end do
	close(1)
else if(nmodel.eq.1) then

	! read crust 1x1:

	open(1,file='../../DATA/COMMON/CRUST/cr_1/crust_1_1.dat')
	read(1,*)fa1,fb1,dfi1,nfi1
	read(1,*)tb1,ta1,dtet,ntet1
	dtet1=-dtet
	read(1,*)((dcr_1(ifi,itet),ifi=1,nfi1),itet=ntet1,1,-1)
	close(1)


else
	write(*,*)' nmodel=',nmodel
	call pause()
end if

if (nmodel.eq.3) then

!read local crust model

    open(1, file='../../DATA/COMMON/CRUST/local/crust.dat')
        read(1,*)floc1,floc2,dfloc,nfloc
        read(1,*)tloc1,tloc2,dtloc,ntloc
        read(1,*)((dcr_loc(ifi,itet),ifi=1,nfloc),itet=1,ntloc)
        !write(*,*)((dcr_loc(ifi,itet),ifi=1,nfloc),itet=1,ntloc)
    close(1)
    
end if

    

!write(*,*)fa1,fb1,dfi1,nfi1
!write(*,*)ta1,tb1,dtet1,ntet1


return
end
