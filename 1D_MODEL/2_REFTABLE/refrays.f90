character*8 ar,re,line
character*1 rm,it
real tref(100000),dref(100000),alref(100000),href(100000)
real hmod(600),vmodp(600),vmods(600) ! ref. models in different zones
real zst(20),dzst(20),dst(10),ddist(10)

common/refmod/nrefmod,hmod,vmodp,vmods
common/pi/pi,per


one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0
rz=6371.

open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter		! code of the area
close(1)
write(it,'(i1)')iter-1

write(*,*)' rg=',re,' ar=',ar,' it=',it		! code of the area


open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/1Dmod_PARAM.DAT')
do i=1,10000
	read(1,*,end=553)line
	if(line.eq.'REF_PARA') goto 554
end do
553 continue
write(*,*)' cannot find REF_PARAM in 1Dmod_PARAM.DAT!!!'
call pause()


554 read(1,*)
read(1,*)depmax		!=100.
read(1,*)distmax		!=2000.
read(1,*)nstd
do i=1,nstd
	read(1,*)dst(i),ddist(i)
end do
read(1,*)nstep		
do i=1,nstep
	read(1,*)zst(i),dzst(i)
end do
read(1,*)zztmax		!=50.
close(1)

if(iter.eq.1) then
	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ref_start.dat')
else
	open(1,file='../../DATA/'//re//'/'//ar//'/RESULT/ref'//it//'.dat')
end if
read(1,*,end=81)vpvs
i=0
82	i=i+1
	read(1,*,end=81)hmod(i),vmodp(i),vs
	if(vpvs.lt.0.000001) then
		vmods(i)=vs
	else
		vmods(i)=vmodp(i)/vpvs
	end if
	!write(*,*)hmod(i),vmodp(i),vmods(i)
goto 82
81	close(1)
nrefmod=i-1
write(*,*)' nrefmod=',nrefmod

dzzt=dzst(1)
zout=0
zzt=zst(1)-dzzt
izt=0
dmin=ddist(1)


open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/table.dat',form='unformatted')
!	do zzt=zztmin,zztmax,dzzt
34	zzt=zzt+dzzt
	if(zzt.gt.zztmax) goto 35
	izt=izt+1
	if(nstep.gt.1) then
		do istep=2,nstep
			if(zzt.gt.zst(istep)) dzzt=dzst(istep)
		end do
	end if
!	zzt=10

!if(izt.ne.17) goto 34	
	
	do ips=1,2
		dlast=999
		nref=0
		!open(31,file='tmp.dat')
		do alfa0=180, 0.d0, -0.005d0
		!do alfa0=89.986, 0.d0, -0.002d0
			call reftrace(alfa0,zzt,zout,ips,1,  time,dist,hmax)
			if (time.gt.0) then
				!write(*,*)alfa0,dist,time,hmax
				!call pause()
			end if
			dgrad=(dist/rz)/per
			!call pause()
		!	if (hmax.ge.hmod(nrefmod-1)) exit
			if(dist.lt.0.) cycle
			dkm=dist
			if (nstd.gt.1) then
				do istep=2,nstd
					if (dkm.gt.dst(istep)) dmin=ddist(istep)
				end do
			end if
			!write(*,*)'dmin=',dmin
			if(abs(dkm-dlast).gt.dmin) then
				nref=nref+1
				tref(nref)=time
				dref(nref)=dkm
				if(nref.eq.1)dref(nref)=0
				alref(nref)=alfa0
				href(nref)=hmax
				dlast=dkm
				!write(*,*)nref,alfa0,dist,time,hmax
				!call pause()
				!write(31,*)nref,alfa0,dkm,time,hmax
			end if
			if (hmax.gt.depmax)exit 
			if (dist.gt.distmax)exit 
				
		end do
		!close(31)
		write(11)zzt,nref
		if(mod(izt,10).eq.0)write(*,*)' i=',izt,' z=',zzt,' ips=',ips,' nref=',nref
		!call pause()
		do i=1,nref
			write(11)dref(i),tref(i),alref(i),href(i)
			!write(*,*)dref(i),tref(i),alref(i),href(i)
		end do
		!call pause()
	end do
goto 34
35 close(11)
stop
end 
