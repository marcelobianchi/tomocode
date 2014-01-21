character*8 ar,gr,line
character*1 rm
real tref(100000),dref(100000),alref(100000),href(100000)
real hmod(600),vmodp(600),vmods(600) ! ref. models in different zones
real zst(20),dzst(20),dst(10),ddist(10)
integer maxnref


common/refmod/nrefmod,hmod,vmodp,vmods
common/pi/pi,per

maxnref=0
one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0
rz=6371.

open(1,file='../../model.dat')
read(1,'(a8)')gr		! code of the area
read(1,'(a8)')ar		! code of the area
close(1)

write(*,*)' ar=',ar		! code of the area



open(1,file='../../DATA/'//gr//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,*,end=553)line
	if(line.eq.'REF_PARA') goto 554
end do
553 continue
write(*,*)' cannot find REF_PARAM in major_param.dat!!!'
pause


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


open(1,file='../../DATA/'//gr//'/'//ar//'/INI_PARAM/refmod.dat')
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

open(11,file='../../DATA/'//gr//'/'//ar//'/TIMES/table.dat',form='binary')
!	do zzt=zztmin,zztmax,dzzt
34	zzt=zzt+dzzt
	!write(*,*) 'zzt=',zzt
	!if (zzt.lt.1) goto 34
	if(zzt.gt.zztmax) goto 35
	izt=izt+1
	if(nstep.gt.1) then
		do istep=2,nstep
			if(zzt.gt.zst(istep)) dzzt=dzst(istep)
		end do
	end if

!if(izt.ne.1) goto 34	
	
	do ips=1,2
		!write(*,*)' zzt=',zzt
		dlast=999
		nref=0
		dmin=ddist(1)
		!open(31,file='tmp.dat')
		do alfa0=180.0d0, 0.01d0, -0.005d0
		!do alfa0=1.00d0, 0.01d0, -0.02d0
			!write(*,*)alfa0
			call reftrace(alfa0,zzt,zout,ips,1,  time,dist,hmax)
			!write(*,*)' alfa0=',alfa0,' dist=',dist,' hmax=',hmax
			dgrad=(dist/rz)/per
		!	if (hmax.ge.hmod(nrefmod-1)) exit
			if(dist.lt.0.) cycle
			dkm=dist
			if (nstd.gt.1) then
				do istep=2,nstd
					if (dkm.gt.dst(istep)) dmin=ddist(istep)
				end do
			end if
			if(abs(dkm-dlast).gt.dmin) then
				nref=nref+1
				tref(nref)=time
				dref(nref)=dkm
				if(nref.eq.1)dref(nref)=0
				alref(nref)=alfa0
				href(nref)=hmax
				dlast=dkm
				!write(*,*)nref,alfa0,dist,time,hmax
				!pause
				!write(31,*)nref,alfa0,dkm,time,hmax
			end if
			if (hmax.gt.depmax)exit 
			if (dist.gt.distmax)exit 
				
		end do
		!close(31)
		if (nref.GE.maxnref) maxnref = nref
		write(11)zzt,nref
		!write(*,*)zzt,nref		
		if(mod(izt,10).eq.0) write(*,*)' i=',izt,' z=',zzt,' ips=',ips,' nref=',nref
		do i=1,nref
			write(11)dref(i),tref(i),alref(i),href(i)
			!write(*,*)dref(i),tref(i),alref(i),href(i)
		end do
		!pause
	end do
goto 34
35 close(11)
write (*,*) 'Max Nref= ',maxnref
stop
end 
