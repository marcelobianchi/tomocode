character*4 dsaa/'DSAA'/
character*8 ar,re,line
character*1 ps
character*2 lv
!real hlev(100)

! For reading the kod_syn_all.dat
integer whichmodel, num_models, imodel, narg, iargc
character parinput

allocatable v_ini(:,:)
common/center/fi0,tet0
common/pi/pi,per
common/levels/nlev,hlev(100)


common/brd_dv/xbr1(2),xbr2(2),dxbr1(2),dxbr2(2),&
			  ybr1(2),ybr2(2),dybr1(2),dybr2(2),&
			  zbr1(2),zbr2(2),dzbr1(2),dzbr2(2),amp(2)

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0

zzz=2
ips=1

!Reading the parameters from the kod_syn_all.dat
open(1, file='../../kod_syn_all.dat')
read(1,*) num_models
read(1,*)
read(1,*)
read(1,*)

!Get the number of parameters
narg=iargc()

if (num_models.GT.1) then
 if(narg.EQ.0) then
  do imodel=1,num_models
   read(1,'(a8)') re
   read(1,'(a8)') ar
   read(1,*)
   write (*,'(i3,3a,a17)') imodel,') ',re//' '//ar
  enddo
  write(*,'(a,$)') 'Which model do you want to compute for? '
  read (*,*) whichmodel

  rewind(1)
  read(1,*) num_models
  read(1,*)
  read(1,*)
  read(1,*)
 else
  call getarg (1,parinput)
  read(parinput,*) whichmodel
  write(*,*) 'Using model:',whichmodel
 endif
else
 whichmodel=1
endif

if (whichmodel.GT.num_models.OR.whichmodel.LE.0) then
 WRITE(*,*) 'Selected model ',whichmodel,' larger than possible ',num_models,'.'
 STOP
endif

do imodel=1,whichmodel
 read(1,'(a8)') re
 read(1,'(a8)') ar
 read(1,*)
enddo

write(*,*) 'Region=',re,' Area=',ar
close(1)

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
pause
554 read(1,*)fi0,tet0
close(1)

!******************************************************************

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/anomaly.dat')
read(1,*) n_anomaly
close(1)
write(*,*)' ar=',ar,'   kod of anom.=',n_anomaly


open(2,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/sethor.dat')
read(2,*) nlev  
read(2,*) (hlev(i),i=1,nlev)  
read(2,*) fmap1,fmap2,dfmap,tmap1,tmap2,dtmap  
close(2)

!BIANCHI Why divide by two ? Best set sethor !
!dfmap=dfmap/2.
!dtmap=dtmap/2.

nfmap=int_best((fmap2-fmap1)/dfmap+1.)
ntmap=int_best((tmap2-tmap1)/dtmap+1.)
write(*,*)' nfmap=',nfmap,' ntmap=',ntmap


allocate(v_ini(nfmap,ntmap))

if(n_anomaly.eq.1)then
	call prep_board_dv(re,ar)
else if(n_anomaly.eq.2)then
	call read_hor_an (re,ar)
else if(n_anomaly.eq.3)then
	call read_vert_an(re,ar)
else if(n_anomaly.eq.4)then
	call read_vert_brd(re,ar)
end if

open(66,file='../../FIG_FILES/SYN_INI/refvelocities.dat')

do ips=1,2
	write(ps,'(i1)')ips
	DO ilev=1,nlev
		dvan=0
		vvv=0
		zzz=hlev(ilev)
		if (ips.EQ.1) write(66,*) ilev,zzz,0.0
		!zzz=5
		write(lv,'(i2)')ilev
		write(*,*)' ilev=',ilev,' zzz=',zzz


		v_ini=0

		do itet=1,ntmap
			ttt=(itet-1)*dtmap+tmap1+tet0
			!write(*,*)' itet=',itet,' ttt=',ttt
			!ttt=-7.35
			do ifi=1,nfmap
				fff=(ifi-1)*dfmap+fmap1+fi0
				!fff=fi0+1
				!fff=-18.8
				!fff=110.84
				call SFDEC(fff,ttt,0.,xxx,yyy,Z,fi0,tet0)
				if(n_anomaly.eq.1)then
					dv = dv_board(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.2)then
					dv = hor_anom(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.3)then
					dv=vert_anom(xxx,yyy,zzz,ips)
				else if(n_anomaly.eq.4)then
					dv=vert_brd(xxx,yyy,zzz,ips)
				end if
				v_ini(ifi,itet)=dv
				write(*,*)' fi=',fff,' tet=',ttt,' dv=',dv
				write(*,*)' x=',xxx,' y=',yyy,' z=',zzz,' dv=',dv
				!pause
			end do
		end do

		!open(14,file='../../FIG_files/hor/dv'//ps//'_'//lv//'.grd')
		open(14,file='../../FIG_FILES/SYN_INI/hor'//ps//'_'//lv//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nfmap,ntmap
		write(14,*)fmap1+fi0,fmap2+fi0
		write(14,*)tmap1+tet0,tmap2+tet0
		write(14,*)-999,999
		do itet=1,ntmap
			write(14,*)(v_ini(ifi,itet),ifi=1,nfmap)
		end do
		close(14)
	end do
end do
close(66)
stop
end
