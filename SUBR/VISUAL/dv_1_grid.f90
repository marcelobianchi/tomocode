subroutine dv_1_grid(fff,ttt,zzz,smaxx,   dv,umn)
real wgt(2),dvv(2),xt(3),yt(3),dvt(3)
real amatr(3,3),bmatr(3),sol(3)
integer nonr(540),popr(540,560)


common/pi/pi,per
common/center/fi0,tet0
common/grid/nornt,ornt(4),sinal,cosal,&
		nlev,hlev(30),ntop(30),nobr,&
		xtop(10000,30), ytop(10000,30),n_pop(10000,30),&
		dv_mod(10000)


dv=0
umn=0

if(zzz.lt.hlev(1)) return
if(zzz.gt.hlev(nlev)) return

!do ilev=2,nlev
	!write(*,*)ilev,' ntop(ile)=',ntop(ilev)
!end do

do ilev=2,nlev
	z1=hlev(ilev-1)
	z2=hlev(ilev)
	if((zzz-z1)*(zzz-z2).le.0.) exit
end do

!write(*,*)' ilev=',ilev

!write(*,*)' fff=',fff,' ttt=',ttt
!write(*,*)' fi0=',fi0,' tet0=',tet0

call SFDEC(fff,ttt,0.,x11,y11,Z,fi0,tet0)

!write(*,*)' sinal=',sinal,' cosal=',cosal

xcur=x11*cosal+y11*sinal
ycur=-x11*sinal+y11*cosal
!write(*,*)' xcur=',xcur,' ycur=',ycur

xl=xtop(1,ilev)
xr=xtop(ntop(ilev),ilev)
yb=ytop(1,ilev)
yu=ytop(ntop(ilev),ilev)
!write(*,*)xl,xr
!write(*,*)yb,yu
if((xcur-xr)*(xcur-xl).gt.0) return
if((ycur-yb)*(ycur-yu).gt.0) return

dvv=0
wgt=0
do il=1,2
	ile=ilev
	if(il.eq.1) ile=ilev-1

	!write(*,*)' ile=',ile,' ntop(ile)=',ntop(ile)

	wgt(il)=0

	smin=9999999.
	do i=1,ntop(ile)
		if(n_pop(i,ile).eq.0) cycle
		x=xtop(i,ile)
		y=ytop(i,ile)
		z=hlev(ile)
		!write(*,*)' z=',z,' zzz=',zzz
		ss=sqrt((x-xcur)*(x-xcur)+(y-ycur)*(y-ycur)+(z-zzz)*(z-zzz))
		if(ss.lt.smin) smin=ss
		if(ss.lt.smaxx) then
			umnn=1
			goto 71
		end if
	end do
!write(*,*)' smin=',smin,' smaxx=',smaxx


	if(smin.gt.smaxx*2.) then
		umnn=0.
		cycle
	else 
		umnn=(smaxx*2.-smin)/smaxx
	end if
71	continue
!write(*,*)' smin=',smin,' smaxx=',smaxx
!write(*,*)' umn=',umn
	wgt(il)=umnn


	nrow=0
	xold=-100000.
	do itop=1,ntop(ile)
		if(abs(xtop(itop,ile)-xold).gt.0.00001)then
			nrow=nrow+1
			nonr(nrow)=0
			xold=xtop(itop,ile)
		end if
		nonr(nrow)=nonr(nrow)+1
		popr(nrow,nonr(nrow))=itop
	end do
!write(*,*)' nrow=',nrow,' ntop(ile)=',ntop(ile)

	do irow=1,nrow-1
		x1=xtop(popr(irow,1),ile)
		x2=xtop(popr(irow+1,1),ile)
		if((xcur-x1)*(xcur-x2).le.0) exit
	end do
	do iy1=1,nonr(irow)-1
		ya1=ytop(popr(irow,iy1),ile)
		yb1=ytop(popr(irow,iy1+1),ile)
		if((ycur-ya1)*(ycur-yb1).le.0) goto 897
	end do
	umnn=0.
	cycle

897	continue

	ipop1=n_pop(popr(irow,iy1),ile)
	ipop2=n_pop(popr(irow,iy1+1),ile)
	va1=0
	vb1=0
	if(ipop1.ne.0.and.irow.ne.1)va1=dv_mod(ipop1)
	if(ipop2.ne.0.and.irow.ne.1)vb1=dv_mod(ipop2)

!write(*,*)' xcur=',xcur,' x1=',x1,' x2=',x2
!write(*,*)' ycur=',ycur,' ya1=',ya1,' yb1=',yb1
!write(*,*)' va1=',va1,' vb1=',vb1

	do iy2=1,nonr(irow+1)-1
		ya2=ytop(popr(irow+1,iy2),ile)
		yb2=ytop(popr(irow+1,iy2+1),ile)
		if((ycur-ya2)*(ycur-yb2).le.0) goto 898
	end do
	umnn=0.
	cycle

898	continue
	ipop1=n_pop(popr(irow+1,iy2),ile)
	ipop2=n_pop(popr(irow+1,iy2+1),ile)
	va2=0
	vb2=0
	if(ipop1.ne.0.and.irow.ne.nrow-1)va2=dv_mod(ipop1)
	if(ipop2.ne.0.and.irow.ne.nrow-1)vb2=dv_mod(ipop2)

!write(*,*)' ycur=',ycur,' ya2=',ya2,' yb2=',yb2
!write(*,*)' va2=',va2,' vb2=',vb2


	sa2b1=sqrt((ya2-yb1)*(ya2-yb1)+(x2-x1)*(x2-x1))
	sa1b2=sqrt((ya1-yb2)*(ya1-yb2)+(x2-x1)*(x2-x1))
	yt(1)=ya1
	xt(1)=x1
	dvt(1)=va1
	yt(2)=yb2
	xt(2)=x2
	dvt(2)=vb2
	if(sa1b2.gt.sa2b1)then
		yt(1)=ya2
		xt(1)=x2
		dvt(1)=va2
		yt(2)=yb1
		xt(2)=x1
		dvt(2)=vb1
	end if
	yper=yt(1)+(yt(1)-yt(2))/(xt(1)-xt(2))*(xcur-xt(1))
	if(ycur.ge.yper.and.sa1b2.le.sa2b1)then
		yt(3)=yb1
		xt(3)=x1
		dvt(3)=vb1
	else if(ycur.lt.yper.and.sa1b2.le.sa2b1)then
		yt(3)=ya2
		xt(3)=x2
		dvt(3)=va2
	else if(ycur.ge.yper.and.sa1b2.gt.sa2b1)then
		yt(3)=yb2
		xt(3)=x2
		dvt(3)=vb2
	else if(ycur.lt.yper.and.sa1b2.gt.sa2b1)then
		yt(3)=ya1
		xt(3)=x1
		dvt(3)=va1
	end if
	do i=1,3
		amatr(i,1)=xt(i)-xcur
		amatr(i,2)=yt(i)-ycur
		amatr(i,3)=1.
		bmatr(i)=dvt(i)
!write(*,*)(amatr(i,j),j=1,3),bmatr(i)
	end do
	call kram3(amatr,bmatr,sol)
	dvv(il)=sol(3)
!write(*,*)' dvv=',dvv(il)




end do

umn=wgt(1)+((wgt(2)-wgt(1))/(z2-z1))*(zzz-z1)
dv =dvv(1)+((dvv(2)-dvv(1))/(z2-z1))*(zzz-z1)

!write(*,*)' umn=',umn,' dv=',dv

return
end