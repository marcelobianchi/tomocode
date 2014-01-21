subroutine dv_1_gr_block_ftz(fff,ttt,zzz, dv1,dv2,dv3,alpha,www)

common/pi/pi,per
common/center/fi0,tet0
common/grid/nornt,ornt(4),sinal,cosal,&
			nbl,block(100000,6),dv1_mod(100000),dv2_mod(100000),dv3_mod(100000),alpha_mod(100000)

dv1=0.
dv2=0.
dv3=0.
alpha=0.
www=0.

call sfdec(fff,ttt,zzz,xxx,yyy,h,fi0,tet0)
!write(*,*)xxx,yyy
x=xxx*cosal+yyy*sinal
y=yyy*cosal-xxx*sinal
z=zzz
do ibl=1,nbl
	x1=block(ibl,1)
	x2=block(ibl,2)
	if ((x-x1)*(x-x2).gt.0) cycle
	y1=block(ibl,3)
	y2=block(ibl,4)
	if ((y-y1)*(y-y2).gt.0) cycle
	z1=block(ibl,5)
	z2=block(ibl,6)
	if ((z-z1)*(z-z2).gt.0) cycle
	dv1=dv1_mod(ibl)
	dv2=dv2_mod(ibl)
	dv3=dv3_mod(ibl)
	!write(*,*)ibl,' dv1=',dv1,' dv2=',dv2,' dv3=',dv3
	!pause
	alpha=alpha_mod(ibl)
	www=1.
	exit
end do

return
end