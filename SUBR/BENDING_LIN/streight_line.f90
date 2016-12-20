subroutine steight_line(xzt,yzt,zzt,xst,yst,zst,ips, tout)

common/ray/ nodes,xray(1000),yray(1000),zray(1000)
common/ray_param/ds_ini,ds_bend,val_bend_min


dshor = sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
dsver = abs(zzt-zst)
dstot = sqrt(dshor*dshor+dsver*dsver)

nodes = dstot/ds_ini+1


ds = dstot/(nodes-1)
if(nodes.eq.1) then
	nodes=2
	ds=dstot
end if

!write(*,*)' ztr:',xzt,yzt,zzt
do inode = 1,nodes
	sss = ds * (inode-1)
	xray(inode) = xzt + sss * (xst-xzt)/dstot
	yray(inode) = yzt + sss * (yst-yzt)/dstot
	zray(inode) = zzt + sss * (zst-zzt)/dstot
	!write(*,*) sss,xray(inode),yray(inode),zray(inode)
end do
!write(*,*)sss,' sta:',xst,yst,zst

! Time along the streight line:

ddd=0
ttt=0
sss=0
do inode=1,nodes-1
	x1=xray(inode)
	x2=xray(inode+1)
	xm=(x1+x2)/2.

	y1=yray(inode)
	y2=yray(inode+1)
	ym=(y1+y2)/2.

	z1=zray(inode)
	z2=zray(inode+1)
	zm=(z1+z2)/2.

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
	!write(*,*)xm,ym,zm,ips
	vvv=velocity (xm,ym,zm,ips)
	!write(*,*)' vvv=',vvv
	ttt=ttt+ds/vvv
	sss=sss+ds
	!write(*,*)' ds=',ds,' sss=',sss
	!write(*,*)' t=',ttt,' s=',sss,' vvv=',vvv
end do
write(*,*)' ttt=',ttt,' sss=',sss
call pause()

tout=ttt



return
end