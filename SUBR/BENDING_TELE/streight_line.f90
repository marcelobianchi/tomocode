subroutine steight_line(xzt,yzt,zzt,xst,yst,zst,ips, tout)

integer nodes1,nodes2
common/ray/ nodes,xray(10000),yray(10000),zray(10000)
common/ray_param/ds_ini,ds_ini1,ds_bend,ds_bend1,dist0,val_bend_min


dshor = sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
dsver = abs(zzt-zst)
dstot = sqrt(dshor*dshor+dsver*dsver)

ds_sm=dist0
ds_lar=dstot-ds_sm
if (dstot.lt.dist0) then
	ds_sm=dstot
	ds_lar=0.
end if
nodes1= ds_sm/ds_ini1 + 1
nodes2= ds_lar/ds_ini

nodes = nodes1+nodes2

ds1 = ds_sm/(nodes1-1)
ds2 = ds_lar/nodes2

if(nodes.eq.1) then
	nodes=2
	ds=dstot
end if

!write(*,*)' ztr:',xzt,yzt,zzt
do inode = 1,nodes1
	sss = ds1 * (inode-1)
	xray(inode) = xzt + sss * (xst-xzt)/dstot
	yray(inode) = yzt + sss * (yst-yzt)/dstot
	zray(inode) = zzt + sss * (zst-zzt)/dstot
	!write(*,*) sss,xray(inode),yray(inode),zray(inode)
end do
if (nodes2.gt.0) then
	do inode=1,nodes2
		sss = ds2 * inode
		xray(nodes1+inode) = xzt + (ds_sm+sss) * (xst-xzt)/dstot
		yray(nodes1+inode) = yzt + (ds_sm+sss) * (yst-yzt)/dstot
		zray(nodes1+inode) = zzt + (ds_sm+sss) * (zst-zzt)/dstot
	end do
end if


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
!write(*,*)' ttt=',ttt,' sss=',sss
!pause

tout=ttt



return
end