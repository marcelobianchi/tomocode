subroutine remeshing()

common/ray/ nodes,xray(10000),yray(10000),zray(10000)
real xtmp(10000),ytmp(10000),ztmp(10000)
common/ray_param/ds_ini,ds_ini1,ds_bend,ds_bend1,dist0,val_bend_min
common/ray_parameterezation/length,nod1,nod2


ntmp=nodes
xtmp=xray
ytmp=yray
ztmp=zray

nodes=0

len=0
do inode=1,ntmp-1
	x1=xtmp(inode)
	x2=xtmp(inode+1)

	y1=ytmp(inode)
	y2=ytmp(inode+1)
	
	z1=ztmp(inode) 
	z2=ztmp(inode+1) 

	dz1=ztmp(inode) 
	dz2=ztmp(inode+1) 

	nodes=nodes+1
	xray(nodes)=x1
	yray(nodes)=y1
	zray(nodes)=z1

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
	len = len + ds
	!write(*,*)' normal: ',ds,xray(nodes),yray(nodes),zray(nodes)

	ds_ini_cur=ds_ini1
	if (len.gt.dist0) ds_ini_cur=ds_ini

	if(ds.gt.ds_ini_cur*1.5) then
		nmid = ds/ds_ini_cur + 1
		dsmid = ds/nmid
		do imid=1,nmid-1

			xmid=x1+((x2-x1)/ds)*(dsmid*imid)
			ymid=y1+((y2-y1)/ds)*(dsmid*imid)
			zmid=z1+((z2-z1)/ds)*(dsmid*imid)

			nodes=nodes+1
			xray(nodes)=xmid
			yray(nodes)=ymid
			zray(nodes)=zmid
		!write(*,*)'  mid: ',dsmid,xray(nodes),yray(nodes),zray(nodes)
		end do
	end if
end do

nodes=nodes+1
xray(nodes)=xtmp(ntmp)
yray(nodes)=ytmp(ntmp)
zray(nodes)=ztmp(ntmp)

length=len

return
end