subroutine read_ini_model_3D(re,ar)
character*8 ar,re
character*1 ps
common/center/fi0,tet0
common/ini3d/fmin3D,anom3d(2,500,500,100),df3D,nnetf,tmin3D,dt3D,nnett,zmin3D,dz3D,nnetz

nnetf=0

do ips=1,2
    write(ps,'(i1)')ips 
    open(1,file='../../DATA/'//re//'/INIDATA/dv3d_'//ps//'.dat',form='binary')
    read(1,err=123)fmin3D,df3D,nnetf
    read(1)tmin3D,dt3D,nnett
    read(1)zmin3D,dz3D,nnetz
    write(*,*) '3D Model f:', fmin3D,df3D,nnetf
    write(*,*) '3D Model t:', tmin3D,dt3D,nnett
    write(*,*) '3D Model z:', zmin3D,dz3D,nnetz

    do izz=1,nnetz
        read(1)((anom3d(ips,ixx,iyy,izz),iyy=1,nnett),ixx=1,nnetf)
        !write(*,*)((anom3d(ips,ixx,iyy,izz),iyy=1,nnett),ixx=1,nnetf)
    end do
    close(1)
end do

123 continue
return
end
