subroutine ini_3D_model(fff,ttt,zzz,ips, dv)
!real dv !,ini_3D_model

common/ini3d/fmin3D,anom3d(2,500,500,100),df3D,nnetf,tmin3D,dt3D,nnett,zmin3D,dz3D,nnetz

dv=0.0
if (nnetf.eq.0) goto 102
!write(*,*) 'oi'

fmax3D=fmin3D+nnetf*df3D
!write(*,*)fmin3D,fmax3D
if ((fff-fmin3D)*(fff-fmax3D).gt.0) goto 102

tmax3D=tmin3D+nnett*dt3D
!write(*,*)tmin3D,tmax3D
if ((ttt-tmin3D)*(ttt-tmax3D).gt.0) goto 102

zmax3D=zmin3D+nnetz*dz3D
!write(*,*)zmin3D,zmax3D
if ((zzz-zmin3D)*(zzz-zmax3D).gt.0) goto 102





do iff=1,nnetf-1
    f1=fmin3D+(iff-1)*df3D
    f2=fmin3D+iff*df3D
    if ((fff-f1)*(fff-f2).gt.0) cycle
    exit
end do

do itt=1,nnett-1
    t1=tmin3D+(itt-1)*dt3D
    t2=tmin3D+itt*dt3D
    if ((ttt-t1)*(ttt-t2).gt.0) cycle
    exit
end do

do izz=1,nnetz-1
    z1=zmin3D+(izz-1)*dz3D
    z2=zmin3D+izz*dz3D
    if ((zzz-z1)*(zzz-z2).gt.0) cycle
    exit
end do

!write(*,*)iff,itt,izz

dv111=anom3d(ips,iff,itt,izz)
dv211=anom3d(ips,iff+1,itt,izz)
dv121=anom3d(ips,iff,itt+1,izz)
dv221=anom3d(ips,iff+1,itt+1,izz)

dv112=anom3d(ips,iff,itt,izz+1)
dv212=anom3d(ips,iff+1,itt,izz+1)
dv122=anom3d(ips,iff,itt+1,izz+1)
dv222=anom3d(ips,iff+1,itt+1,izz+1)

dv11=dv111+((dv111-dv211)/(f1-f2))*(fff-f1)
dv21=dv121+((dv121-dv221)/(f1-f2))*(fff-f1)

dv12=dv112+((dv112-dv212)/(f1-f2))*(fff-f1)
dv22=dv122+((dv122-dv222)/(f1-f2))*(fff-f1)

dv1=dv11+((dv11-dv12)/(z1-z2))*(zzz-z1)

dv2=dv21+((dv21-dv22)/(z1-z2))*(zzz-z1)

dv=dv1+((dv1-dv2)/(t1-t2))*(ttt-t1)

102 continue
!ini_3D_model=dv
!write(*,*)'dv=',dv !,ini_3D_model

return 
end
