SUBROUTINE HIPC(FIR,TETR,FIS,TETS,fi0,tet0,HD,AZ)
REAL PI/3.1415926/
PER=PI/180.

hd=epic_dist(FIR,TETR,FIS,TETS)
CALL SFDEC(FIR,TETR,0.,X1,Y1,Z1,fi0,tet0)
CALL SFDEC(FIS,TETS,0.,X2,Y2,Z2,fi0,tet0)
X=X2-X1
Y=Y2-Y1
if(x.eq.0..and.y.ge.0.)then
	az=90.
	return
end if
if(x.eq.0..and.y.lt.0.)then
	az=270.
	return
end if
IF(X.GT.0..AND.Y.GT.0.)PA=0.
IF(X.LT.0.)PA=PI
IF(X.GT.0..AND.Y.LT.0.)PA=2.*PI
AZ=(ATAN(Y/X)+PA)/PER
RETURN
END
