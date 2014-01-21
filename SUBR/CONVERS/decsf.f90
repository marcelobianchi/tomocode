SUBROUTINE decsf(x,y,zz,fi0,tet0,FI,TET,h)
! this program calculates fi and tet of a point in global coordinates from
! given x, y, z coordinates relatively the point fi0, tet0

REAL PI/3.1415926/, rz/6371./
PER=PI/180.
z=rz-zz
X1=x
Y1=-Y*SIN(tet0*PER)+Z*COS(tet0*PER)
Z1=Y*COS(tet0*PER)+Z*SIN(tet0*PER)
if(x1.eq.0.) then
	fi=fi0
else
	IF(X1.GT.0..AND.Y1.GT.0.)PA=0.
	IF(X1.LT.0.)PA=-PI
	IF(X1.GT.0..AND.Y1.LT.0.)PA=2.*PI
	ff=atan(y1/x1)/per
	FI=fi0-(ff+PA/PER)+90.
end if
if(fi.lt.-180.)fi=fi+360.
if(fi.gt.180.)fi=fi-360.
if(abs(fi-fi0).gt.abs(fi-fi0-360)) fi=fi-360
if(abs(fi-fi0).gt.abs(fi-fi0+360)) fi=fi+360

r=sqrt(x1*x1+y1*y1+z1*z1)
TET=ASIN(Z1/r)/PER
h=rz-r
RETURN
END

