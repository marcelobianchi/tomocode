function vref_smooth(z,ips)
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
real vmod(600)
real fz3(3),zabc(3)

if(nrefmod.eq.0) then
	write(*,*)' the reference model is not defined!'
	pause
end if

do i=1,nrefmod
	vmod(i)=vmodp(i)
	if(ips.eq.2) vmod(i)=vmods(i)
end do

if(z.le.hmod(1))then
	z1=hmod(1)
	z2=hmod(2)
	v1=vmod(1)
	v2=vmod(2)
	vref_smooth=v1+((v2-v1)/(z2-z1))*(z-z1)
	return
else if(z.ge.hmod(nrefmod)) then
	z1=hmod(nrefmod-1)
	z2=hmod(nrefmod)
	v1=vmod(nrefmod-1)
	v2=vmod(nrefmod)
	vref_smooth=v1+((v2-v1)/(z2-z1))*(z-z1)
	return
end if


do iz=1,nrefmod-1
	z1=hmod(iz)
	z2=hmod(iz+1)
	if((z-z1)*(z-z2).gt.0.)cycle
	v1=vmod(iz)
	v2=vmod(iz+1)
	exit
end do


if(abs(z-z1).lt.abs(z-z2)) then
	iadd=0
	if(iz.eq.1) iadd=1
	do i=1,3
		zabc(i)=hmod(i+iz-2+iadd)
		fz3(i)=vmod(i+iz-2+iadd)
	end do
else
	iadd=0
	if(iz.eq.nrefmod-1) iadd=-1
	do i=1,3
		zabc(i)=hmod(i+iz-1+iadd)
		fz3(i)=vmod(i+iz-1+iadd)
	end do
end if

za=zabc(1)
zb=zabc(2)
zc=zabc(3)
fa=fz3(1)
fb=fz3(2)
fc=fz3(3)
!write(*,*)' za=',za,' zb=',zb,' zc=',zc
!write(*,*)' fa=',fa,' fb=',fb,' fc=',fc
vref_smooth = cut_angle(za,zb,zc,fa,fb,fc, z)




return
end