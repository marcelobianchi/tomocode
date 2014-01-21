function vrefmod(z,ips)

common/refmod/nrefmod,hhmod(600),vmodp(600),vmods(600)

vrefmod=-900.
nref=nrefmod

if(nref.eq.0) then
	write(*,*)' the reference model is not defined!'
	pause
end if
!write(*,*)' z=',z,hhmod(1),hhmod(nref)
if(z.le.hhmod(1))then
	z1=hhmod(1)
	z2=hhmod(2)
	if(ips.eq.1) then
		v1=vmodp(1)
		v2=vmodp(2)
	else
		v1=vmods(1)
		v2=vmods(2)
	end if
	vrefmod=v1+((v2-v1)/(z2-z1))*(z-z1)
else if(z.ge.hhmod(nref)) then
	z1=hhmod(nref-1)
	z2=hhmod(nref)
	if(ips.eq.1) then
		v1=vmodp(nref-1)
		v2=vmodp(nref)
	else
		v1=vmods(nref-1)
		v2=vmods(nref)
	end if
	vrefmod=v1+((v2-v1)/(z2-z1))*(z-z1)
end if

do i=1,nref-1
	z1=hhmod(i)
	z2=hhmod(i+1)
	if((z-z1)*(z-z2).gt.0.)cycle
	if(ips.eq.1) then
		v1=vmodp(i)
		v2=vmodp(i+1)
	else
		v1=vmods(i)
		v2=vmods(i+1)
	end if
	!write(*,*)' z1=',z1,' z2=',z2
	!write(*,*)' v1=',v1,' v2=',v2
	vrefmod=v1+((v2-v1)/(z2-z1))*(z-z1)
end do

return
end 