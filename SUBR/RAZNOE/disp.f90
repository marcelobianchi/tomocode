subroutine disp(dt,nkrat, aver,nkr,dsp)
real*8 dt(500)

aver=0
nkr=0
do i=1,nkrat
	if(abs(dt(i)).gt.10) cycle
	nkr=nkr+1
	aver=aver+dt(i)
end do
aver=aver/nkr

dsp=0.
do i=1,nkrat
	if(abs(dt(i)).gt.10) cycle
	dsp=dsp+abs(dt(i)-aver)
end do
if(nkr.eq.0) then
	dsp=0
else
	dsp=dsp/nkr
end if

return
end
