function dh_anom(fi,tet)

common/dh_an/nan,anom(40,4000,2),val_dh(40),nnod(40)


!write(*,*)' nan=',nan


xxx=fi
yyy=tet


dh_anom = 0

!write(*,*)' nan=',nan
if(nan.eq.0) return



do ian=1,nan
	icrup=0

	do inod=2,nnod(ian)
!write(*,*)' fi=',anom(ian,inod,1),' tet=',anom(ian,inod,2)
		x1=anom(ian,inod-1,1)
		x2=anom(ian,inod,1)
		y1=anom(ian,inod-1,2)
		y2=anom(ian,inod,2)
!write(*,*)' x1=',x1,' x2=',x2
!write(*,*)' z1=',z1,' z2=',z2
		if(x1.eq.x2) cycle
		if ((xxx-x1)*(xxx-x2).gt.0.) cycle
		if ((xxx-x1).eq.0.) then
			in=inod
667			continue
			if(in.gt.2) then
				x0=anom(ian,in-2,1)
				if(x0.eq.x1) then
					in=in-1
					goto 667
				end if
			else
				x0=anom(ian,nnod(ian)-1,1)
			end if
			if ((x0-x1)*(x2-x1).gt.0.) cycle
		end if
		if ((xxx-x2).eq.0.) cycle
		yy=y1+((y2-y1)/(x2-x1))*(xxx-x1)
		!write(*,*)' x1=',x1,' x2=',x2,' yy=',yy
		if (yy.lt.yyy) cycle
		icrup=icrup+1
	end do
	icrup2=int(icrup/2)*2
!write(*,*)' ian=',ian,' icrup=',icrup,' icrup2=',icrup2
	if(icrup.eq.icrup2) cycle
	dh_anom=val_dh(ian)
end do

return
end


