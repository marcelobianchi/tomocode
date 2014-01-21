function depth_moho(fi,tet)

real moho_depth

common/crust1/fa1,fb1,ta1,tb1,dfi1,dtet1,nfi1,ntet1,dcr_1(360,180)
common/crust_local/floc1,floc2,tloc1,tloc2,dfloc,dtloc,nfloc,ntloc, dcr_loc(360,180)
common/crust_par/avmoho,vcr1_p,vcr2_p,vcr1_s,vcr2_s,acrs,acrp
common/crust_model/nmod

!write(*,*) 'Moho for',fi,tet
fi1=fi

if(fi.lt.-180.) fi1=fi1+360
if(fi.gt. 180.) fi1=fi1-360

if(nfi1.eq.0) then
	depth_moho=avmoho
	return
end if

!write(*,*)'fi=',fi,' tet=',tet
if (nmod.eq.3) then
    !write (*,*) floc1,floc2,tloc1,tloc2
    !write (*,*) (fi1-floc1)*(fi1-floc2)
    !write (*,*) (tet-tloc1)*(tet-tloc2)

    if (((fi1-floc1)*(fi1-floc2)).gt.0) goto 100
    if (((tet-tloc1)*(tet-tloc2)).gt.0) goto 100
    do ifi=1,nfloc-1
        f1=floc1+(ifi-1)*dfloc
        f2=floc1+ifi*dfloc
        if ((fi1-f1)*(fi1-f2).le.0) exit
    end do
    
    do itet=1,ntloc-1
        t1=tloc1+(itet-1)*dtloc
        t2=tloc1+itet*dtloc
        if ((tet-t1)*(tet-t2).le.0) exit
    end do

    d11=dcr_loc(ifi,itet)
    d12=dcr_loc(ifi,itet+1)
    d21=dcr_loc(ifi+1,itet)
    d22=dcr_loc(ifi+1,itet+1)
    
    goto 200
  
end if

100 continue

if(fi1.lt.fa1) then
	if1=nfi1
	if2=1
	f1=fb1-360
	f2=fa1
else if(fi1.gt.fb1) then
	if1=nfi1
	if2=1
	f1=fb1
	f2=fa1+360
else
	do if1=1,nfi1
		f1=fa1+(if1-1)*dfi1
		f2=fa1+if1*dfi1
		if((fi1-f1)*(fi1-f2).le.0.) exit
	end do
	if2=if1+1
end if


if(tet.lt.ta1) then
	it1=1
	it2=2
	t1=ta1
	t2=ta1+dtet1
else if(tet.gt.tb1) then
	it1=ntet1-1
	it2=ntet1
	t1=tb1-dtet1
	t2=tb1
else
	do it1=1,ntet1
		t1=ta1+(it1-1)*dtet1
		t2=ta1+it1*dtet1
		if((tet-t1)*(tet-t2).le.0.) exit
	end do
	it2=it1+1
end if

d11=dcr_1(if1,it1)
d12=dcr_1(if1,it2)
d21=dcr_1(if2,it1)
d22=dcr_1(if2,it2)

!write(*,*)d11,d12,d21,d22
!d22=0
200 continue

d1=d11+((d12-d11)/(t2-t1))*(tet-t1)
d2=d21+((d22-d21)/(t2-t1))*(tet-t1)

!write(*,*)d1,d2
!write(*,*)' fi1=',fi1,' f1=',f1,' f2=',f2
depth_moho=d1+((d2-d1)/(f2-f1))*(fi1-f1)
!write(*,*)'depth_moho=',depth_moho,' model=',nmod
!pause
return
end
