function cut_angle(x1,x2,x3,zz1,zz2,zz3, x)
real aaa(3,3),bbb(3),xxx(3)

y1=x1-x2
y2=(x2+x1)/2-x2
y3=0
y4=(x2+x3)/2-x2
y5=x3-x2

y0=x-x2

zcen=zz2

z1=zz1 - zz2
z3=0
z5=zz3 - zz2

dzdy13=(z3-z1)/(y3-y1)
dzdy35=(z5-z3)/(y5-y3)

z2=z1+dzdy13*(y2-y1)
z4=z3+dzdy35*(y4-y3)

!write(*,*)y1,y2,y3,y4,y5
!write(*,*)z1,z2,z3,z4,z5


aaa(1,1)=(y2*y2*y2-y4*y4*y4)
aaa(1,2)=(y2*y2-y4*y4)
aaa(1,3)=(y2-y4)
bbb(1)=z2-z4
aaa(2,1)=3*y2*y2
aaa(2,2)=2*y2
aaa(2,3)=1
bbb(2)=dzdy13
aaa(3,1)=3*y4*y4
aaa(3,2)=2*y4
aaa(3,3)=1
bbb(3)=dzdy35

call kram3(aaa,bbb,xxx)

d=z2 - (xxx(1)*y2*y2*y2 + xxx(2)*y2*y2 + xxx(3)*y2)

cut_angle = zz2 + xxx(1)*y0*y0*y0 + xxx(2)*y0*y0 + xxx(3)*y0 + d

return
end