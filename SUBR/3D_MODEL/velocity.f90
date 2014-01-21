function velocity(x,y,z,ips)

parameter (Rz=6371.)

dist=sqrt(x**2+y**2)
zsf=Rz - sqrt(dist**2+(Rz-z)**2)

v0=vrefmod(zsf,ips)

dv_apr= 0.01 * vert_anom(x,y,z,ips) * v0

dv = anom_3D_xyz_lin_v(x,y,z,ips)


velocity = v0 + dv + dv_apr


return
end