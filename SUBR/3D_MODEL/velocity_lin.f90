function velocity_lin(x,y,z,ips)

v0=vrefmod(z,ips)
dv = anom_3D_xyz_lin(x,y,z,ips)

!write(*,*)' v0=',v0,' dv=',dv

velocity_lin = v0 + dv

return
end