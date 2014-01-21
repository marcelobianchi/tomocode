subroutine apriori_anom(xx,yy,zz,ips, dv)
common/center/fi0,tet0
common/inimodel/model_type

if (model_type.eq.5) then
    call decsf(xx,yy,zz,fi0,tet0,fff,ttt,h)
    call ini_3D_model(fff,ttt,zz,ips, dv)
else 
    dv=vert_anom(xx,yy,zz,ips)
end if

return
end


