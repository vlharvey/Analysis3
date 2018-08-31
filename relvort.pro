   pro relvort,ul,vl,zeta,lon,lat,nc,nr
;
; compute relative vorticity on pressure surface
;
   pi2 = 6.2831853071796
   dtr=pi2/360.
   radea=6.37E6
   omega=7.292E-5
   result=size(ul)
   for k=0L,result(3)-1L do begin
   for i = 0, nc-1 do begin
       ip1=i+1
       im1=i-1
       ip2=i+2
       im2=i-2
       if i eq 0 then begin
         im1=nc-1
         im2=nc-2
       endif
       if i eq nc-1 then begin
         ip1=1
         ip2=2
       endif
       if i eq 1 then im2=nc-1
       if i eq nc-2 then ip2=0
       for j = 2, nr-3 do begin
           jp1=j+1
           jm1=j-1
           jp2=j+2
           jm2=j-2
           dy1=radea*(lat(jp1)-lat(jm1))*dtr
           dy2=radea*(lat(jp2)-lat(jm2))*dtr
           dx1=radea*cos(lat(j)*dtr)*pi2/(.5*nc)
           dx2=radea*cos(lat(j)*dtr)*pi2/(.25*nc)
           dvdx = (4./3.) * (vl(ip1,j,k) - vl(im1,j,k)) / dx1 $
                - (1./3.) * (vl(ip2,j,k) - vl(im2,j,k)) / dx2
           dudy = (4./3.)*(ul(i,jp1,k)*cos(lat(jp1)*dtr) $
                - ul(i,jm1,k)*cos(lat(jm1)*dtr)) / dy1 $
                - (1./3.)*(ul(i,jp2,k)*cos(lat(jp2)*dtr) $
                - ul(i,jm2,k)*cos(lat(jm2)*dtr)) / dy2
           dudy = dudy/cos(lat(j)*dtr)
           zeta(i,j,k) = dvdx-dudy
       endfor
;
; 4th order differentiation across the pole
;
; jm2 is across the South pole
;
       j=1
       jm1=j-1 & jm2=j
       jp1=j+1 & jp2=j+2
       if i lt nc/2 then i2=i+nc/2
       if i ge nc/2 then i2=i-nc/2
       i2p1=i2+1
       i2m1=i2-1
       if i2p1 gt nc-1 then i2p1=0
       if i2m1 lt 0  then i2m1=nc-1
       i2p2=i2p1+1
       i2m2=i2m1-1
       if i2p2 gt nc-1 then i2p2=0
       if i2m2 lt 0  then i2m2=nc-1
       dy1=radea*(lat(jp1)-lat(jm1))*dtr
       dy2=radea*(lat(jp2)-lat(jm2))*dtr
       dx1=radea*cos(lat(j)*dtr)*pi2/(.5*nc)
       dx2=radea*cos(lat(j)*dtr)*pi2/(.25*nc)
       dvdx = (4./3.) * (vl(ip1,j,k) - vl(im1,j,k)) / dx1 $
            - (1./3.) * (vl(ip2,j,k) - vl(im2,j,k)) / dx2
       dudy = (4./3.)*(ul(i,jp1,k)*cos(lat(jp1)*dtr) $
            - ul(i,jm1,k)*cos(lat(jm1)*dtr)) / dy1 $
            - (1./3.)*(ul(i,jp2,k)*cos(lat(jp2)*dtr) $
            - ul(i,jm2,k)*cos(lat(jm2)*dtr)) / dy2
       dudy = dudy/cos(lat(j)*dtr)
       zeta(i,j,k) = dvdx-dudy
;
; jm1 and jm2 across the South pole
;
;      j=0
;      jm1=j & jm2=j+1
;      jp1=j+1 & jp2=j+2
;      if i lt nc/2 then i2=i+nc/2
;      if i ge nc/2 then i2=i-nc/2
;      i2p1=i2+1
;      i2m1=i2-1
;      if i2p1 gt nc-1 then i2p1=0
;      if i2m1 lt 0  then i2m1=nc-1
;      i2p2=i2p1+1
;      i2m2=i2m1-1
;      if i2p2 gt nc-1 then i2p2=0
;      if i2m2 lt 0  then i2m2=nc-1
;      dy1=radea*(lat(jp1)-lat(jm1))*dtr
;      dy2=radea*(lat(jp2)-lat(jm2))*dtr
;      dx1=radea*cos(lat(j)*dtr)*pi2/(.5*nc)
;      dx2=radea*cos(lat(j)*dtr)*pi2/(.25*nc)
;      dvdx = (4./3.) * (vl(ip1,j,k) - vl(im1,j,k)) / dx1 $
;           - (1./3.) * (vl(ip2,j,k) - vl(im2,j,k)) / dx2
;      dudy = (4./3.)*(ul(i,jp1,k)*cos(lat(jp1)*dtr) $
;           - ul(i,jm1,k)*cos(lat(jm1)*dtr)) / dy1 $
;           - (1./3.)*(ul(i,jp2,k)*cos(lat(jp2)*dtr) $
;           - ul(i,jm2,k)*cos(lat(jm2)*dtr)) / dy2
;      dudy = dudy/cos(lat(j)*dtr)
;      zeta(i,j,k) = dvdx-dudy
;
; jp2 is across the North pole
;
       j=nr-2
       jm1=j-1  & jm2=j-2
       jp1=j+1 & jp2=j
       if i lt nc/2 then i2=i+nc/2
       if i ge nc/2 then i2=i-nc/2
       i2p1=i2+1
       i2m1=i2-1
       if i2p1 gt nc-1 then i2p1=0
       if i2m1 lt 0  then i2m1=nc-1
       i2p2=i2p1+1
       i2m2=i2m1-1
       if i2p2 gt nc-1 then i2p2=0
       if i2m2 lt 0  then i2m2=nc-1
       dy1=radea*(lat(jp1)-lat(jm1))*dtr
       dy2=radea*(lat(jp2)-lat(jm2))*dtr
       dx1=radea*cos(lat(j)*dtr)*pi2/(.5*nc)
       dx2=radea*cos(lat(j)*dtr)*pi2/(.25*nc)
       dvdx = (4./3.) * (vl(ip1,j,k) - vl(im1,j,k)) / dx1 $
            - (1./3.) * (vl(ip2,j,k) - vl(im2,j,k)) / dx2
       dudy = (4./3.)*(ul(i,jp1,k)*cos(lat(jp1)*dtr) $
            - ul(i,jm1,k)*cos(lat(jm1)*dtr)) / dy1 $
            - (1./3.)*(ul(i,jp2,k)*cos(lat(jp2)*dtr) $
            - ul(i,jm2,k)*cos(lat(jm2)*dtr)) / dy2
       dudy = dudy/cos(lat(j)*dtr)
       zeta(i,j,k) = dvdx-dudy
;
; jp1 and jp2 across the North pole
;
;      j=nr-1
;      jm1=j-1 & jm2=j-2
;      jp1=j   & jp2=j-1
;      if i lt nc/2 then i2=i+nc/2
;      if i ge nc/2 then i2=i-nc/2
;      i2p1=i2+1
;      i2m1=i2-1
;      if i2p1 gt nc-1 then i2p1=0
;      if i2m1 lt 0  then i2m1=nc-1
;      i2p2=i2p1+1
;      i2m2=i2m1-1
;      if i2p2 gt nc-1 then i2p2=0
;      if i2m2 lt 0  then i2m2=nc-1
;      dy1=radea*(lat(jp1)-lat(jm1))*dtr
;      dy2=radea*(lat(jp2)-lat(jm2))*dtr
;      dx1=radea*cos(lat(j)*dtr)*pi2/(.5*nc)
;      dx2=radea*cos(lat(j)*dtr)*pi2/(.25*nc)
;      dvdx = (4./3.) * (vl(ip1,j,k) - vl(im1,j,k)) / dx1 $
;           - (1./3.) * (vl(ip2,j,k) - vl(im2,j,k)) / dx2
;      dudy = (4./3.)*(ul(i,jp1,k)*cos(lat(jp1)*dtr) $
;           - ul(i,jm1,k)*cos(lat(jm1)*dtr)) / dy1 $
;           - (1./3.)*(ul(i,jp2,k)*cos(lat(jp2)*dtr) $
;           - ul(i,jm2,k)*cos(lat(jm2)*dtr)) / dy2
;      dudy = dudy/cos(lat(j)*dtr)
;      zeta(i,j,k) = dvdx-dudy
   endfor
   endfor
   return
   end
