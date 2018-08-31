pro marker_circhighs_v7,sf,mark,qdf,zeta,u,v,x,y,theta
;
; developed from marker_lows_v7
;
nbins=20
nr=n_elements(y)
nc=n_elements(x)
dx=x(1)-x(0)
dy=y(1)-y(0)
lon=0.*sf
lat=0.*sf
for i=0,n_elements(x)-1 do lat(i,*)=y
for j=0,n_elements(y)-1 do lon(*,j)=x
;
; QDF, zeta at poles bad
;
index=where(abs(lat) eq max(y))
qdf(index)=0.
zeta(index)=0.
speed=sqrt(u^2+v^2)
;
; anticyclone modification, will need easterly requirement?
;
index=where(u lt 0.)
if index(0) ne -1L then speed(index)=-1.0*speed(index)
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
avgz_vs_sf=-999.+0.*fltarr(nbins)		; average relative vorticity
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
avgy_vs_sf=-999.+0.*fltarr(nbins)               ; average latitude per bin
sfbin=0.0*fltarr(nbins)

; set NH latmin, latmax
latmin=25.
latmax=90.
kk=where(lat gt latmin)
sfmin=min(sf(kk))
sfmax=max(sf(kk))
sfint=(sfmax-sfmin)/(nbins)
sfbin=sfmin+sfint*findgen(nbins)

; loop over SF bins in NH
for n=0,nbins-2 do begin
    t=where(lat ge latmin and sf gt sfbin(n) and sf le sfbin(n+1),it)
; check latmin.  make sure bins are resolved (do not intersect latmin)
    if (it gt 2) then begin
        if min(lat(t))-latmin le dy then begin
           avgq_vs_sf(n)=-999.
           goto,jumpnhbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        avgz_vs_sf(n)=total(zeta(t))/float(it)
        avgs_vs_sf(n)=total(speed(t))/float(it)
        avgy_vs_sf(n)=total(lat(t))/float(it)
    endif
    jumpnhbin:
endfor	; loop over bins
s=where(lat ge latmin and sf gt sfbin(nbins-1),is)
if is gt 2 then begin
if min(lat(s))-latmin gt dy then begin
   avgq_vs_sf(nbins-1)=total(qdf(s))/float(is)
   avgz_vs_sf(nbins-1)=total(zeta(s))/float(is)
   avgs_vs_sf(nbins-1)=total(speed(s))/float(is)
   avgy_vs_sf(nbins-1)=total(lat(s))/float(is)
endif
endif
index=where(avgq_vs_sf ne -999.,nbins)
if index(0) ne -1 then begin
   avgq_vs_sf=avgq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   avgy_vs_sf=avgy_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then goto,dosh
;
; adjust avgq in case of no nodes to find minimum in second derivative
;
index=where(avgq_vs_sf ne -999.0)
qmin=min(avgq_vs_sf(index))
qmax=max(avgq_vs_sf(index))
if qmax lt 0. then avgq_vs_sf(index)=avgq_vs_sf(index)-qmax+1.0

; interpolate SF values to where integrated QDF=0.0 y=mx+b; where y=0 x=-b/m
sfnode=0.*fltarr(nbins)
wnode=0.*fltarr(nbins)
znode=0.*fltarr(nbins)
ynode=0.*fltarr(nbins)
s0=sfbin(0)
q0=avgq_vs_sf(0)
z0=avgz_vs_sf(0)
w0=avgs_vs_sf(0)
y0=avgy_vs_sf(0)
for n=1,nbins-1 do begin
    s1=sfbin(n)
    q1=avgq_vs_sf(n)
    z1=avgz_vs_sf(n)
    w1=avgs_vs_sf(n)
    y1=avgy_vs_sf(n)
    if q0*q1 le 0. then begin
       if q0 ne -999. and q1 ne -999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       ynode(n)=y0+scale*(y1-y0)        ; latitude at QDF node
       endif
       if q0 eq -999. then begin
       sfnode(n)=s1
       wnode(n)=w1
       znode(n)=z1
       ynode(n)=y1
       endif
       if q1 eq -999. then begin
       sfnode(n)=s0
       wnode(n)=w0
       znode(n)=z0
       ynode(n)=y0
       endif
    endif
    s0=s1
    q0=q1
    w0=w1
    z0=z1
    y0=y1
endfor

; eliminate sfnodes that get close to latmin
index=where(sfnode ne 0.,nnodes)
if index(0) ne -1 then begin
for l=0,nnodes-1 do begin
    s=where(lat ge latmin and sf ge sfnode(index(l)))
    if s(0) ne -1 then $
       if min(lat(s))-latmin lt dy then $
          sfnode(index(l))=0.
endfor
endif

; any anticyclonic znodes?
iindex=where(znode lt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin
; choose the "candidate" furthest poleward
;print,'NH nodes at ',ynode(iindex)
;print,'Zeta ',znode(iindex)
;print,'U ',wnode(iindex)
   jindex=where(ynode(iindex) eq max(ynode(iindex)))
   index=where(sfnode eq sfnode(iindex(jindex(0))))
   s=where(lat ge latmin and sf ge sfnode(index(0)),is)
   if is gt 2 then begin
   if max(lat(s)) eq max(lat) and min(lat(s))-latmin gt dy then begin
      mark(s)=-1.0
;     print,'NH Circumpolar High',min(lat(s))
   endif
   endif
endif   ; anticyclonic relative vorticity
;goto,plotit

; Southern Hemisphere
dosh:

nbins=20
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
avgz_vs_sf=-999.+0.*fltarr(nbins)               ; average relative vorticity per bin
avgy_vs_sf=-999.+0.*fltarr(nbins)               ; average latitude per bin
sfbin=0.0*fltarr(nbins)

latmin=-90.
latmax=-25.

kk=where(lat lt latmax)
sfmin=min(sf(kk))
sfmax=max(sf(kk))
sfint=(sfmax-sfmin)/(nbins)
; in SH highs are low SF values.  loop sfbin from low up to mark circumpolar high first
sfbin=sfmin+sfint*findgen(nbins)

; loop over SF bins in SH
for n=0,nbins-2 do begin
    t=where(lat le latmax and sf gt sfbin(n) and sf le sfbin(n+1),it)
; check latmax.  make sure bins are resolved (do not intersect latmax)
; and make sure bins are not divided by latmax
    if (it gt 2) then begin
        if abs(abs(max(lat(t)))-abs(latmax)) le dy then begin
           avgq_vs_sf(n)=-999.
           goto,jumpshbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        avgz_vs_sf(n)=total(zeta(t))/float(it)
        avgs_vs_sf(n)=total(speed(t))/float(it)
        avgy_vs_sf(n)=total(lat(t))/float(it)
    endif
    jumpshbin:
endfor
s=where(lat le latmax and sf gt sfbin(nbins-1),is)
if is gt 2 then begin
if abs(abs(max(lat(s)))-abs(latmax)) gt dy then begin
   avgq_vs_sf(nbins-1)=total(qdf(s))/float(is)
   avgz_vs_sf(nbins-1)=total(zeta(s))/float(is)
   avgs_vs_sf(nbins-1)=total(speed(s))/float(is)
   avgy_vs_sf(nbins-1)=total(lat(s))/float(is)
endif
endif
index=where(avgq_vs_sf ne -999.,nbins)
if index(0) ne -1 then begin
   avgq_vs_sf=avgq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   avgy_vs_sf=avgy_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then return
;
; adjust avgq in case of no nodes to find minimum in second derivative
;
index=where(avgq_vs_sf ne -999.0)
qmin=min(avgq_vs_sf(index))
qmax=max(avgq_vs_sf(index))
if qmax lt 0. then avgq_vs_sf(index)=avgq_vs_sf(index)-qmax+1.0

; interpolate SF values to where integrated QDF=0.0 y=mx+b; where y=0 x=-b/m
sfnode=0.*fltarr(nbins)
wnode=0.*fltarr(nbins)
znode=0.*fltarr(nbins)
ynode=0.*fltarr(nbins)
s0=sfbin(0)
q0=avgq_vs_sf(0)
w0=avgs_vs_sf(0)
z0=avgz_vs_sf(0)
y0=avgy_vs_sf(0)
for n=1,nbins-1 do begin
    s1=sfbin(n)
    q1=avgq_vs_sf(n)
    w1=avgs_vs_sf(n)
    z1=avgz_vs_sf(n)
    y1=avgy_vs_sf(n)
    if q0*q1 le 0. then begin
       if q0 ne -999. and q1 ne -999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       ynode(n)=y0+scale*(y1-y0)        ; latitude at QDF node
       endif
       if q0 eq -999. then begin
       sfnode(n)=s1
       wnode(n)=w1
       znode(n)=z1
       ynode(n)=y1
       endif
       if q1 eq -999. then begin
       sfnode(n)=s0
       wnode(n)=w0
       znode(n)=z0
       ynode(n)=y0
       endif
    endif
    s0=s1
    q0=q1
    w0=w1
    z0=z1
    y0=y1
endfor

; eliminate sfnodes that get close to latmax
index=where(sfnode ne 0.,nnodes)
if index(0) ne -1 then begin
for l=0,nnodes-1 do begin
    s=where(lat le latmax and sf le sfnode(index(l)))
    if s(0) ne -1 then $
       if abs(abs(max(lat(s)))-abs(latmax)) lt dy then $
          sfnode(index(l))=0.
endfor
endif

; any anticyclonic znodes
iindex=where(znode gt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin
; choose the "candidate" furthest poleward 
;print,'SH nodes at ',ynode(iindex)
;print,'Zeta ',znode(iindex)
;print,'U ',wnode(iindex)
   jindex=where(ynode(iindex) eq min(ynode(iindex)))
   index=where(sfnode eq sfnode(iindex(jindex(0))))
   s=where(lat le latmax and sf le sfnode(index(0)),is)
   if is gt 2 then begin
   if min(lat(s)) eq min(lat) and abs(abs(max(lat(s)))-abs(latmax)) gt dy then begin
      mark(s)=-1.0
;     print,'SH Circumpolar High',max(lat(s))
   endif
   endif
endif   ; anticyclonic relative vorticity
;
; check
;
;plotit:
;!p.multi=0
;window,xsize=600,ysize=600
;loadct,38
;device,decompose=0
;mcolor=byte(!p.color)
;a=findgen(8)*(2*!pi/8.)
;usersym,cos(a),sin(a),/fill
;nlvls=nbins
;col1=1+indgen(nlvls)*mcolor/nlvls
;erase
;!psym=0
;set_viewport,0.,1.,0.,1.
;MAP_SET,0,0,0,/noeras,/grid,/contin
;index=sort(sfbin)
;contour,sf,x,y,/overplot,nlevels=nlvls,/cell_fill,/noeras,c_color=col1
;s=where(mark lt 0.)
;if s(0) ne -1L then oplot,lon(s),lat(s),psym=8,color=0
;index=where(lat*zeta lt 0. and qdf lt 0.)
;oplot,lon(index),lat(index),psym=8,symsize=0.5,color=mcolor*0.9
;contour,sf,x,y,/overplot,nlevels=nlvls,/follow,/noeras,c_color=mcolor,thick=3
;stop
return
end
