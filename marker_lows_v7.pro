pro marker_lows_v7,sf,mark,qdf,zeta,u,v,x,y,theta

; version 7: deal with the subtropical jet in lower strat
; set equatorward bound at 40 degrees at and below 500 K
; set equatorward bound at 50 degrees at and below 400 K

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
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
avgz_vs_sf=-999.+0.*fltarr(nbins)		; average relative vorticity
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
sfbin=0.0*fltarr(nbins)

; set NH latmin, latmax
latmin=15.
if theta le 500. then latmin=40.
if theta le 400. then latmin=50.
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
           avgq_vs_sf(n)=999.
           goto,jumpnhbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        avgz_vs_sf(n)=total(zeta(t))/float(it)
        avgs_vs_sf(n)=total(speed(t))/float(it)
    endif
    jumpnhbin:
endfor	; loop over bins
s=where(lat ge latmin and sf gt sfbin(nbins-1),is)
if is gt 2 then begin
if min(lat(s))-latmin gt dy then begin
   avgq_vs_sf(nbins-1)=total(qdf(s))/float(is)
   avgz_vs_sf(nbins-1)=total(zeta(s))/float(is)
   avgs_vs_sf(nbins-1)=total(speed(s))/float(is)
endif
endif
index=where(avgq_vs_sf ne -999.,nbins)
if index(0) ne -1 then begin
   avgq_vs_sf=avgq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then goto,dosh

; interpolate SF values to where integrated QDF=0.0 y=mx+b; where y=0 x=-b/m
sfnode=0.*fltarr(nbins)
wnode=0.*fltarr(nbins)
znode=0.*fltarr(nbins)
s0=sfbin(0)
q0=avgq_vs_sf(0)
z0=avgz_vs_sf(0)
w0=avgs_vs_sf(0)
for n=1,nbins-1 do begin
    s1=sfbin(n)
    q1=avgq_vs_sf(n)
    z1=avgz_vs_sf(n)
    w1=avgs_vs_sf(n)
    if q0*q1 le 0. then begin
       if q0 ne 999. and q1 ne 999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       endif
       if q0 eq 999. then begin
       sfnode(n)=s1
       wnode(n)=w1
       znode(n)=z1
       endif
       if q1 eq 999. then begin
       sfnode(n)=s0
       wnode(n)=w0
       znode(n)=z0
       endif
    endif
    s0=s1
    q0=q1
    w0=w1
    z0=z1
endfor

; eliminate sfnodes that get close to latmin
index=where(sfnode ne 0.,nnodes)
if index(0) ne -1 then begin
for l=0,nnodes-1 do begin
    s=where(lat ge latmin and sf le sfnode(index(l)))
    if s(0) ne -1 then $
       if min(lat(s))-latmin lt dy then $
          sfnode(index(l))=0.
endfor
endif

; all cyclonic znodes
iindex=where(znode gt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin
; choose the "candidate" with largest wind speed
   jindex=where(wnode(iindex) eq max(wnode(iindex)))
   index=where(sfnode eq sfnode(iindex(jindex(0))))
   s=where(lat ge latmin and sf le sfnode(index(0)),is)
   if is gt 2 and min(lat(s))-latmin gt dy then begin
      mark(s)=1.0
;     print,'Arctic vortex',theta,max(wnode(iindex))
   endif
endif   ; cyclonic relative vorticity

; Southern Hemisphere
dosh:

nbins=20
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
avgz_vs_sf=-999.+0.*fltarr(nbins)               ; average relative vorticity per bin
sfbin=0.0*fltarr(nbins)

latmin=-90.
latmax=-15.
if theta le 500. then latmax=-40.
if theta le 400. then latmax=-50.

kk=where(lat lt latmax)
sfmin=min(sf(kk))
sfmax=max(sf(kk))
sfint=(sfmax-sfmin)/(nbins)
; in SH lows are high SF values.  loop sfbin from high down so mark
; polar vortex first
sfbin=sfmax-sfint*findgen(nbins)

; loop over SF bins in SH
for n=0,nbins-2 do begin
    t=where(lat le latmax and sf lt sfbin(n) and sf ge sfbin(n+1),it)
; check latmax.  make sure bins are resolved (do not intersect latmax)
; and make sure bins are not divided by latmax
    if (it gt 2) then begin
        if abs(abs(max(lat(t)))-abs(latmax)) le dy then begin
           avgq_vs_sf(n)=999.
           goto,jumpshbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        avgz_vs_sf(n)=total(zeta(t))/float(it)
        avgs_vs_sf(n)=total(speed(t))/float(it)
    endif
    jumpshbin:
endfor
s=where(lat le latmax and sf lt sfbin(nbins-1),is)
if is gt 2 then begin
if abs(abs(max(lat(s)))-abs(latmax)) gt dy then begin
   avgq_vs_sf(nbins-1)=total(qdf(s))/float(is)
   avgz_vs_sf(nbins-1)=total(zeta(s))/float(is)
   avgs_vs_sf(nbins-1)=total(speed(s))/float(is)
endif
endif
index=where(avgq_vs_sf ne -999.,nbins)
if index(0) ne -1 then begin
   avgq_vs_sf=avgq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then return

; interpolate SF values to where integrated QDF=0.0 y=mx+b; where y=0 x=-b/m
sfnode=0.*fltarr(nbins)
wnode=0.*fltarr(nbins)
znode=0.*fltarr(nbins)
s0=sfbin(0)
q0=avgq_vs_sf(0)
w0=avgs_vs_sf(0)
z0=avgz_vs_sf(0)
for n=1,nbins-1 do begin
    s1=sfbin(n)
    q1=avgq_vs_sf(n)
    w1=avgs_vs_sf(n)
    z1=avgz_vs_sf(n)
    if q0*q1 le 0. then begin
       if q0 ne 999. and q1 ne 999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       endif
       if q0 eq 999. then begin
       sfnode(n)=s1
       wnode(n)=w1
       znode(n)=z1
       endif
       if q1 eq 999. then begin
       sfnode(n)=s0
       wnode(n)=w0
       znode(n)=z0
       endif
    endif
    s0=s1
    q0=q1
    w0=w1
    z0=z1
endfor

; eliminate sfnodes that get close to latmax
index=where(sfnode ne 0.,nnodes)
if index(0) ne -1 then begin
for l=0,nnodes-1 do begin
    s=where(lat le latmax and sf ge sfnode(index(l)))
    if s(0) ne -1 then $
       if abs(abs(max(lat(s)))-abs(latmax)) lt dy then $
          sfnode(index(l))=0.
endfor
endif

; all cyclonic znodes
iindex=where(znode lt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin
; choose the "candidate" with largest wind speed
   jindex=where(wnode(iindex) eq max(wnode(iindex)))
   index=where(sfnode eq sfnode(iindex(jindex(0))))
   s=where(lat le latmax and sf ge sfnode(index(0)),is)
   if is gt 2 and abs(abs(max(lat(s)))-abs(latmax)) gt dy then begin
      mark(s)=1.0
;     print,'Antarctic vortex',theta,max(wnode(iindex))
   endif
endif   ; cyclonic relative vorticity
return
end
