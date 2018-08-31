pro marker_lows_v8,sf,mark,qdf,zeta,u,v,x,y,theta,pv

; version 7: deal with the subtropical jet in lower strat
; set equatorward bound at 40 degrees at and below 500 K
; set equatorward bound at 50 degrees at and below 400 K
;
; version 8: deal with SAO in the upper stratosphere and STJ in upper troposphere
; always mark poleward-most jet and don't require for there to be a node in QDF
;
plotit=0L	; set to 1 for plotting checks

nbins=20
nr=n_elements(y)
nc=n_elements(x)
dx=x(1)-x(0)
dy=y(1)-y(0)
lon=0.*sf
lat=0.*sf
for i=0,n_elements(x)-1 do lat(i,*)=y
for j=0,n_elements(y)-1 do lon(*,j)=x

if plotit eq 1 then begin
!type=2^2+2^3
loadct,38
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
window
nlvls=nbins
col1=1+indgen(nlvls)*mcolor/nlvls
endif
;
; QDF, zeta at poles bad
;
index=where(abs(lat) eq max(y))
qdf(index)=0.
zeta(index)=0.
speed=sqrt(u^2+v^2)
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
totq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per area
avgz_vs_sf=-999.+0.*fltarr(nbins)		; average relative vorticity
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
avgy_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
sfbin=0.0*fltarr(nbins)

; set NH latmin, latmax
latmin=15.
if theta le 500. then latmin=40.
if theta le 400. then latmin=50.
latmax=90.
kk=where(lat gt latmin)
sfmin=min(sf(kk))
sfmax=max(sf(kk))
if sfmax eq 0. then return
sfint=(sfmax-sfmin)/(nbins)
sfbin=sfmin+sfint*findgen(nbins)
;
; if theta surface is discontinuous poleward of 40 degrees latitude
;
index=where(pv eq 1.00000e+12)
if index(0) ne -1L then begin
   print,theta,' discontinuous up to ',max(lat(index))
   if max(lat(index)) gt 20. then goto,dosh
endif
;
; check
;
if plotit eq 1 then begin
erase
set_viewport,.05,.45,.55,.95
map_set,90,0,-90,/stereo,/contin,/grid,/noeras,title=string(theta)
index=where(zeta gt 0. and qdf lt 0. and lat gt 0.)
oplot,lon(index),lat(index),psym=2,color=mcolor*.9
contour,sf,x,y,/overplot,levels=sfbin,color=mcolor,/follow
contour,speed,x,y,/overplot,levels=[30.],thick=4,color=mcolor*0.3
contour,speed,x,y,/overplot,levels=[40.],thick=4,color=mcolor*0.6
contour,speed,x,y,/overplot,levels=[50.],thick=4,color=mcolor*0.7
contour,speed,x,y,/overplot,levels=[60.],thick=4,color=mcolor*0.8
contour,speed,x,y,/overplot,levels=[70.],thick=4,color=mcolor*0.9
endif

; loop over SF bins in NH
for n=0,nbins-2 do begin
    t=where(lat ge latmin and sf gt sfbin(n) and sf le sfbin(n+1),it)
    s=where(lat ge latmin and sf le sfbin(n+1),is)	; NH vortex is low SF bins
; check latmin.  make sure bins are resolved (do not intersect latmin)
    if (it gt 2) then begin
        if min(lat(t))-latmin le dy then begin
           avgq_vs_sf(n)=999.
           goto,jumpnhbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        totq_vs_sf(n)=total(qdf(s))/float(is)
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
   totq_vs_sf=totq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   avgy_vs_sf=avgy_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then goto,dosh
;
; smooth Q vs SF to eliminate "small" nodes
;
index=where(avgq_vs_sf ne -999. and avgq_vs_sf ne 999.,ngood)
if ngood lt nbins/2 then goto,dosh
;avgq_vs_sf(index)=smooth(avgq_vs_sf(index),3)		; don't smooth out Q peaks
avgq_vs_sf(index)=avgq_vs_sf(index)+10.			; in fact, raise them up to encourage zero crossings

if plotit eq 1 then begin
set_viewport,.55,.95,.55,.95
index=where(avgy_vs_sf ne -999.)
plot,sfbin(index),avgy_vs_sf(index),/noeras,psym=8,title='Lat vs SF bin',xrange=[min(sfbin),max(sfbin)],yrange=[15,90]
oplot,sfbin,avgy_vs_sf,thick=2
set_viewport,.05,.45,.05,.45
plot,sfbin(index),avgs_vs_sf(index),xrange=[min(sfbin),max(sfbin)],yrange=[0.,100.],/noeras,psym=8,title='Speed vs SF bin'
oplot,sfbin(index),avgs_vs_sf(index),thick=2
axis,yaxis=1,yrange=[-400.,400.],/save
oplot,sfbin(index),avgq_vs_sf(index),thick=2,color=mcolor*.9
oplot,sfbin(index),totq_vs_sf(index),thick=2,color=mcolor*.6
plots,min(sfbin),0
plots,max(sfbin),0,/continue
xyouts,max(sfbin),420,'QDF',color=mcolor*.9,/data
endif

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
       if q0 ne 999. and q1 ne 999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       ynode(n)=y0+scale*(y1-y0)        ; latitude at QDF node
       endif
;      if q0 eq 999. then begin
;      sfnode(n)=s1
;      wnode(n)=w1
;      znode(n)=z1
;      ynode(n)=y1
;      endif
;      if q1 eq 999. then begin
;      sfnode(n)=s0
;      wnode(n)=w0
;      znode(n)=z0
;      ynode(n)=y0
;      endif
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
    s=where(lat ge latmin and sf le sfnode(index(l)))
    if s(0) ne -1 then $
       if min(lat(s))-latmin lt dy then $
          sfnode(index(l))=0.
endfor
endif

; all cyclonic znodes
iindex=where(znode gt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin
if plotit eq 1 then oplot,sfnode(iindex),0.*sfnode(iindex),psym=8,symsize=2

; choose the "candidate" with largest wind speed from 500 to 2000 K
;  if theta ge 500. and theta le 2000. then jindex=where(wnode(iindex) eq max(wnode(iindex)))
; choose the "candidate" furthest poleward above and below
;  if theta lt 500. or theta gt 2000. then jindex=where(ynode(iindex) eq max(ynode(iindex)))
   jindex=where(ynode(iindex) eq max(ynode(iindex)))

   index=where(sfnode eq sfnode(iindex(jindex(0))))
if wnode(index(0)) lt 15. then goto,dosh
if plotit eq 1 then oplot,sfnode(index),0.*sfnode(index),psym=8,symsize=3,color=mcolor*.9

   s=where(lat ge latmin and sf le sfnode(index(0)),is)
   if is gt 2 and min(lat(s))-latmin gt dy then begin
      mark(s)=1.0
if plotit eq 1 then begin
set_viewport,.05,.45,.55,.95
map_set,90,0,-90,/stereo,/contin,/grid,/noeras,title=string(theta)
contour,sf,x,y,/overplot,levels=sfnode(index(0)),color=mcolor*.5,/follow,thick=6
stop
endif
     print,'Arctic vortex',theta,max(wnode(iindex))
   endif
endif   ; cyclonic relative vorticity

; Southern Hemisphere
dosh:

nbins=20
avgq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per bin
totq_vs_sf=-999.+0.*fltarr(nbins)               ; average QDF per area
avgs_vs_sf=-999.+0.*fltarr(nbins)               ; average windspeed per bin
avgz_vs_sf=-999.+0.*fltarr(nbins)               ; average relative vorticity per bin
avgy_vs_sf=-999.+0.*fltarr(nbins)               ; average latitude per bin (SF based Equivalent Latitude)
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
;
; if theta surface is discontinuous poleward of 40 degrees latitude
;
index=where(pv eq 1.00000e+12)
if index(0) ne -1L then begin
   print,theta,' discontinuous up to ',min(lat(index))
   if min(lat(index)) lt -20. then return
endif

if plotit eq 1 then begin
erase
set_viewport,.05,.45,.55,.95
map_set,-90,0,-90,/stereo,/contin,/grid,/noeras,title=string(theta)
index=where(qdf lt 0. and lat lt 0. and zeta lt 0.)
oplot,lon(index),lat(index),psym=2,color=mcolor*.9
contour,sf,x,y,/overplot,levels=reverse(sfbin),color=mcolor,/follow
contour,speed,x,y,/overplot,levels=[30.],thick=4,color=mcolor*0.3
contour,speed,x,y,/overplot,levels=[40.],thick=4,color=mcolor*0.6
contour,speed,x,y,/overplot,levels=[50.],thick=4,color=mcolor*0.7
contour,speed,x,y,/overplot,levels=[60.],thick=4,color=mcolor*0.8
contour,speed,x,y,/overplot,levels=[70.],thick=4,color=mcolor*0.9
endif

; loop over SF bins in SH
for n=0,nbins-2 do begin
    t=where(lat le latmax and sf lt sfbin(n) and sf ge sfbin(n+1),it)
    s=where(lat le latmax and sf ge sfbin(n+1),is)		; SH vortex is high SF bins
; check latmax.  make sure bins are resolved (do not intersect latmax)
; and make sure bins are not divided by latmax
    if (it gt 2) then begin
        if abs(abs(max(lat(t)))-abs(latmax)) le dy then begin
           avgq_vs_sf(n)=999.
           goto,jumpshbin
        endif
        avgq_vs_sf(n)=total(qdf(t))/float(it)
        totq_vs_sf(n)=total(qdf(s))/float(is)
        avgz_vs_sf(n)=total(zeta(t))/float(it)
        avgs_vs_sf(n)=total(speed(t))/float(it)
        avgy_vs_sf(n)=total(lat(t))/float(it)
    endif
    jumpshbin:
endfor
s=where(lat le latmax and sf lt sfbin(nbins-1),is)
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
   totq_vs_sf=totq_vs_sf(index)
   avgz_vs_sf=avgz_vs_sf(index)
   avgs_vs_sf=avgs_vs_sf(index)
   avgy_vs_sf=avgy_vs_sf(index)
   sfbin=sfbin(index)
endif
if index(0) eq -1 then return
;
; smooth Q vs SF to eliminate "small" nodes
;
index=where(avgq_vs_sf ne -999. and avgq_vs_sf ne 999.,ngood)
if ngood lt nbins/2 then return
;avgq_vs_sf(index)=smooth(avgq_vs_sf(index),3)
avgq_vs_sf(index)=avgq_vs_sf(index)+10.                 ; in fact, raise them up to encourage zero crossings

if plotit eq 1 then begin
set_viewport,.55,.95,.55,.95
index=where(avgy_vs_sf ne -999.)
plot,sfbin(index),avgy_vs_sf(index),/noeras,psym=8,title='Lat vs SF bin',xrange=[min(sfbin),max(sfbin)],yrange=[-90,-15]
oplot,sfbin,avgy_vs_sf,thick=2
set_viewport,.05,.45,.05,.45
plot,sfbin(index),avgs_vs_sf(index),xrange=[min(sfbin),max(sfbin)],yrange=[0.,100.],/noeras,psym=8,title='Speed vs SF bin'
oplot,sfbin(index),avgs_vs_sf(index),thick=2
axis,yaxis=1,yrange=[-400.,400.],/save
oplot,sfbin(index),avgq_vs_sf(index),thick=2,color=mcolor*.9
oplot,sfbin(index),totq_vs_sf(index),thick=2,color=mcolor*.6
plots,min(sfbin),0
plots,max(sfbin),0,/continue
xyouts,0,420,'QDF',color=mcolor*.9,/data
endif

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
       if q0 ne 999. and q1 ne 999. then begin
       slope=(q0-q1)/(s0-s1)
       ycept=q0-slope*s0
       sfnode(n)=-ycept/slope           ; streamfunction at QDF node
       ss=sfnode(n)
       scale=(s0-ss)/(s0-s1)
       wnode(n)=w0+scale*(w1-w0)        ; wind speed at QDF node
       znode(n)=z0+scale*(z1-z0)        ; relative vorticity at QDF node
       ynode(n)=y0+scale*(y1-y0)        ; latitude at QDF node
       endif
;      if q0 eq 999. then begin
;      sfnode(n)=s1
;      wnode(n)=w1
;      znode(n)=z1
;      ynode(n)=y1
;      endif
;      if q1 eq 999. then begin
;      sfnode(n)=s0
;      wnode(n)=w0
;      znode(n)=z0
;      ynode(n)=y0
;      endif
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
    s=where(lat le latmax and sf ge sfnode(index(l)))
    if s(0) ne -1 then $
       if abs(abs(max(lat(s)))-abs(latmax)) lt dy then $
          sfnode(index(l))=0.
endfor
endif

; all cyclonic znodes
iindex=where(znode lt 0. and sfnode ne 0.)
if iindex(0) ne -1 then begin

;oplot,sfnode(iindex),0.*sfnode(iindex),psym=8,symsize=2
; choose the "candidate" with largest wind speed from 500 to 2000 K
;  if theta ge 500. and theta le 2000. then jindex=where(wnode(iindex) eq max(wnode(iindex)))
; choose the "candidate" furthest poleward above and below
;  if theta lt 500. or theta gt 2000. then jindex=where(ynode(iindex) eq min(ynode(iindex)))
   jindex=where(ynode(iindex) eq min(ynode(iindex)))

   index=where(sfnode eq sfnode(iindex(jindex(0))))
if wnode(index(0)) lt 15. then return
if plotit eq 1 then oplot,sfnode(index),0.*sfnode(index),psym=8,symsize=3,color=mcolor*.9

   s=where(lat le latmax and sf ge sfnode(index(0)),is)
   if is gt 2 and abs(abs(max(lat(s)))-abs(latmax)) gt dy then begin
      mark(s)=1.0
if plotit eq 1 then begin
set_viewport,.05,.45,.55,.95
map_set,-90,0,-90,/stereo,/contin,/grid,/noeras,title=string(theta)
contour,sf,x,y,/overplot,levels=sfnode(index(0)),color=mcolor*.5,/follow,thick=6
stop
endif
print,'Antarctic vortex',theta,max(wnode(iindex))
   endif
endif   ; cyclonic relative vorticity
return
end
