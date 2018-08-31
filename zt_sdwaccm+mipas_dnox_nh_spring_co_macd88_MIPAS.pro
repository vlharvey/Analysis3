;
; MIPAS
; dNOx=NOx-NOx on 01/01
; spring time period only
; plot time-altitude section of NH NOx
;
@kgmt
@fillit
@smoothit

loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
setplot='x'
read,'setplot=',setplot
mcolor=icolmax
icmm1=icolmax-1
icmm2=icolmax-2
nxdim=800 & nydim=800
xorig=[0.1,0.1,0.1,0.55,0.55,0.55]
yorig=[0.7,0.4,0.1,0.7,0.4,0.1]
xlen=0.4
ylen=0.2
cbaryoff=0.01
cbarydel=0.03
!NOERAS=-1
!p.font=1
!x.ticklen=-0.05
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
   !p.background=mcolor
endif
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
;  !p.font=0
   !p.charthick=3
   device,font_size=8
   device,/landscape,bits=8,filename='zt_sdwaccm_nox_nh_spring_7pan+co.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize
endif
erase
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
syear=['2004']	;,'2006','2008','2009','2010','2011','2012']
nyear=n_elements(syear)
;
; loop over years
;
for iyear=0L,nyear-1L do begin
;
; restore MIPAS data
;
lc=icolmax
if syear(iyear) eq '2004' then lc=0
spawn,'ls /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.'+syear(iyear)+'*.nc_MIPAS.sav',ifiles
ifiles=['/Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.2003-12-26-03600.nc_MIPAS.sav',ifiles]
nfile=n_elements(ifiles)
icount=0L
for ifile=0L,nfile-1L do begin
    restore,ifiles(ifile)
    print,ifiles(ifile)
    index=where(latitude gt 0.,nprof)
    if index(0) ne -1L then begin
       if icount ne 0L then begin
          instrument_all=[instrument_all,INSTRUMENT(index)]
          date_all=[date_all,DATE(index)]
          fdoy_all=[fdoy_all,FDOY(index)]
          latitude_all=[latitude_all,LATITUDE(index)]
          longitude_all=[longitude_all,LONGITUDE(index)]
          ltime_all=[ltime_all,LTIME(index)]
          sctype_all=[sctype_all,SCTYPE(index)]
          comix=[comix,CO(index,*)]
          nomix=[nomix,NO(index,*)]
          no2mix=[no2mix,NO2(index,*)]
          o3mix=[o3mix,O3(index,*)]
          press=[press,P(index,*)]
          temp=[temp,T(index,*)]
          altitude=[altitude,Z(index,*)]
       endif
       if icount eq 0L then begin
          instrument_all=INSTRUMENT(index)
          date_all=DATE(index)
          fdoy_all=FDOY(index)
          latitude_all=LATITUDE(index)
          longitude_all=LONGITUDE(index)
          ltime_all=LTIME(index)
          sctype_all=SCTYPE(index)
          comix=CO(index,*)              
          nomix=NO(index,*)
          no2mix=NO2(index,*)
          o3mix=O3(index,*)
          press=P(index,*)
          temp=T(index,*)
          altitude=Z(index,*)
          icount=1L
       endif
;help,ifile,z(index,*),altitude
    endif
endfor
;
; extract MIPAS
;
index=where(instrument_all eq 9L)
if index(0) eq -1L then stop,'No MIPAS data'
date_all=DATE_all(index)
fdoy_all=FDOY_all(index)
latitude_all=LATITUDE_all(index)
longitude_all=LONGITUDE_all(index)
ltime_all=LTIME_all(index)
sctype_all=SCTYPE_all(index)
comix=COmix(index,*)
nomix=NOmix(index,*)
no2mix=NO2mix(index,*)
o3mix=O3mix(index,*)
press=PRESS(index,*)
temp=TEMP(index,*)
altitude=altitude(index,*)
;
; rename
;
fdoy=fdoy_all
latitude=latitude_all
altitude_save=altitude/1000.
noxmix=nomix+no2mix
;
; daily average NOx in the vortex
;
ithresh=0.0
    nday=long(max(fdoy))-long(min(fdoy))+1L
    nday=365
    nz=n_elements(lev)
    onox=fltarr(nday,nz)
    oco=fltarr(nday,nz)
    otemp=fltarr(nday,nz)
    oo3=fltarr(nday,nz)
    altitude=fltarr(nday,nz)
    num=lonarr(nday,nz)
    for iday=1L,nday do begin
        today=where(long(fdoy) eq iday and latitude gt 0.,nprof)	; Arctic
        if nprof le 1L then goto,skipday1
        noxday=reform(noxmix(today,*))
        coday=reform(comix(today,*))
        tempday=reform(temp(today,*))
        o3day=reform(o3mix(today,*))
        altday=reform(altitude_save(today,*))
        for iprof=0L,nprof-1L do begin
            noxs=reform(noxday(iprof,*))
            cos0=reform(coday(iprof,*))
            tp0=reform(tempday(iprof,*))
            o30=reform(o3day(iprof,*))
            zprof=reform(altday(iprof,*))
            if latitude(today(iprof)) ge 70. then begin
               for k=nz-1L,0L,-1L do begin	; loop from the bottom up
                   if noxs(k) gt ithresh and cos0(k) ne -99. then begin
                      onox(iday-1L,k)=onox(iday-1L,k)+noxs(k)
                      oco(iday-1L,k)=oco(iday-1L,k)+cos0(k)
                      otemp(iday-1L,k)=otemp(iday-1L,k)+tp0(k)
                      oo3(iday-1L,k)=oo3(iday-1L,k)+o30(k)
                      altitude(iday-1L,k)=altitude(iday-1L,k)+zprof(k)
                      num(iday-1L,k)=num(iday-1L,k)+1L
                   endif
               endfor
            endif
        endfor		; loop over profiles
        skipday1:
    endfor		; loop over days
;
; daily average
;
    index=where(num gt 0L)
    if index(0) eq -1L then goto,jumpyear
    onox(index)=1.e6*onox(index)/float(num(index))
    oco(index)=1.e6*oco(index)/float(num(index))
    otemp(index)=otemp(index)/float(num(index))
    oo3(index)=1.e6*oo3(index)/float(num(index))
    altitude(index)=altitude(index)/float(num(index))
;
; fill
;
    onox_fill=onox
    for j=0,nz-1 do begin
        dummy=reform(onox(*,j))
        index1=where(dummy gt 0.,ngood)
        index2=where(dummy le 0.,nbad)
        if ngood gt 1L and nbad gt 1L then begin
           filled=interpol(dummy(index1),index1,index2)
           onox_fill(index2,j)=filled
        endif
    endfor
;    if syear(iyear) eq '2004' then begin
;       altitude(59,*)=(altitude(58,*)+altitude(60,*))/2.
;;      onox_fill(0:41,*)=0.      ; do not extrapolate data voids 
;;      index=where(fdoy lt 30.)
;;      latitude(index)=0.
;    endif
;   if syear(iyear) eq '2007' then begin
;      if max(fdoy) lt 360. then onox_fill(long(max(fdoy)):long(nday)-1L,*)=0.      ; do not extrapolate data voids
;   endif
;   if syear(iyear) eq '2008' then begin
;      if min(fdoy) gt 3. then onox_fill(0:long(min(fdoy))-1,*)=0.
;   endif
;   if syear(iyear) eq '2009' then begin
;      if max(fdoy) lt 90. then onox_fill(long(max(fdoy)):long(nday)-1L,*)=0.
;   endif
;   onox=onox_fill
;
; interpolate small gaps in time
;
for k=0,nz-1 do begin
    dlev=reform(onox(*,k))
;
; filter unphysically large values that were not flagged by mask
;
medval=median(dlev)
index=where(dlev ne 0.)
if index(0) ne -1L then medval=median(dlev(index))
index=where(dlev gt 75.*medval)
if index(0) ne -1L then dlev(index)=0.

    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 10.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump1
               endif
           endfor
jump1:
        endif
    endfor
    onox(*,k)=dlev

    dlev=reform(oco(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 10.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump2
               endif
           endfor
jump2:
        endif
    endfor
    oco(*,k)=dlev

    dlev=reform(altitude(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 10.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump3
               endif
           endfor
jump3:
        endif
    endfor
    altitude(*,k)=dlev

    dlev=reform(otemp(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 10.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump4
               endif
           endfor
jump4:
        endif
    endfor
    otemp(*,k)=dlev

    dlev=reform(oo3(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 10.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump5
               endif
           endfor
jump5:
        endif
    endfor
    oo3(*,k)=dlev

endfor
;
; smooth
;
;   fillit,onox,onoxfill
;   fillit,oco,ocofill
;   onox=onoxfill
;   oco=ocofill
;   smoothit,onox,onoxsmooth
;   smoothit,oco,ocosmooth
;   wnox=onoxsmooth
;   wco=ocosmooth
;   waltitude=altitude
wnox=smooth(onox,3,/edge_truncate,/nan)
wco=smooth(oco,3,/edge_truncate,/nan)
waltitude=smooth(altitude,3,/edge_truncate,/nan)
wtemp=smooth(otemp,3,/edge_truncate,/nan)
wo3=smooth(oo3,3,/edge_truncate,/nan)
;
; subtract 1 Jan
;
dnox=wnox
wprof=reform(wnox(0,*))
for i=0L,nday-1L do dnox(i,*)=100.*(wnox(i,*)-wprof)/wnox(i,*)
dtemp=wtemp
tprof=reform(wtemp(15,*))
for i=0L,nday-1L do dtemp(i,*)=(wtemp(i,*)-tprof)	;/wnox(i,*)
index=where(wtemp eq 0.)
if index(0) ne -1L then dtemp(index)=0./0.
dco=wco
coprof=reform(wco(15,*))
for i=0L,nday-1L do dco(i,*)=100.*(wco(i,*)-coprof)/wco(i,*)

;
    !type=2^2+2^3
    xmn=xorig(iyear)
    xmx=xorig(iyear)+xlen
    ymn=yorig(iyear)
    ymx=yorig(iyear)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    level=[-1000.,-700.,-400.,-200,-150,-100,-70,-50,-40,-30,-20,-10,10.,20.,30.,40.,50.,70.,100.,150.]
;          500.,1000.,2000.,5000.,10000.,20000.,50000.,100000.,2.e5,5.e5,1.e6]/10000.
    nlvls=n_elements(level)
    col1=1L+indgen(nlvls)*mcolor/float(nlvls)
    print,min(fdoy),max(fdoy)
    kday=365.
    leapdy=(long(syear(iyear)) mod 4)
    kday=kday+leapdy
;kday=long(max(fdoy))	; uncomment to "zoom" in on partial year
kday=91
    xlab=[' ',' ',' ',' ']
;   if iyear eq nyear-1L then xlab=['Jan','Feb','Mar']
index=where(wnox eq 0.)
if index(0) ne -1L then wnox(index)=0./0.
if index(0) ne -1L then wco(index)=0./0.
ititle=' '
if iyear eq 0L then ititle='SD-WACCM dNOx'
    contour,dnox,1.+findgen(nday),waltitude,/noeras,c_color=col1,/cell_fill,levels=level,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title=ititle
index=where(level lt 0)
    contour,dnox,1.+findgen(nday),waltitude,/noeras,color=mcolor,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=1,c_linestyle=5
index=where(level gt 0)
    contour,dnox,1.+findgen(nday),waltitude,/noeras,color=0,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=1
    tlevel=180.+5.*findgen(nlvls)
loadct,0
   xyouts,5.,35.,syear(iyear),/data,color=200,charsize=5,charthick=10
loadct,39
;
    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif
!type=2^2+2^3+2^5
xmnb=xorig(0)+xlen+cbaryoff
xmxb=xmnb+cbarydel
ymnb=yorig(0)
ymxb=yorig(0)+ylen
set_viewport,xmnb,xmxb,ymnb,ymxb
slab=' '+strarr(n_elements(level))
plot,[0,0],[min(level),max(level)],xrange=[0,10],color=0,$
     yticks=n_elements(level)-1L,ytickname=slab,$
     yrange=[min(level),max(level)],charsize=2,title='(%)',charthick=2
xbox=[0,10,10,0,0]
y1=min(level)
dy=(max(level)-min(level))/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor
;slab=strcompress(string(format='(f7.3)',level),/remove_all)
slab=strcompress(string(format='(i4)',level),/remove_all)
y1=min(level)+dy/2
for i=0L,n_elements(slab)-1L do begin
    slab0=slab(i)
    flab0=float(slab(i))
;   if flab0 lt 0.01 then begin
;      slab0=strcompress(string(format='(f5.3)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=1.4,/data,color=mcolor,charthick=2
;   endif
;   if flab0 lt 1. and flab0 ge 0.01 then begin
;      slab0=strcompress(string(format='(f4.2)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
;   endif
    if abs(flab0) ge 1. then begin
       slab0=strcompress(long(slab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
    endif
    y1=y1+dy
endfor
;
; CO
;
    !type=2^2+2^3
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,dco,1.+findgen(nday),waltitude,/noeras,c_color=col1,/cell_fill,levels=level,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title='dCO'
t2level=-100.+10.*findgen(nlvls)
index=where(level lt 0)
    contour,dco,1.+findgen(nday),waltitude,/noeras,color=mcolor,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=2
index=where(level gt 0)
    contour,dco,1.+findgen(nday),waltitude,/noeras,color=0,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=2

    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif
!type=2^2+2^3+2^5
xmnb=xorig(2)+xlen+cbaryoff
xmxb=xmnb+cbarydel
ymnb=yorig(2)
ymxb=yorig(2)+ylen
set_viewport,xmnb,xmxb,ymnb,ymxb
slab=' '+strarr(n_elements(level))
plot,[0,0],[min(level),max(level)],xrange=[0,10],color=0,$
     yticks=n_elements(level)-1L,ytickname=slab,$
     yrange=[min(level),max(level)],charsize=2,title='(%)',charthick=2
xbox=[0,10,10,0,0]
y1=min(level)
dy=(max(level)-min(level))/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor
;slab=strcompress(string(format='(f7.3)',level),/remove_all)
slab=strcompress(string(format='(i4)',level),/remove_all)
y1=min(level)+dy/2
for i=0L,n_elements(slab)-1L do begin
    slab0=slab(i)
    flab0=float(slab(i))
;   if flab0 lt 0.01 then begin
;      slab0=strcompress(string(format='(f5.3)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=1.4,/data,color=mcolor,charthick=2
;   endif
;   if flab0 lt 1. and flab0 ge 0.01 then begin
;      slab0=strcompress(string(format='(f4.2)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
;   endif
    if abs(flab0) ge 1. then begin
       slab0=strcompress(long(slab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
    endif
    y1=y1+dy
endfor

;
; ozone
;
    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    o3level=.35*findgen(nlvls)
    colevel=[0.01,0.1,0.2,0.5,1,2.,3,4,5.,7.5,10.,15,20.,25,30.,35,40.,50.,60.,75.]

    contour,wco,1.+findgen(nday),waltitude,/noeras,c_color=col1,/cell_fill,levels=colevel,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title='CO'
    contour,wco,1.+findgen(nday),waltitude,/noeras,color=mcolor,/follow,levels=colevel(0:-1:2),/overplot,$
         c_labels=0*tlevel,thick=3
    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif

!type=2^2+2^3+2^5
xmnb=xorig(1)+xlen+cbaryoff
xmxb=xmnb+cbarydel
ymnb=yorig(1)
ymxb=yorig(1)+ylen
set_viewport,xmnb,xmxb,ymnb,ymxb
slab=' '+strarr(n_elements(colevel))
plot,[0,0],[min(colevel),max(colevel)],xrange=[0,10],color=0,$
     yticks=n_elements(colevel)-1L,ytickname=slab,$
     yrange=[min(colevel),max(colevel)],charsize=2,title='(ppmv)',charthick=2
xbox=[0,10,10,0,0]
y1=min(colevel)
dy=(max(colevel)-min(colevel))/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor
;slab=strcompress(string(format='(f7.3)',colevel),/remove_all)
slab=strcompress(string(format='(i4)',colevel),/remove_all)
y1=min(colevel)+dy/2
for i=0L,n_elements(slab)-1L do begin
    slab0=slab(i)
    flab0=float(slab(i))
;   if flab0 lt 0.01 then begin
;      slab0=strcompress(string(format='(f5.3)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=1.4,/data,color=mcolor,charthick=2
;   endif
;   if flab0 lt 1. and flab0 ge 0.01 then begin
;      slab0=strcompress(string(format='(f4.2)',flab0),/remove_all)
;      xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
;   endif
;   if abs(flab0) ge 1. then begin
       slab0=strcompress(long(slab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
;   endif
    y1=y1+dy
endfor
;goto,plotmipas
;
; MIPAS are different numbers of profiles so need to match up to sum no and no2. 
; using Laura's zt files for now
;
restore,'/Volumes/Data/MIPAS_data/Datfiles/no.sav
noalt=altitude
nolat=latitude
nolon=longitude
nojday=jday
restore,'/Volumes/Data/MIPAS_data/Datfiles/no2.sav
no2alt=altitude
no2lat=latitude
no2lon=longitude
no2jday=jday
restore,'/Volumes/Data/MIPAS_data/Datfiles/co.sav
coalt=altitude
colat=latitude
colon=longitude
cojday=jday
;
; first remove equatorward of 70N
;
index=where(nolat ge 70.)
nolat=nolat(index)
nolon=nolon(index)
nojday=nojday(index)
no=reform(no(index,*))
caldat, nojday, month, day, year
smon=string(format='(i2.2)',month)
sday=string(format='(i2.2)',day)
syear=strcompress(year,/remove_all)
nosdate=syear+smon+sday

index=where(no2lat ge 70.)
no2lat=no2lat(index)
no2lon=no2lon(index)
no2jday=no2jday(index)
no2=reform(no2(index,*))
caldat, no2jday, month, day, year
smon=string(format='(i2.2)',month)
sday=string(format='(i2.2)',day)
syear=strcompress(year,/remove_all)
no2sdate=syear+smon+sday

index=where(colat ge 70.)
colat=colat(index)
colon=colon(index)
cojday=cojday(index)
co=reform(co(index,*))
caldat, cojday, month, day, year
smon=string(format='(i2.2)',month)
sday=string(format='(i2.2)',day)
syear=strcompress(year,/remove_all)
cosdate=syear+smon+sday
;
; start on 20040101
;
index=where(nosdate ge '20040101')
nolat=nolat(index)
nolon=nolon(index)
nojday=nojday(index)
no=reform(no(index,*))
nosdate=nosdate(index)

index=where(no2sdate ge '20040101')
no2lat=no2lat(index)
no2lon=no2lon(index)
no2jday=no2jday(index)
no2=reform(no2(index,*))
no2sdate=no2sdate(index)

index=where(cosdate ge '20040101')
colat=colat(index)
colon=colon(index)
cojday=cojday(index)
co=reform(co(index,*))
cosdate=cosdate(index)
;
; merge NO and NO2. NO2 has less measurements. flag NO coincident with NO2
;
flag=fltarr(n_elements(no2lat))
for i=0L,n_elements(no2lat)-1L do begin
    index=where(abs(nolat-no2lat(i)) le 0.1 and abs(nolon-no2lon(i)) le 0.1 and abs(nojday-no2jday(i)) eq 0L)
    if index(0) ne -1L then flag(i)=1.0
;   if index(0) ne -1L then print,i,no2lat(i),no2lon(i),no2jday(i)
endfor
index=where(flag eq 1.,nprof)
print,n_elements(no2lat),n_elements(index)
;
; *** eliminate any non matching NO and NO2 ***
;
nolat=nolat(index)
nolon=nolon(index)
nojday=nojday(index)
no=reform(no(index,*))
nosdate=nosdate(index)
no2lat=no2lat(index)
no2lon=no2lon(index)
no2jday=no2jday(index)
no2=reform(no2(index,*))
no2sdate=no2sdate(index)
;
; now interpolate both to a common grid
;
nz=121
altitude_save=findgen(nz)
nogrid=fltarr(nprof,nz)
no2grid=fltarr(nprof,nz)

for i=0L,nprof-1L do begin
for kk=0L,nz-2L do begin                   ; loop over SOSST altitudes except 0 km
    zlev=altitude_save(kk)                           ; interpolate MIPAS to zlev
    for k=0L,n_elements(noalt)-2L do begin                     ; loop over MPAS altitudes 
        z0=noalt(k) & z1=noalt(k+1L)
        if z0 le zlev and z1 ge zlev then begin
           zscale=(z1-zlev)/(z1-z0)
           if no(i,k+1L) ne -99. and no(i,k) ne -99. then nogrid(i,kk)=no(i,k+1)-zscale*(no(i,k+1L)-no(i,k))
        endif
    endfor

    for k=0L,n_elements(no2alt)-2L do begin                     ; loop over MPAS altitudes
        z0=no2alt(k) & z1=no2alt(k+1L)
        if z0 le zlev and z1 ge zlev then begin
           zscale=(z1-zlev)/(z1-z0)
           if no2(i,k+1L) ne -99. and no2(i,k) ne -99. then no2grid(i,kk)=no2(i,k+1)-zscale*(no2(i,k+1L)-no2(i,k))
        endif
    endfor
endfor
endfor
;
; grid CO separately
;
nprof=n_elements(colat)
cogrid=fltarr(nprof,nz)

for i=0L,nprof-1L do begin
for kk=0L,nz-2L do begin                   ; loop over SOSST altitudes except 0 km
    zlev=altitude_save(kk)                           ; interpolate MIPAS to zlev
    for k=0L,n_elements(coalt)-2L do begin                     ; loop over MPAS altitudes
        z0=coalt(k) & z1=coalt(k+1L)
        if z0 le zlev and z1 ge zlev then begin
           zscale=(z1-zlev)/(z1-z0)
           if co(i,k+1L) ne -99. and co(i,k) ne -99. then cogrid(i,kk)=co(i,k+1)-zscale*(co(i,k+1L)-co(i,k))
        endif
    endfor
endfor
endfor
noxmix=nogrid+no2grid
;
; working arrays are nogrid, no2grid, cogrid
;
; make fdoy arrays
;
nn=n_elements(nosdate)
nofdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(nosdate(i),0,4))
    imn=long(strmid(nosdate(i),4,2))
    idy=long(strmid(nosdate(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    nofdoy(i)=1.0*iday
endfor
nn=n_elements(cosdate)
cofdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(cosdate(i),0,4))
    imn=long(strmid(cosdate(i),4,2))
    idy=long(strmid(cosdate(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    cofdoy(i)=1.0*iday
endfor
;
; daily averages poleward of 70N
;
ithresh=0.0
    nday=long(max(nofdoy))-long(min(nofdoy))+1L
    nday=365
    nz=n_elements(altitude_save)
    onox=fltarr(nday,nz)
    oco=fltarr(nday,nz)
    num1=lonarr(nday,nz)
    num2=lonarr(nday,nz)
    for iday=1L,nday do begin
        today=where(long(nofdoy) eq iday and nolat gt 0.,nprof)        ; Arctic
        if nprof le 1L then goto,skipday2
        noxday=reform(noxmix(today,*))
        for iprof=0L,nprof-1L do begin
            noxs=reform(noxday(iprof,*))
            if nolat(today(iprof)) ge 70. then begin
               for k=nz-1L,0L,-1L do begin      ; loop from the bottom up
                   if noxs(k) gt ithresh and noxs(k) ne -99. then begin
                      onox(iday-1L,k)=onox(iday-1L,k)+noxs(k)
                      num1(iday-1L,k)=num1(iday-1L,k)+1L
                   endif
               endfor
            endif
        endfor          ; loop over profiles
;
; CO
;
        today=where(long(cofdoy) eq iday and colat gt 0.,nprof)        ; Arctic
        if nprof le 1L then goto,skipday2
        coday=reform(cogrid(today,*))
        for iprof=0L,nprof-1L do begin
            cos0=reform(coday(iprof,*))
            if colat(today(iprof)) ge 70. then begin
               for k=nz-1L,0L,-1L do begin      ; loop from the bottom up
                   if cos0(k) gt ithresh and cos0(k) ne -99. then begin
                      oco(iday-1L,k)=oco(iday-1L,k)+cos0(k)
                      num2(iday-1L,k)=num2(iday-1L,k)+1L
                   endif
               endfor
            endif
        endfor          ; loop over profiles

        skipday2:
    endfor              ; loop over days
;
; daily average
;
    index=where(num1 gt 0L)
    onox(index)=onox(index)/float(num1(index))
    index=where(num2 gt 0L)
    oco(index)=oco(index)/float(num2(index))
save,file='mipas_2004_zt.sav',onox,oco,altitude_save
plotmipas:
;restore,'mipas_2004_zt.sav
nz=n_elements(altitude_save)
;
; interpolate small gaps in time
;
for k=0,nz-1 do begin
    dlev=reform(onox(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 20.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump11
               endif
           endfor
jump11:
        endif
    endfor
    onox(*,k)=dlev

    dlev=reform(oco(*,k))
    for i=1,nday-1 do begin
        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
           for ii=i+1,nday-1 do begin
               naway=float(ii-i)
               if naway le 20.0 and dlev(ii) gt ithresh then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump22
               endif
           endfor
jump22:
        endif
    endfor
    oco(*,k)=dlev

;    dlev=reform(otemp(*,k))
;    for i=1,nday-1 do begin
;        if dlev(i) le ithresh and dlev(i-1) gt ithresh then begin
;           for ii=i+1,nday-1 do begin
;               naway=float(ii-i)
;               if naway le 10.0 and dlev(ii) gt ithresh then begin
;                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
;                  goto,jump4
;               endif
;           endfor
;jump4:
;        endif
;    endfor
;    otemp(*,k)=dlev
endfor
;
; subtract 1 Jan
;
onox=smooth(onox,3,/edge_truncate,/nan)
oco=smooth(oco,3,/edge_truncate,/nan)

dnox=onox
wprof=reform(onox(0,*))
for i=0L,nday-1L do dnox(i,*)=100.*(onox(i,*)-wprof)/onox(i,*)
dtemp=wtemp
;tprof=reform(wtemp(15,*))
;for i=0L,nday-1L do dtemp(i,*)=(wtemp(i,*)-tprof)       ;/wnox(i,*)
;index=where(wtemp eq 0.)
;if index(0) ne -1L then dtemp(index)=0./0.
dco=oco
coprof=reform(oco(15,*))
for i=0L,nday-1L do dco(i,*)=100.*(oco(i,*)-coprof)/oco(i,*)

;
    !type=2^2+2^3
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    nlvls=n_elements(level)
    col1=1L+indgen(nlvls)*mcolor/float(nlvls)
    print,min(fdoy),max(fdoy)
    kday=365.
    leapdy=(long(syear(iyear)) mod 4)
    kday=kday+leapdy
;kday=long(max(fdoy))   ; uncomment to "zoom" in on partial year
kday=91
    xlab=[' ',' ',' ',' ']
;   if iyear eq nyear-1L then xlab=['Jan','Feb','Mar']
index=where(onox eq 0.)
if index(0) ne -1L then onox(index)=0./0.
if index(0) ne -1L then oco(index)=0./0.
ititle=' '
if iyear eq 0L then ititle='MIPAS dNOx'
    contour,dnox,1.+findgen(nday),altitude_save,/noeras,c_color=col1,/cell_fill,levels=level,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title=ititle
index=where(level lt 0)
    contour,dnox,1.+findgen(nday),altitude_save,/noeras,color=mcolor,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=1,c_linestyle=5
index=where(level gt 0)
    contour,dnox,1.+findgen(nday),altitude_save,/noeras,color=0,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=1
    tlevel=180.+5.*findgen(nlvls)
;
    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif
;
; CO
;
    !type=2^2+2^3
    xmn=xorig(5)
    xmx=xorig(5)+xlen
    ymn=yorig(5)
    ymx=yorig(5)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,dco,1.+findgen(nday),altitude_save,/noeras,c_color=col1,/cell_fill,levels=level,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title='dCO'
t2level=-100.+10.*findgen(nlvls)
index=where(level lt 0)
    contour,dco,1.+findgen(nday),altitude_save,/noeras,color=mcolor,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=2
index=where(level gt 0)
    contour,dco,1.+findgen(nday),altitude_save,/noeras,color=0,/follow,levels=level(index),/overplot,$
            c_labels=0*level,thick=2
;
    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif

    !type=2^2+2^3
    xmn=xorig(4)
    xmx=xorig(4)+xlen
    ymn=yorig(4)
    ymx=yorig(4)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,oco,1.+findgen(nday),altitude_save,/noeras,c_color=col1,/cell_fill,levels=colevel,$
         yrange=[30.,110.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=2,ytitle='Altitude',color=0,charthick=2,yminor=1,title='CO'
    contour,oco,1.+findgen(nday),altitude_save,/noeras,color=mcolor,/follow,levels=colevel(0:-1:2),/overplot,$
            c_labels=0*colevel,thick=3,c_charthick=4,c_charsize=2
 
    if iyear eq nyear-1L then begin
       xyouts,15.,20.,'Jan',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31+15.,20.,'Feb',/data,color=0,charsize=2,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,20.,'Mar',/data,color=0,charsize=2,alignment=0.5,charthick=2
    endif

    jumpyear:
endfor  ; loop over years

if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim zt_sdwaccm_nox_nh_spring_7pan+co.ps -rotate -90 zt_sdwaccm_nox_nh_spring_7pan+co.jpg'
endif
end
