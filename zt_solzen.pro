;
; plot time-altitude section of solar zenith angle
;
@stddat
@kgmt
@ckday
@kdate

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
xorig=[0.1]
yorig=[0.2]
xlen=0.8
ylen=0.6
cbaryoff=0.01
cbarydel=0.03
!NOERAS=-1
!p.font=1
!x.ticklen=-0.05
lstmn=1
lstdy=1
lstyr=2007
ledmn=4
leddy=1
ledyr=2007
lstday=0
ledday=0
syear=['2004']  ;,'2006','2008','2009','2010','2011','2012']
nyear=n_elements(syear)
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
   !p.background=mcolor
endif
;
; loop over years
;
for iyear=0L,nyear-1L do begin

if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
;  !p.font=0
   !p.charthick=3
   device,font_size=8
   device,/landscape,bits=8,filename='zt_solzen_'+syear(iyear)+'.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize
endif
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
longitude=longitude_all
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
wnox=smooth(onox,3,/edge_truncate,/nan)
wco=smooth(oco,3,/edge_truncate,/nan)
waltitude=smooth(altitude,3,/edge_truncate,/nan)
wtemp=smooth(otemp,3,/edge_truncate,/nan)
wo3=smooth(oo3,3,/edge_truncate,/nan)
;
; solar zenith angle
;
      pi=3.14159265
      dtor=pi/180.
      earinc=23.5
      zangle=fltarr(n_elements(latitude))
      for ii=0L,n_elements(latitude)-1 do begin
          rlat=latitude(ii)
          rlon=longitude(ii)
          gmt=12.	;time(ii)
          sinlat=sin(rlat*dtor)
          coslat=sqrt(1.-sinlat^2.)
          sinlon=sin(rlon*dtor)
          coslon=cos(rlon*dtor)
          soya=(fdoy(ii)-81.25)*pi/182.5           ; day angle
          soha=2.*pi*(gmt-12.)/24.            ; hour angle
          soha=-soha
          sininc=sin(earinc*dtor)
          sindec=sininc*sin(soya)
          cosdec= sqrt(1.-sindec^2.)
          coszen=cos(soha)*coslon+sin(soha)*sinlon
          coszen=coszen*cosdec*coslat
          coszen=sindec*sinlat+coszen
          coszen=min([max([coszen,-1.]),1.])
          chi = acos(coszen)
          zangle(ii) = chi/dtor
      endfor
set_viewport,.15,.9,.3,.8
!type=2^2+2^3
erase
plot,fdoy,zangle,color=0,psym=1,xrange=[0,90],/noeras,/nodata,charsize=2,xtitle='DOY',ytitle='Noon Zenith Angle >70N',yrange=[80,100]
index=where(latitude ge 70.)
oplot,fdoy(index),zangle(index),psym=1,color=0
plots,0,90
plots,90,90,/continue,color=0
stop
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

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim zt_solzen_'+syear(iyear)+'.ps -rotate -90 zt_solzen_'+syear(iyear)+'.jpg'
endif
    jumpyear:
endfor  ; loop over years

end
