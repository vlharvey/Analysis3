;
; zt NH Jan, Feb, Mar
; SABER NO and CO2 in 20009
;
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
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
cbaryoff=0.05
cbarydel=0.05
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
   device,/landscape,bits=8,filename='zt_sdwaccm_no+co2_nh_spring_2009_SABER.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize
endif
erase
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
syear=['2009']	;,'2006','2008','2009','2010','2011','2012']
nyear=n_elements(syear)
;
; loop over years
;
for iyear=0L,nyear-1L do begin
;goto,quick
;
; restore data
;
lc=icolmax
if syear(iyear) eq '2009' then lc=0
spawn,'ls /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.'+syear(iyear)+'-0*.nc_MLS_etal.sav',ifiles
spawn,'ls /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.'+syear(iyear)+'-0*.nc_MLS_etal_v2.sav',ifiles2
nfile=n_elements(ifiles)
nfile=13
icount=0L
for ifile=0L,nfile-1L do begin
    restore,ifiles(ifile)
    restore,ifiles2(ifile)
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
          co2mix=[co2mix,CO2(index,*)]
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
          co2mix=CO2(index,*)              
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
; extract SABER
;
index=where(instrument_all eq 4L)
if index(0) eq -1L then stop,'No SABER data'
date_all=DATE_all(index)
fdoy_all=FDOY_all(index)
latitude_all=LATITUDE_all(index)
longitude_all=LONGITUDE_all(index)
ltime_all=LTIME_all(index)
sctype_all=SCTYPE_all(index)
comix=COmix(index,*)
co2mix=CO2mix(index,*)
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
noxmix=nomix		;+no2mix
;
; daily average NOx in the vortex
;
ithresh=0.0
    nday=long(max(fdoy))-long(min(fdoy))+1L
    nday=365
    nz=n_elements(lev)
    date_array=lonarr(nday)
    onox=fltarr(nday,nz)
    oco=fltarr(nday,nz)
    oco2=fltarr(nday,nz)
    otemp=fltarr(nday,nz)
    oo3=fltarr(nday,nz)
    altitude=fltarr(nday,nz)
    num=lonarr(nday,nz)
    for iday=1L,nday do begin
        today=where(long(fdoy) eq iday and latitude gt 0.,nprof)	; Arctic
        if nprof le 1L then goto,skipday
        date_array(iday-1)=date_all(today(0))
        noxday=reform(noxmix(today,*))
        coday=reform(comix(today,*))
        co2day=reform(co2mix(today,*))
        tempday=reform(temp(today,*))
        o3day=reform(o3mix(today,*))
        altday=reform(altitude_save(today,*))
        for iprof=0L,nprof-1L do begin
            noxs=reform(noxday(iprof,*))
            cos0=reform(coday(iprof,*))
            co2s0=reform(co2day(iprof,*))
            tp0=reform(tempday(iprof,*))
            o30=reform(o3day(iprof,*))
            zprof=reform(altday(iprof,*))
;           if latitude(today(iprof)) ge 65. and latitude(today(iprof)) le 75. and longitude(today(iprof)) gt 200. and longitude(today(iprof)) lt 220. then begin
            if latitude(today(iprof)) ge 65. and latitude(today(iprof)) le 70. and longitude(today(iprof)) gt 212.5-10. and longitude(today(iprof)) lt 212.5+10. then begin
;           if latitude(today(iprof)) ge 70. then begin
               for k=nz-1L,0L,-1L do begin	; loop from the bottom up
                   if noxs(k) gt ithresh and co2s0(k) ne -99. and num(iday-1L,k) eq 0L then begin
                      onox(iday-1L,k)=onox(iday-1L,k)+noxs(k)
                      oco(iday-1L,k)=oco(iday-1L,k)+cos0(k)
                      oco2(iday-1L,k)=oco2(iday-1L,k)+co2s0(k)
                      otemp(iday-1L,k)=otemp(iday-1L,k)+tp0(k)
                      oo3(iday-1L,k)=oo3(iday-1L,k)+o30(k)
                      altitude(iday-1L,k)=altitude(iday-1L,k)+zprof(k)
                      num(iday-1L,k)=num(iday-1L,k)+1L
print,date_array(iday-1L),long(fdoy(today(iprof))),latitude(today(iprof)),longitude(today(iprof))
                   endif
               endfor
            endif
            jumpprof:
        endfor		; loop over profiles
        skipday:
    endfor		; loop over days
;
; daily average
;
    index=where(num gt 0L)
    if index(0) eq -1L then goto,jumpyear
    onox(index)=1.e6*onox(index)/float(num(index))
    oco(index)=1.e6*oco(index)/float(num(index))
    oco2(index)=1.e6*oco2(index)/float(num(index))
    otemp(index)=otemp(index)/float(num(index))
    oo3(index)=1.e6*oo3(index)/float(num(index))
    altitude(index)=altitude(index)/float(num(index))
;
; save
;
save,file='saber_no+co2_1pf_2009.sav',altitude,onox,oco2,date_array
quick:
restore,'saber_no+co2_1pf_2009.sav
;
; plot zt
;
wnox=onox
wco2=oco2
waltitude=altitude
;
    !type=2^2+2^3
    xmn=xorig(iyear)
    xmx=xorig(iyear)+xlen
    ymn=yorig(iyear)
    ymx=yorig(iyear)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    level=[10.,20.,30.,40.,50.,70.,100.,150.,200.,$
           500.,1000.,2000.,5000.,10000.,20000.,50000.,100000.,2.e5,5.e5,1.e6,5.e6]/10000.
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
if index(0) ne -1L then wco2(index)=0./0.
ititle=' '
if iyear eq 0L then ititle='SD-WACCM at SABER @ PFRR'
    contour,wnox,1.+findgen(nday),waltitude,/noeras,c_color=col1,/cell_fill,levels=level,$
         yrange=[70.,120.],yticks=4,xrange=[1.,kday],xticks=3L,xtickname=xlab,$
         charsize=3,ytitle='Altitude',color=0,min_value=0.,charthick=2,yminor=1,title=ititle
    contour,wnox,1.+findgen(nday),waltitude,/noeras,color=0,/follow,levels=level,/overplot,$
            c_labels=0*level,min_value=0.
    colevel=[50,100,150,200,250,300,325,350,375]
loadct,0
    contour,wco2,1.+findgen(nday),waltitude,/noeras,color=200,/follow,levels=colevel,/overplot,$
            c_labels=1L+0*colevel,thick=5,c_charthick=4,c_charsize=2
   xyouts,75.,72.,syear(iyear),/data,color=200,charsize=5,charthick=10
loadct,39
;
    if iyear eq nyear-1L then begin
       xyouts,15.,65.,'Jan',/data,color=0,charsize=3,alignment=0.5,charthick=2
       xyouts,31+15.,65.,'Feb',/data,color=0,charsize=3,alignment=0.5,charthick=2
       xyouts,31.+28.+15.,65.,'Mar',/data,color=0,charsize=3,alignment=0.5,charthick=2
    endif
    jumpyear:
endfor  ; loop over years
!type=2^2+2^3+2^5
xmnb=xorig(0)+xlen+cbaryoff
xmxb=xmnb+cbarydel
ymnb=min(yorig)
ymxb=max(yorig)+ylen
set_viewport,xmnb,xmxb,ymnb,ymxb
slab=' '+strarr(n_elements(level))
plot,[0,0],[min(level),max(level)],xrange=[0,10],color=0,$
     yticks=n_elements(level)-1L,ytickname=slab,$
     yrange=[min(level),max(level)],charsize=2,title='NO (ppmv)',charthick=2
xbox=[0,10,10,0,0]
y1=min(level)
dy=(max(level)-min(level))/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor
slab=strcompress(string(format='(f7.3)',level),/remove_all)
y1=min(level)+dy/2
for i=0L,n_elements(slab)-1L do begin
    slab0=slab(i)
    flab0=float(slab(i))
    if flab0 lt 0.01 then begin
       slab0=strcompress(string(format='(f5.3)',flab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=1.4,/data,color=mcolor,charthick=2
    endif
    if flab0 lt 1. and flab0 ge 0.01 then begin
       slab0=strcompress(string(format='(f4.2)',flab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
    endif
    if flab0 ge 1. then begin
       slab0=strcompress(long(slab0),/remove_all)
       xyouts,xorig(0)+xlen+0.02,y1,slab0,charsize=2,/data,color=0,charthick=2
    endif
    y1=y1+dy
endfor

if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim zt_sdwaccm_no+co2_nh_spring_2009_SABER.ps -rotate -90 zt_sdwaccm_no+co2_nh_spring_2009_SABER.jpg'
endif
end
