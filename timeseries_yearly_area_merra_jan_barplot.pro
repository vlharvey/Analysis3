;
; MERRA
; timeseries of vortex area
; January barplot as in Burnett
;
@stddat
@kgmt
@ckday
@kdate

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.25]
yorig=[0.3]
cbaryoff=0.07
cbarydel=0.01
xlen=0.6
ylen=.5
device,decompose=0
mcolor=byte(!p.color)
nlvls=20
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dum=1979+indgen(36)	; 1979-2014
syear=strcompress(dum,/remove_all)     ; 1979-2014
lyear=long(syear)
nyear=n_elements(syear)
nlvls=nyear
ssw=0*indgen(nyear)
index=where(syear eq '1985' or syear eq '1987' or syear eq '2004' or syear eq '2006' or syear eq '2009' or syear eq '2013')	; major SSW 
ssw(index)=1
smon=['09','10','11','12','01','02','03','04']
smon=['01']
febflux=[204,200,205,214,123,141,74,84,72,105,222,178,243,232,143,100,86,72,74,93,143,173,147,205,125,107,97,77,78,71,70,85,95,107,104,170]
kmon=[30,31,30,31,31,28,31,30]
smonth='     '+['Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr',' ']
smonth='     '+['Oct','Nov','Dec','Jan','Feb','Mar',' ']
smonth='          '+['Nov','Dec','Jan','Feb','Mar',' ']
nmon=n_elements(smon)
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
;
; loop over years
;
for iyear=0L,nyear-1L do begin
for imon=0,0 do begin	;0L,nmon-1L do begin ; January
    icount=0
    dum=findfile('vortex_shape_merra_'+syear(iyear)+smon(imon)+'.sav')
    if dum(0) eq '' then goto,skipmon
    restore,dum(0)
    print,dum(0)
    if icount ne 0L then begin
       area1_all=[area1_all,area1]
       centroid_longitude1_all=[centroid_longitude1_all,centroid_longitude1]
       centroid_latitude1_all=[centroid_latitude1_all,centroid_latitude1]
       number_vortex_lobes1_all=[number_vortex_lobes1_all,number_vortex_lobes1]
       ellipticity1_all=[ellipticity1_all,ellipticity1]
       altitude_all=[altitude_all,altitude]
       sdate_tot=[sdate_tot,sdate_all]
    endif
    if icount eq 0L then begin
       if iyear eq 0L then begin
          print,th
          rth=1400.
;         rth=1000.
;read,'Enter desired theta ',rth
          index=where(th eq rth)
          ith=index(0)
          sth=strcompress(long(th(ith)),/remove_all)+'K'
          erase
          if setplot eq 'ps' then begin
             set_plot,'ps'
             xsize=nxdim/100.
             ysize=nydim/100.
             !psym=0
             !p.font=0
             device,font_size=9
             device,/landscape,bits=8,filename='barplot_area_merra_'+sth+'_jan.ps'
             device,/color
             device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                    xsize=xsize,ysize=ysize
             !p.charsize=2.0
          endif
       endif

       area1_all=area1
       centroid_longitude1_all=centroid_longitude1
       centroid_latitude1_all=centroid_latitude1
       number_vortex_lobes1_all=number_vortex_lobes1
       ellipticity1_all=ellipticity1
       altitude_all=altitude
       sdate_tot=sdate_all
       icount=1L
    endif

skipmon:
endfor  ; loop over months

if iyear eq 0L then begin
   ztd_allyears=fltarr(nyear)
   area_allyears=fltarr(nyear)
   areasig_allyears=fltarr(nyear)
   ellip_allyears=fltarr(nyear)
   y0_allyears=fltarr(nyear)
   x0_allyears=fltarr(nyear)
   nvort_allyears=fltarr(nyear)
endif
index=where(area1_all lt 0.)
if index(0) ne -1L then area1_all(index)=0./0.
area1_all=smooth(area1_all,3,/Nan)
area_allyears(iyear)=mean(area1_all(*,ith),/nan)		; area goes with theta
tmp=reform(area1_all(*,ith))
index=where(finite(tmp) eq 1)
areasig_allyears(iyear)=stdev(tmp(index))
x0_all=smooth(centroid_longitude1_all,3,/Nan)
x0_all=smooth(x0_all,3,/Nan)
x0_allyears(iyear)=mean(x0_all(*,ith))         ; area goes with theta
y0_all=smooth(centroid_latitude1_all,3,/Nan)
y0_all=smooth(y0_all,3,/Nan)
y0_allyears(iyear)=mean(y0_all(*,ith))         ; area goes with theta
nvort_all=number_vortex_lobes1_all	;smooth(number_vortex_lobes1_all,3,/Nan)
;nvort_all=smooth(nvort_all,3,/Nan)
nvort_allyears(iyear)=mean(nvort_all(*,ith))         ; area goes with theta
ellip_all=smooth(ellipticity1_all,3,/Nan)
ellip_all=smooth(ellip_all,3,/Nan)
ellip_allyears(iyear)=mean(ellip_all(*,ith))         ; area goes with theta

ztd_all=nvort_all
index=where(ztd_all lt 0.)      ;gt 0.33)
if index(0) ne -1L then ztd_all(index)=0./0.
ztd_all=smooth(ztd_all,3,/Nan)
ztd_allyears(iyear)=mean(ztd_all(*,ith))		; d goes against theta

endfor	; loop over years
;index=where(lyear lt max(lyear),nyear)
;lyear=lyear(index)
;syear=syear(index)
;area_allyears=area_allyears(index)
;
; calculate means and sigmas for non ssw years
;
arrmean=median(area_allyears)
arrsig=stdev(area_allyears)

xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
;
; plot all years
;
;b = BARPLOT(area_allyears-arrmean, NBARS=nyear, FILL_COLOR=0,xtickname=syear)

plot,lyear,area_allyears,yrange=[0,40],color=0,charsize=2,charthick=2,ytitle=sth+' Vortex Area (%)',xrange=[min(lyear),max(lyear)],psym=8,/noeras,symsize=1.5
oplot,lyear,area_allyears+areasig_allyears,psym=1,color=0
oplot,lyear,area_allyears-areasig_allyears,psym=1,color=0
for n=0,nyear-1 do begin
    plots,lyear(n),area_allyears(n)+areasig_allyears(n)
    plots,lyear(n),area_allyears(n)-areasig_allyears(n),/continue,color=0,thick=2
    if ssw(n) eq 1 then oplot,[lyear(n),lyear(n)],[area_allyears(n),area_allyears(n)],color=250,psym=8
endfor

;oplot,lyear,arrmean+0*lyear,color=0,thick=3
;oplot,lyear,arrmean+arrsig+0*lyear,color=0,thick=1
;oplot,lyear,arrmean-arrsig+0*lyear,color=0,thick=1
xyouts,xmn+0.01,ymx-0.03,'January',/normal,charsize=2,charthick=2,color=0
;
; solar cycle?
;
axis,yaxis=1,/save,yrange=[50,300],color=250,ytitle='Solar 10.7 cm Radio Flux',charsize=2,charthick=2
oplot,lyear,febflux,color=250,thick=3
result=correlate(febflux,area_allyears)
stemp=strcompress(result(0),/remove_all)
xyouts,xmx-0.15,ymx-0.03,'r='+strmid(stemp,0,5),charsize=2,charthick=2,color=0,/normal

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device,/close
   spawn,'convert -trim barplot_area_merra_'+sth+'_jan.ps -rotate -90 barplot_area_merra_'+sth+'_jan.jpg'
endif

end
