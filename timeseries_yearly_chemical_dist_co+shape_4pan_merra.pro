;
; MERRA
; timeseries of chemical distinction d and vortex shape diagnostics
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
xorig=[0.15,0.15,0.15,0.15]
yorig=[0.86,0.64,0.42,0.2]-0.13
cbaryoff=0.07
cbarydel=0.01
xlen=0.6
ylen=.2
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dum=1979+indgen(36)	; 1979-2014
syear=strcompress(dum,/remove_all)     ; 1979-2014
nyear=n_elements(syear)
ssw=0*indgen(nyear)
index=where(syear eq '1984' or syear eq '1986' or syear eq '2005' or syear eq '2008' or syear eq '2012')	; fall of major SSW winter
ssw(index)=1
ssw=1+0*indgen(nyear)
ssw(0)=0
smon=['09','10','11','12','01','02','03','04']
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
for iyear=0L,nyear-2L do begin
    icount=0
for imon=0L,nmon-1L do begin

    dum=findfile('vortex_shape_merra_'+syear(iyear)+smon(imon)+'.sav')
    if imon ge 4L then dum=findfile('vortex_shape_merra_'+syear(iyear+1)+smon(imon)+'.sav')
;print,dum
    if dum(0) eq '' then goto,skipmon
    restore,dum(0)
;    print,dum(0)
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
          rth=1000.
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
             device,/landscape,bits=8,filename='timeseries_shape_merra_'+sth+'.ps'
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

;help,sdate_tot
skipmon:
endfor  ; loop over months

if iyear eq 0L then begin
   ztd_allyears=fltarr(243,nyear)
   area_allyears=fltarr(243,nyear)
   ellip_allyears=fltarr(243,nyear)
   y0_allyears=fltarr(243,nyear)
   x0_allyears=fltarr(243,nyear)
   nvort_allyears=fltarr(243,nyear)
   sdate_allyears=strarr(243,nyear)
endif

index=where(area1_all lt 0.)
if index(0) ne -1L then area1_all(index)=0./0.
area1_all=smooth(area1_all,3,/Nan)
area1_all=smooth(area1_all,3,/Nan)
area_allyears(0:n_elements(sdate_tot)-1,iyear)=area1_all(*,ith)		; area goes with theta
sdate_allyears(0:n_elements(sdate_tot)-1,iyear)=sdate_tot
x0_all=smooth(centroid_longitude1_all,3,/Nan)
x0_all=smooth(x0_all,3,/Nan)
x0_allyears(0:n_elements(sdate_tot)-1,iyear)=x0_all(*,ith)         ; area goes with theta
y0_all=smooth(centroid_latitude1_all,3,/Nan)
y0_all=smooth(y0_all,3,/Nan)
y0_allyears(0:n_elements(sdate_tot)-1,iyear)=y0_all(*,ith)         ; area goes with theta
nvort_all=number_vortex_lobes1_all	;smooth(number_vortex_lobes1_all,3,/Nan)
;nvort_all=smooth(nvort_all,3,/Nan)
nvort_allyears(0:n_elements(sdate_tot)-1,iyear)=nvort_all(*,ith)         ; area goes with theta
ellip_all=smooth(ellipticity1_all,3,/Nan)
ellip_all=smooth(ellip_all,3,/Nan)
ellip_allyears(0:n_elements(sdate_tot)-1,iyear)=ellip_all(*,ith)         ; area goes with theta

ztd_all=nvort_all
index=where(ztd_all lt 0.)      ;gt 0.33)
if index(0) ne -1L then ztd_all(index)=0./0.
ztd_all=smooth(ztd_all,3,/Nan)
ztd_allyears(0:n_elements(sdate_tot)-1,iyear)=ztd_all(*,ith)		; d goes against theta
;
; restore polar MLS CO
;
;restore,'/Users/harvey/Analysis/Save_files/zt_mls_temp+co_'+syear(iyear)+'-'+syear(iyear+1)+'.sav'
;oco1=MLSPOLARCO_ZT
;result=size(oco1)
;fdoymls=1.+findgen(result(1))

endfor	; loop over years
;
; calculate means and sigmas for non ssw years
;
sdate_tot=reform(sdate_allyears(*,0))
nn=n_elements(sdate_tot)
arrmean=fltarr(nn)
arrsig=fltarr(nn)
for i=0,nn-1L do begin
    dum=reform(ztd_allyears(i,*))
    index=where(dum ne 0.)
    arrmean(i)=mean(dum(index))
    if n_elements(index) gt 1 then arrsig(i)=stdev(dum(index))
endfor
arrmean=smooth(arrmean,3,/nan)
arrsig=smooth(arrsig,3,/nan)
;
; plot all years
;
kcount=0
for iyear=0L,nyear-2L do begin
    sdate_tot=reform(sdate_allyears(*,iyear))
    arr1=reform(ztd_allyears(*,iyear))
index=where(arr1 eq 0.)
if index(0) ne -1L then arr1(index)=0./0.
;
; remove missing days
;
index=where(sdate_tot ne '',nn)
sdate_tot=sdate_tot(index)
arr1=arr1(index)

fdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(sdate_tot(i),0,4))
    imn=long(strmid(sdate_tot(i),4,2))
    idy=long(strmid(sdate_tot(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    fdoy(i)=1.0*iday
endfor
startday=244.	; Sep 1
startday=274.	; Oct 1
startday=305.	; Nov 1
index=where(fdoy lt 180.)
maxf=max(fdoy)
if index(0) ne -1L then fdoy(index)=fdoy(index)+maxf
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
if iyear eq 0L then begin
   if ssw(iyear) eq 0 then begin
      loadct,0
      plot,fdoy,arrmean,yrange=[1.,3],color=0,charsize=2,charthick=2,ytitle='# Lobes',xrange=[startday-1,max(fdoy)-30],xticks=n_elements(smonth)-1,xtickname=smonth,thick=5
      for i=0L,nn-1L do begin
          if fdoy(i) ge startday and fdoy(i) lt max(fdoy)-30 then begin
          plots,fdoy(i),1.	;arrmean(i)-arrsig(i)
          plots,fdoy(i),arrmean(i)+arrsig(i),/continue,color=150,thick=8
          endif
      endfor
      oplot,fdoy,arrmean,color=0,thick=5
      oplot,fdoy,arrmean+arrsig,color=0,thick=1
;     oplot,fdoy,arrmean-arrsig,color=0,thick=1
      loadct,39
   endif
day0=startday
;for i=0L,n_elements(smonth)-1 do begin
;    plots,day0,500
;    plots,day0,300,/continue,color=0
;    day0=day0+kmon(i)
;;print,day0
;endfor
endif
;if ssw(iyear) eq 0 then oplot,fdoy,arr1,thick=2,color=0	;mcolor*(iyear/float(nyear-1.5))
if ssw(iyear) eq 1 then begin
   index=where(arr1 gt 1.)
   if index(0) ne -1L then oplot,fdoy(index),arr1(index),psym=8,color=mcolor*(iyear/float(nyear+1.)),symsize=1.5
;  xyouts,xmn+0.02,ymx-0.03-0.03*kcount,syear(iyear)+'-'+syear(iyear+1),charsize=2,charthick=2,color=mcolor*(iyear/float(nyear-2)),/normal
   kcount=kcount+1
endif
endfor
;xyouts,xmx-0.15,ymx-0.04,sth,/normal,charsize=2,charthick=2,color=0
;
; calculate means and sigmas for non ssw years
;
sdate_tot=reform(sdate_allyears(*,0))
nn=n_elements(sdate_tot)
arrmean=fltarr(nn)
arrsig=fltarr(nn)
for i=0,nn-1L do begin
    dum=reform(area_allyears(i,*))
    index=where(dum ne 0.)
    arrmean(i)=mean(dum(index))
    if n_elements(index) gt 1 then arrsig(i)=stdev(dum(index))
endfor
arrmean=smooth(arrmean,3,/nan)
arrsig=smooth(arrsig,3,/nan)

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
;
; plot all years
;
kcount=0
for iyear=0L,nyear-2L do begin
    sdate_tot=reform(sdate_allyears(*,iyear))
    arr1=reform(area_allyears(*,iyear))
index=where(arr1 eq 0.)
if index(0) ne -1L then arr1(index)=0./0.
;
; remove missing days
;
index=where(sdate_tot ne '',nn)
sdate_tot=sdate_tot(index)
arr1=arr1(index)

fdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(sdate_tot(i),0,4))
    imn=long(strmid(sdate_tot(i),4,2))
    idy=long(strmid(sdate_tot(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    fdoy(i)=1.0*iday
endfor
startday=244.   ; Sep 1
startday=274.   ; Oct 1
startday=305.   ; Nov 1
index=where(fdoy lt 180.)
maxf=max(fdoy)
if index(0) ne -1L then fdoy(index)=fdoy(index)+maxf
if iyear eq 0L then begin
   if ssw(iyear) eq 0 then begin
      loadct,0
      plot,fdoy,arrmean,yrange=[0,40],color=0,charsize=2,charthick=2,ytitle='Area (%)',xrange=[startday-1,max(fdoy)-30],xticks=n_elements(smonth)-1,xtickname=[' ',' ',' ',' ',' ',' '],thick=5,/noeras
      for i=0L,nn-1L do begin
          if fdoy(i) ge startday and fdoy(i) lt max(fdoy)-30 then begin
          plots,fdoy(i),arrmean(i)-arrsig(i)
          plots,fdoy(i),arrmean(i)+arrsig(i),/continue,color=150,thick=8
          endif
      endfor
      oplot,fdoy,arrmean,color=0,thick=5
      oplot,fdoy,arrmean+arrsig,color=0,thick=1
      oplot,fdoy,arrmean-arrsig,color=0,thick=1
      loadct,39
   endif
day0=startday
for i=0L,n_elements(smonth)-1 do begin
    plots,day0,500
    plots,day0,300,/continue,color=0
    day0=day0+kmon(i)
;print,day0
endfor
endif
;if ssw(iyear) eq 0 then oplot,fdoy,arr1,thick=2,color=0        ;mcolor*(iyear/float(nyear-1.5))
if ssw(iyear) eq 1 then begin
   oplot,fdoy,arr1,thick=5,color=mcolor*(iyear/float(nyear+1.))
;  xyouts,xmn+0.02,ymx-0.03-0.03*kcount,syear(iyear)+'-'+syear(iyear+1),charsize=2,charthick=2,color=mcolor*(iyear/float(nyear-2)),/normal
   kcount=kcount+1
endif
endfor
xyouts,xmn+0.01,ymx-0.03,sth,/normal,charsize=2,charthick=2,color=0


nn=n_elements(sdate_tot)
arrmean=fltarr(nn)
arrsig=fltarr(nn)
for i=0,nn-1L do begin
    dum=reform(y0_allyears(i,*))
    index=where(dum ne 0.)
    arrmean(i)=mean(dum(index))
    if n_elements(index) gt 1 then arrsig(i)=stdev(dum(index))
endfor
arrmean=smooth(arrmean,3,/nan)
arrsig=smooth(arrsig,3,/nan)

xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
;
; plot all years
;
kcount=0
for iyear=0L,nyear-2L do begin
    sdate_tot=reform(sdate_allyears(*,iyear))
    arr1=reform(y0_allyears(*,iyear))
index=where(arr1 eq 0.)
if index(0) ne -1L then arr1(index)=0./0.
;
; remove missing days
;
index=where(sdate_tot ne '',nn)
sdate_tot=sdate_tot(index)
arr1=arr1(index)

fdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(sdate_tot(i),0,4))
    imn=long(strmid(sdate_tot(i),4,2))
    idy=long(strmid(sdate_tot(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    fdoy(i)=1.0*iday
endfor
startday=244.   ; Sep 1
startday=274.   ; Oct 1
startday=305.   ; Nov 1
index=where(fdoy lt 180.)
maxf=max(fdoy)
if index(0) ne -1L then fdoy(index)=fdoy(index)+maxf
if iyear eq 0L then begin
   if ssw(iyear) eq 0 then begin
      loadct,0
      plot,fdoy,arrmean,yrange=[40,90],color=0,charsize=2,charthick=2,ytitle='Centroid Latitude',xrange=[startday-1,max(fdoy)-30],xticks=n_elements(smonth)-1,xtickname=[' ',' ',' ',' ',' ',' '],thick=5,/noeras
      for i=0L,nn-1L do begin
          if fdoy(i) ge startday and fdoy(i) lt max(fdoy)-30 then begin
          plots,fdoy(i),arrmean(i)-arrsig(i)
          plots,fdoy(i),arrmean(i)+arrsig(i),/continue,color=150,thick=8
          endif
      endfor
      oplot,fdoy,arrmean,color=0,thick=5
      oplot,fdoy,arrmean+arrsig,color=0,thick=1
      oplot,fdoy,arrmean-arrsig,color=0,thick=1
      loadct,39
   endif
day0=startday
for i=0L,n_elements(smonth)-1 do begin
    plots,day0,500
    plots,day0,300,/continue,color=0
    day0=day0+kmon(i)
;print,day0
endfor
endif
;if ssw(iyear) eq 0 then oplot,fdoy,arr1,thick=2,color=0        ;mcolor*(iyear/float(nyear-1.5))
if ssw(iyear) eq 1 then begin
   oplot,fdoy,arr1,thick=5,color=mcolor*(iyear/float(nyear+1.))
   xyouts,xmx+0.02,ymx+0.1-0.03*kcount,syear(iyear)+'-'+syear(iyear+1),charsize=2,charthick=2,color=mcolor*(iyear/float(nyear-2)),/normal
   kcount=kcount+1
endif
endfor



nn=n_elements(sdate_tot)
arrmean=fltarr(nn)
arrsig=fltarr(nn)
for i=0,nn-1L do begin
    dum=reform(ellip_allyears(i,*))
    index=where(dum ne 0.)
    arrmean(i)=mean(dum(index))
    if n_elements(index) gt 1 then arrsig(i)=stdev(dum(index))
endfor
arrmean=smooth(arrmean,3,/nan)
arrsig=smooth(arrsig,3,/nan)

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
;
; plot all years
;
kcount=0
for iyear=0L,nyear-2L do begin
    sdate_tot=reform(sdate_allyears(*,iyear))
    arr1=reform(ellip_allyears(*,iyear))
index=where(arr1 eq 0.)
if index(0) ne -1L then arr1(index)=0./0.
;
; remove missing days
;
index=where(sdate_tot ne '',nn)
sdate_tot=sdate_tot(index)
arr1=arr1(index)

fdoy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(sdate_tot(i),0,4))
    imn=long(strmid(sdate_tot(i),4,2))
    idy=long(strmid(sdate_tot(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    fdoy(i)=1.0*iday
endfor
startday=244.   ; Sep 1
startday=274.   ; Oct 1
startday=305.   ; Nov 1
index=where(fdoy lt 180.)
maxf=max(fdoy)
if index(0) ne -1L then fdoy(index)=fdoy(index)+maxf
if iyear eq 0L then begin
   if ssw(iyear) eq 0 then begin
      loadct,0
      plot,fdoy,arrmean,yrange=[0.,1],color=0,charsize=2,charthick=2,ytitle='Ellipticity',xrange=[startday-1,max(fdoy)-30],xticks=n_elements(smonth)-1,xtickname=[' ',' ',' ',' ',' ',' '],thick=5,/noeras
      for i=0L,nn-1L do begin
          if fdoy(i) ge startday and fdoy(i) lt max(fdoy)-30 then begin
          plots,fdoy(i),arrmean(i)-arrsig(i)
          plots,fdoy(i),arrmean(i)+arrsig(i),/continue,color=150,thick=8
          endif
      endfor
      oplot,fdoy,arrmean,color=0,thick=5
      oplot,fdoy,arrmean+arrsig,color=0,thick=1
      oplot,fdoy,arrmean-arrsig,color=0,thick=1
      loadct,39
   endif
day0=startday
for i=0L,n_elements(smonth)-1 do begin
    plots,day0,500
    plots,day0,300,/continue,color=0
    day0=day0+kmon(i)
;print,day0
endfor
endif
;if ssw(iyear) eq 0 then oplot,fdoy,arr1,thick=2,color=0        ;mcolor*(iyear/float(nyear-1.5))
if ssw(iyear) eq 1 then begin
   oplot,fdoy,arr1,thick=5,color=mcolor*(iyear/float(nyear+1.))
;  xyouts,xmn+0.02,ymx-0.03-0.03*kcount,syear(iyear)+'-'+syear(iyear+1),charsize=2,charthick=2,color=mcolor*(iyear/float(nyear-2)),/normal
   kcount=kcount+1
endif
endfor


if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device,/close
   spawn,'convert -trim timeseries_shape_merra_'+sth+'.ps -rotate -90 timeseries_shape_merra_'+sth+'.jpg'
endif

end
