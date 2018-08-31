;
; cloud radius
; plot all latbins in one panel
;
; plot relative to days from solstice
;
; read one CIPS IDL save file of all days of data binned every 5 degrees latitude
; timeseries of frequency in 4 seasons at latitudes 50-85 by 5 degrees in each hem
;
; color table and symbol, set path
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=750
nydim=750
xorig=[0.15,0.15]
yorig=[0.55,0.15]
xlen=0.7
ylen=0.35
!noeras=1
setplot='x'
read,'setplot=',setplot
if setplot ne 'ps' then begin
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
!p.background=mcolor
pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_4_orbit_'
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
;
; latbins
;
nlat=35
latbin=-85.+5.*findgen(nlat)    ; -85 to 85
;
; save file contents:
; CLOUD_ALB_ALL   FLOAT     = Array[678, 35]
; CLOUD_FREQ_AVG_ALL FLOAT     = Array[678, 35]
; CLOUD_FREQ_MAX_ALL FLOAT     = Array[678, 35]
; CLOUD_FREQ_SIGMA_ALL FLOAT     = Array[678, 35]
; CLOUD_IWC_ALL   FLOAT     = Array[678, 35]
; CLOUD_RAD_ALL   FLOAT     = Array[678, 35]
; FDOY_ALL        FLOAT     = Array[678]
; LATBIN          FLOAT     = Array[35]
; SDATE_ALL       STRING    = Array[678]
;
restore,'cloud_rad_alb_iwc_latbin_all.sav
CLOUD_FREQ_AVG_ALL=CLOUD_rad_ALL
CLOUD_FREQ_MAX_ALL=CLOUD_rad_ALL
CLOUD_FREQ_SIGMA_ALL=CLOUD_rad_ALL
;
; ps file 
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='timeseries_cipsl4_cloudrad_all_lat.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif
    erase
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
;
; loop over lat bins
; 
ymin=0.
ymin=40.
ymax=70.
for ilat=0L,nlat/2-1L do begin
    index=where(latbin eq abs(latbin(ilat)))
    ilat2=index(0)
if abs(latbin(ilat2)) lt 60. then goto,skiplatnh
if abs(latbin(ilat2)) eq 85. then icol=mcolor*.9
if abs(latbin(ilat2)) eq 80. then icol=mcolor*.8
if abs(latbin(ilat2)) eq 75. then icol=mcolor*.7
if abs(latbin(ilat2)) eq 70. then icol=mcolor*.6
if abs(latbin(ilat2)) eq 65. then icol=mcolor*.3
if abs(latbin(ilat2)) eq 60. then icol=mcolor*.1
    slat=string(format='(i2)',abs(latbin(ilat2))) 
;
; avg frequency and sigma at this NH latitude
;
;   cloud_freq_avg_lat2=smooth(reform(cloud_freq_avg_all(*,ilat2)),3)       ; fltarr(kday) at ilat2
    cloud_freq_avg_lat2=reform(cloud_freq_avg_all(*,ilat2))
    cloud_freq_max_lat2=reform(cloud_freq_max_all(*,ilat2))       ; fltarr(kday) at ilat2
    cloud_freq_sigma_lat2=smooth(reform(cloud_freq_sigma_all(*,ilat2)),3)
    if ilat eq 0L then begin
    imin=-40.
    imax=80.
    ndoy=imax-imin
    xlab=['-40','-20','0','20','40','60','80']
    nxticks=n_elements(xtickname)
    plot,findgen(ndoy),cloud_freq_max_lat2,ytitle='Cloud Radius',xrange=[imin,imax],$
         xticks=nxticks,xtickname=xlab,xtickv=float(xlab),charsize=1.5,color=0,/nodata,$
         title='CIPS Level 4 V3.20',yrange=[ymin,ymax]
    endif
    syear=strmid(strcompress(sdate_all,/remove_all),0,4)
    nhsol=172.
    index=where(syear eq '2007' and cloud_freq_avg_lat2 ge 0.1 and fdoy_all ge nhsol-40 and fdoy_all lt nhsol+80.)
    if index(0) ne -1L then oplot,fdoy_all(index)-nhsol,cloud_freq_avg_lat2(index),color=icol,psym=8
;   if index(0) ne -1L then oplot,fdoy_all(index)-nhsol,cloud_freq_avg_lat2(index),color=icol,psym=0,thick=3
    index2=where(syear eq '2008' and fdoy_all lt nhsol+80 and cloud_freq_avg_lat2 ge 0.1)
    if index2(0) ne -1L then oplot,fdoy_all(index2)-nhsol,cloud_freq_avg_lat2(index2),color=icol,psym=1
;   if index2(0) ne -1L then oplot,fdoy_all(index2)-nhsol,cloud_freq_avg_lat2(index2),color=icol,psym=0,thick=3
    xyouts,xmn+0.02,ymx-0.03,'2007 (dot)',color=0,/normal,charsize=1.5,charthick=5
    xyouts,xmn+0.02,ymx-0.06,'2008 (plus)',color=0,/normal,charsize=1.5,charthick=5
    if latbin(ilat) le -60. then xyouts,xmx+0.02,ymx-0.05-0.05*ilat,slat+'N',color=icol,/normal,charsize=1.5,charthick=5
skiplatnh:
endfor

    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
;
; loop over SH lat bins
;
for ilat=0L,nlat/2-1L do begin
    if max(cloud_freq_max_all(*,ilat)) lt 2. and max(cloud_freq_max_all(*,ilat2)) lt 2. then goto,skiplatsh
    slat=string(format='(i2)',abs(latbin(ilat)))
if latbin(ilat) gt -60. then goto,skiplatsh
if latbin(ilat) eq -85. then icol=mcolor*.9
if latbin(ilat) eq -80. then icol=mcolor*.8
if latbin(ilat) eq -75. then icol=mcolor*.7
if latbin(ilat) eq -70. then icol=mcolor*.6
if latbin(ilat) eq -65. then icol=mcolor*.3
if latbin(ilat) eq -60. then icol=mcolor*.1
;
; avg frequency and sigma at this latitude
;
;   cloud_freq_avg_lat=smooth(reform(cloud_freq_avg_all(*,ilat)),3)     ; fltarr(kday) at ilat
    cloud_freq_avg_lat=reform(cloud_freq_avg_all(*,ilat))
    cloud_freq_max_lat=reform(cloud_freq_max_all(*,ilat))       ; fltarr(kday) at ilat
    cloud_freq_sigma_lat=smooth(reform(cloud_freq_sigma_all(*,ilat)),3)
    if ilat eq 0L then begin
    imin=-40.
    imax=80.
    ndoy=imax-imin
    xlab=['-40','-20','0','20','40','60','80']
    nxticks=n_elements(xtickname)
    plot,findgen(ndoy),cloud_freq_max_lat,ytitle='Cloud Radius',xrange=[imin,imax],$
         xticks=nxticks,xtickname=xlab,xtickv=float(xlab),charsize=1.5,color=0,/nodata,$
         yrange=[ymin,ymax],xtitle='Days from Solstice'
    endif
    syear=strmid(strcompress(sdate_all,/remove_all),0,4)
;
    shsol=355.
    index1=where(syear eq '2007' and fdoy_all ge shsol-40. and cloud_freq_avg_lat ge 0.1)
    if index1(0) ne -1L then oplot,fdoy_all(index1)-365.,cloud_freq_avg_lat(index1),color=icol,psym=8
;   if index1(0) ne -1L then oplot,fdoy_all(index1)-365.,cloud_freq_avg_lat(index1),color=icol,psym=0,thick=3
    index1=where(syear eq '2008' and fdoy_all le 120. and cloud_freq_avg_lat ge 0.1)
    if index1(0) ne -1L then oplot,fdoy_all(index1),cloud_freq_avg_lat(index1),color=icol,psym=8
;   if index1(0) ne -1L then oplot,fdoy_all(index1),cloud_freq_avg_lat(index1),color=icol,psym=0,thick=3
    
    index1=where(syear eq '2008' and fdoy_all ge shsol-40. and cloud_freq_avg_lat ge 0.1)
    if index1(0) ne -1L then oplot,fdoy_all(index1)-365.,cloud_freq_avg_lat(index1),color=icol,psym=1
;   if index1(0) ne -1L then oplot,fdoy_all(index1)-365.,cloud_freq_avg_lat(index1),color=icol,psym=0,thick=3
    index1=where(syear eq '2009' and fdoy_all lt shsol+80. and cloud_freq_avg_lat ge 0.1)
    if index1(0) ne -1L then oplot,fdoy_all(index1),cloud_freq_avg_lat(index1),color=icol,psym=1
;   if index1(0) ne -1L then oplot,fdoy_all(index1),cloud_freq_avg_lat(index1),color=icol,psym=0,thick=3

    xyouts,xmn+0.02,ymx-0.03,'2007-2008 (dot)',color=0,/normal,charsize=1.5,charthick=5
    xyouts,xmn+0.02,ymx-0.06,'2008-2009 (plus)',color=0,/normal,charsize=1.5,charthick=5
    if latbin(ilat) le -60. then xyouts,xmx+0.02,ymx-0.05-0.05*ilat,slat+'S',color=icol,/normal,charsize=1.5,charthick=5
skiplatsh:
endfor


;
; convert ps to jpg
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim timeseries_cipsl4_cloudrad_all_lat.ps -rotate -90 timeseries_cipsl4_cloudrad_all_lat.jpg'
    endif

end
