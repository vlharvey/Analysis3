;
; scatter plots of cloud albedo vs iwc
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=750
nydim=750
xorig=[0.25,0.25]
yorig=[0.55,0.15]
xlen=0.5
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
CLOUD_FREQ_AVG_ALL=CLOUD_ALB_ALL
CLOUD_FREQ_MAX_ALL=CLOUD_IWC_ALL
CLOUD_FREQ_SIGMA_ALL=CLOUD_RAD_ALL
;
; ps file 
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='scatter_cipsl4_rad_vs_iwc.ps'
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
    cloud_freq_avg_lat2=reform(cloud_freq_avg_all(*,ilat2))
    cloud_freq_max_lat2=reform(cloud_freq_max_all(*,ilat2))       ; fltarr(kday) at ilat2
    cloud_freq_sigma_lat2=reform(cloud_freq_sigma_all(*,ilat2))       ; fltarr(kday) at ilat2
    if ilat eq 0L then begin
    imin=40.
    imax=70.
    plot,cloud_freq_sigma_lat2,cloud_freq_max_lat2,ytitle='IWC',xrange=[imin,imax],$
         charsize=1.5,color=0,title='CIPS Level 4 V3.20',yrange=[0.,150.],/nodata
    endif
    syear=strmid(strcompress(sdate_all,/remove_all),0,4)
    index=where(cloud_freq_sigma_lat2 ge 0.1)
    if index(0) ne -1L then oplot,cloud_freq_sigma_lat2(index),cloud_freq_max_lat2(index),color=icol,psym=8,symsize=0.7
    if latbin(ilat) le -60. then xyouts,xmx+0.02,ymx-0.05-0.05*ilat,slat+'N',color=icol,/normal,charsize=1.5,charthick=5
skiplatnh:
endfor
xyouts,xmn+0.02,ymx-0.02,min(sdate_all)+'-'+max(sdate_all),/normal,charsize=1.5,color=0

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
    cloud_freq_sigma_lat2=reform(cloud_freq_sigma_all(*,ilat))
    cloud_freq_max_lat2=reform(cloud_freq_max_all(*,ilat))       ; fltarr(kday) at ilat2
    if ilat eq 0L then begin
    plot,cloud_freq_sigma_lat2,cloud_freq_max_lat2,ytitle='IWC',xtitle='Cloud Radius',xrange=[imin,imax],$
         charsize=1.5,color=0,yrange=[0.,150.],/nodata
    endif
    syear=strmid(strcompress(sdate_all,/remove_all),0,4)
    index=where(cloud_freq_sigma_lat2 ge 0.1)
    if index(0) ne -1L then oplot,cloud_freq_sigma_lat2(index),cloud_freq_max_lat2(index),color=icol,psym=8,symsize=0.7
    if latbin(ilat) le -60. then xyouts,xmx+0.02,ymx-0.05-0.05*ilat,slat+'S',color=icol,/normal,charsize=1.5,charthick=5
skiplatsh:
endfor
xyouts,xmn+0.02,ymx-0.02,min(sdate_all)+'-'+max(sdate_all),/normal,charsize=1.5,color=0
;
; convert ps to jpg
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim scatter_cipsl4_rad_vs_iwc.ps -rotate -90 scatter_cipsl4_rad_vs_iwc.jpg'
    endif

end
