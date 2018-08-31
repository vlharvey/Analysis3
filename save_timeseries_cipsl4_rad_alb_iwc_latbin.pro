;
; read daily CIPS IDL save files of cloud albedo, iwc, radius binned every 5 degrees latitude
; timeseries in 4 seasons at latitudes 50-85 by 5 degrees in each hem
;
; color table and symbol, set path
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
a=findgen(8)*(2*!pi/8.)
usersym,.8*cos(a),.8*sin(a),/fill
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
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
;goto,quick
;
; set date range
;
lstmn=5
lstdy=25
lstyr=2007
ledmn=4
leddy=1
ledyr=2009

lstday=0
ledday=0
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
kday=ledday-lstday+1L
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount = 0
kcount = 0
;
; loop over days
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,plotit
      syear=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syear+smn+sdy
      if icount eq 0 then begin
         sdate_all=strarr(kday)
         fdoy_all=fltarr(kday)
         cloud_freq_max_all=fltarr(kday,nlat)
         cloud_freq_avg_all=fltarr(kday,nlat)
         cloud_freq_sigma_all=fltarr(kday,nlat)
         cloud_alb_all=fltarr(kday,nlat)
         cloud_iwc_all=fltarr(kday,nlat)
         cloud_rad_all=fltarr(kday,nlat)
         icount=1
      endif
      sdate_all(kcount)=sdate
      fdoy_all(kcount)=float(iday)
;
; skip if save file does not exist
;
      dum=findfile(pth+sdate+'_v03.20_all.sav')
      if dum(0) eq '' then begin
         print,sdate+' missing'
         kcount=kcount+1
         goto,jump
      endif
;
; restore daily file
; ALB_AVG         FLOAT     = Array[35]
; CLOUD_POINTS    FLOAT     = Array[35, 15]
; IWC_AVG         FLOAT     = Array[35]
; LATBIN          FLOAT     = Array[35]
; NORBIT          LONG      =           15
; RAD_AVG         FLOAT     = Array[35]
; TOTAL_POINTS    FLOAT     = Array[35, 15]
; TOT_AVG         FLOAT     = Array[35]
;
      ofile=pth+sdate+'_v03.20_all.sav'
      print,ofile
      restore,ofile	;contents: latbin,cloud_points,total_points,norbit
      if norbit eq 1 then goto,skip
;
; why are some of the total_points values large negative???
;
      bad=where(total_points lt 0.)
      if bad(0) ne -1L then begin
         total_points(bad)=0.
         print,'negative total points ',min(total_points(bad))
      endif
;
; convert number of cloud points to percent clouds
;
      good=where(total_points gt 0.)
      cloud_freq=0.*cloud_points
      if good(0) ne -1L then begin
         cloud_freq(good)=100.*cloud_points(good)/total_points(good)
         index=where(cloud_points gt total_points)
         if index(0) ne -1L then stop,'cld points > total'
      endif
;
; cloud array is (kday,nlat). daily avg, max, sig cloud arrays are (nlat,norbit)
; compute daily average and sigma at each lat
;
      for j=0,nlat-1 do begin
          cloud_freq_orbits=reform(cloud_freq(j,*))
          result=moment(cloud_freq_orbits)
          cloud_freq_max_all(kcount,j)=max(cloud_freq_orbits)	; max of all orbits in this lat
          cloud_freq_avg_all(kcount,j)=result(0)	; avg over orbits in this lat
          cloud_freq_sigma_all(kcount,j)=sqrt(result(1))
;print,latbin(j),cloud_freq_max_all(kcount,j),cloud_freq_avg_all(kcount,j),cloud_freq_sigma_all(kcount,j)
      endfor
;
; retain daily average albedos, iwc, radii
;
      cloud_alb_all(kcount,*)=alb_avg
      cloud_iwc_all(kcount,*)=iwc_avg
      cloud_rad_all(kcount,*)=rad_avg
;erase
;!type=2^2+2^3
;plot,latbin,alb_avg,psym=2,title=sdate,yrange=[0.,100.],$
;     ytitle='Cloud Albedo',xtitle='Latitude',xrange=[-90.,90.],color=0,thick=3
;stop

skip:

      kcount=kcount+1

goto,jump
;
; plot timeseries in each latitude bin
;
plotit:
save,file='cloud_rad_alb_iwc_latbin_all.sav',cloud_freq_avg_all,cloud_freq_max_all,cloud_freq_sigma_all,$
     latbin,fdoy_all,sdate_all,cloud_alb_all,cloud_iwc_all,cloud_rad_all
quick:
restore,'cloud_rad_alb_iwc_latbin_all.sav'
;
; loop over lat bins
;
cloud_freq_avg_all=cloud_alb_all
cloud_freq_max_all=cloud_alb_all
for ilat=0L,nlat/2-1L do begin
    index=where(latbin eq abs(latbin(ilat)))
    ilat2=index(0)
print,latbin(ilat),latbin(ilat2)
    if max(cloud_freq_max_all(*,ilat)) eq 0. and max(cloud_freq_max_all(*,ilat2)) eq 0. then goto,skiplat
    slat=string(format='(i2)',abs(latbin(ilat)))
;
; ps file for this lat bin
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='timeseries_cipsl4_cloudalb_'+slat+'.ps'
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
; avg and sigma at this latitude
;
    cloud_freq_avg_lat=reform(cloud_freq_avg_all(*,ilat))	; fltarr(kday) at ilat
    cloud_freq_max_lat=reform(cloud_freq_max_all(*,ilat))	; fltarr(kday) at ilat
    cloud_freq_sigma_lat=reform(cloud_freq_sigma_all(*,ilat)) 
;
; NH
;
    cloud_freq_avg_lat2=reform(cloud_freq_avg_all(*,ilat2))       ; fltarr(kday) at ilat2
    cloud_freq_max_lat2=reform(cloud_freq_max_all(*,ilat2))       ; fltarr(kday) at ilat2
    cloud_freq_sigma_lat2=reform(cloud_freq_sigma_all(*,ilat2))

;sdate_all=strarr(kday)
;fdoy_all=fltarr(kday)

    imin=0.
    imax=100.
    ndoy=365
    plot,findgen(ndoy),cloud_freq_max_lat,ytitle='Cloud Albedo',yrange=[imin,imax],$
         xtickname=smonth,xticks=n_elements(smonth)-1,charsize=1.5,color=0,/nodata,$
         title='CIPS Level 4 Latitude='+slat
;
; oplot each year
;
    syear=strmid(strcompress(sdate_all,/remove_all),0,4)
    index=where(syear eq '2007' and cloud_freq_avg_lat ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.3,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.3,psym=0
    index=where(syear eq '2007' and cloud_freq_avg_lat2 ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.3,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.3,psym=0
    index=where(syear eq '2008' and cloud_freq_avg_lat ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.5,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.5,psym=0
    index=where(syear eq '2008' and cloud_freq_avg_lat2 ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.5,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.5,psym=8
    index=where(syear eq '2009' and cloud_freq_avg_lat ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.9,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat(index),color=mcolor*.9,psym=0
    index=where(syear eq '2009' and cloud_freq_avg_lat2 ge 0.1)
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.9,psym=8
    if index(0) ne -1L then oplot,fdoy_all(index),cloud_freq_avg_lat2(index),color=mcolor*.9,psym=0
    xyouts,300.,95.,'2007',color=mcolor*.3,/data,charsize=1.5,charthick=5
    xyouts,300.,90.,'2008',color=mcolor*.5,/data,charsize=1.5,charthick=5
    xyouts,300.,85.,'2009',color=mcolor*.9,/data,charsize=1.5,charthick=5
;
; convert ps to jpg
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim timeseries_cipsl4_cloudalb_'+slat+'.ps -rotate -90 timeseries_cipsl4_cloudalb_'+slat+'.jpg'
    endif

skiplat:
endfor
end
