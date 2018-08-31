;
; compare CIPS level 2 v4.2 and v5.10 sampling latitudes for all finite SZA pixels
; VLH 7/25/2017
;
@stddat
@kgmt
@ckday
@kdate
@mkltime

re=40000./2./!pi
rad=double(180./!pi)
dtr=double(!pi/180.)

loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
!p.background=icolmax
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.20]
yorig=[0.25]
xlen=0.7
ylen=0.5
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
pth='/atmos/harvey/CIPS_data/Datfiles/Level_2/cips_sci_2_orbit_'
;
; loop over years
;
for iyear=2007,2007 do begin
syear=strcompress(long(iyear),/r)

;goto,quick

lstmn=5
lstdy=24
lstyr=iyear
;ledmn=9
;leddy=9		; dfs +80

ledmn=12
leddy=31
ledyr=iyear
lstday=0
ledday=0
;
; Ask interactive questions- get starting/ending date and p surface
;
;read,' Enter starting date ',lstmn,lstdy,lstyr
;read,' Enter ending date ',ledmn,leddy,ledyr
if lstyr lt 91 then lstyr=lstyr+2000
if ledyr lt 91 then ledyr=ledyr+2000
if lstyr lt 1900 then lstyr=lstyr+1900
if ledyr lt 1900 then ledyr=ledyr+1900
if lstyr lt 1991 then stop,'Year out of range '
if ledyr lt 1991 then stop,'Year out of range '
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
kday=ledday-lstday+1L
sdate_all=strarr(kday*20)
dfs_all=fltarr(kday*20)
doy_all=fltarr(kday*20)
minlat20_all=fltarr(kday*20)
minlat51_all=fltarr(kday*20)
maxlat20_all=fltarr(kday*20)
maxlat51_all=fltarr(kday*20)
norbit20=fltarr(kday*20)
norbit51=fltarr(kday*20)
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount=0L
kcount=0L
;
; USE THE CLOUD PRESENCE MAP ARRAY TO CALCULATE FREQUENCIES. AND USES THE CPM=1 VALUE TO GET THE ALBEDOS.
; LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
; USING LIM=-99 ESSENTIALLY INCLUDES ALL POINTS THAT ARE FOUND WITH CLOUD_PRESENCE_MAP,
; EVEN IF THE ALBEDO IS NEGATIVE (WHICH DOES HAPPEN) -- BUT THEN THE ALB/ALB_ERR TEST MIGHT CATCH IT.
ALBLIM=2.
;ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO - not used here
SZALIM_HI=92.      ;DATA WITH SZA > SZALIM ARE SUSPECT
SZALIM_LO=42.

; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,plotyear
      syr=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syr+smn+sdy
      print,sdate
;
; get nc filenames on this day (I get an error if I try to read the gzipped file)
;
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v04.20_r05_cat.nc',fnamescat20
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v05.10_r01_cat.nc',fnamescat51
      if fnamescat20(0) eq '' or fnamescat51(0) eq '' then begin
         print,'no orbit files for one of the versions'
         goto,skipcips
      endif
      if n_elements(fnamescat20) ne n_elements(fnamescat51) then begin
         print,'different number of orbit files'
         goto,skipcips
      endif
      norbit=n_elements(fnamescat20)
;
; loop over orbits
;
col1=15.+(findgen(norbit)/float(norbit))*mcolor
      FOR iorbit = 0,norbit-1 DO BEGIN
          sdate_all(kcount)=sdate
          doy_all(kcount)=iday+float(iorbit)/float(norbit)
          if iday gt 90 and iday lt 274 then dfs_all(kcount)=iday-172.
          if iday le 90 or iday ge 274 then dfs_all(kcount)=iday-355.
          if dfs_all(kcount) lt -200 then dfs_all(kcount)=dfs_all(kcount)+365.
          norbit20(kcount)=n_elements(fnamescat20)
          norbit51(kcount)=n_elements(fnamescat51)

          FNAME=FNAMESCAT20(iorbit)
          print,fname
dum=strsplit(fname,'_',/extract)
syr_doy=dum(-4)
sorbit=dum(-5)
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          nvars=result.nvars
          for ivar=0,nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if ( ( size( data, /n_dimensions )  EQ 1 ) && ( size( data, /type ) EQ 1 ) ) then data = string( data )
              if Execute(result.name + ' = data') eq 0 then Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
          endfor
          NCDF_CLOSE, ncid
;
help,latitude
good=where(finite(ZENITH_ANGLE_RAY_PEAK))
print,'v4 ',min(latitude(good)),max(latitude(good))
if iorbit eq 0L then begin
erase
set_viewport,.1,.9,.1,.9
;map_set,-90,0,-90,/ortho,/contin,/grid,/noeras,color=0,title=syr_doy
;plot,findgen(10),findgen(10),yrange=[-150,-20],xrange=[-180,180],color=0,title=syr_doy,ytitle='Latitude',xtitle='Longitude',charsize=2,charthick=2
plot,findgen(10),findgen(10),yrange=[20,150],xrange=[-180,180],color=0,title=syr_doy,ytitle='Latitude',xtitle='Longitude',charsize=2,charthick=2
endif
oplot,longitude(good),latitude(good),psym=3,color=0
      minlat20_all(kcount,*)=min(LATITUDE(good))
      maxlat20_all(kcount,*)=max(LATITUDE(good))
;
; retain all 4.2 orbit data
;
          good=WHERE(finite(ZENITH_ANGLE_RAY_PEAK) eq 1,ngood)	; all data
          IF NGOOD GT 0 THEN BEGIN
             if iorbit eq 0L then begin
                SZA_ALL20=ZENITH_ANGLE_RAY_PEAK(good)
                LAT_ALL20=latitude(good)			; Latitudes greater (less) than 90 (-90) indicate ascending node data
;               LON_ALL20=longitude(good)
;               UT_TIME_ALL20=ut_time(good)
;               UT_DATE_ALL20=ut_date(good)
             endif
             if iorbit gt 0L then begin
                SZA_ALL20=[SZA_ALL20,ZENITH_ANGLE_RAY_PEAK(good)]
                LAT_ALL20=[LAT_ALL20,latitude(good)]
;               LON_ALL20=[LON_ALL20,longitude(good)]
;               UT_TIME_ALL20=[UT_TIME_ALL20,ut_time(good)]
;               UT_DATE_ALL20=[UT_DATE_ALL20,ut_date(good)]
             endif
;            print,'v4.20 min/max lat ',min(latitude(good)),max(latitude(good)),ngood,' points'
          ENDIF
;
; read v5.10 catalogue file
;
          FNAME=FNAMESCAT51(iorbit)
          print,fname
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)   
          nvars=result.nvars        
          for ivar=0,nvars-1 do begin
              result=ncdf_varinq(ncid,ivar) 
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if ( ( size( data, /n_dimensions )  EQ 1 ) && ( size( data, /type ) EQ 1 ) ) then data = string( data )
              if Execute(result.name + ' = data') eq 0 then Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            
          endfor
          NCDF_CLOSE, ncid
help,latitude
good=where(finite(ZENITH_ANGLE_RAY_PEAK))
oplot,longitude(good),latitude(good),psym=3,color=col1(iorbit)
;xyouts,CENTER_LON,10,sorbit,/data,charsize=1.5,charthick=2,color=col1(iorbit)
;xyouts,CENTER_LON,-25,sorbit,/data,charsize=1.5,charthick=2,color=col1(iorbit),orientation=90
xyouts,CENTER_LON,5,sorbit,/data,charsize=1.5,charthick=2,color=col1(iorbit),orientation=90
print,'v5 ',min(latitude(good)),max(latitude(good))

      minlat51_all(kcount,*)=min(LATITUDE(good))
      maxlat51_all(kcount,*)=max(LATITUDE(good))
;
; retain all 5.10 orbit data
;
          good=WHERE(finite(ZENITH_ANGLE_RAY_PEAK) eq 1,ngood)  ; all data
          IF NGOOD GT 0 THEN BEGIN
             if iorbit eq 0L then begin
                SZA_ALL51=ZENITH_ANGLE_RAY_PEAK(good)
                LAT_ALL51=latitude(good)                    ; Latitudes greater (less) than 90 (-90) indicate ascending node data
;               LON_ALL51=longitude(good)
;               UT_TIME_ALL51=ut_time(good)
;               UT_DATE_ALL51=ut_date(good)
             endif
             if iorbit gt 0L then begin
                SZA_ALL51=[SZA_ALL51,ZENITH_ANGLE_RAY_PEAK(good)]
                LAT_ALL51=[LAT_ALL51,latitude(good)]
;               LON_ALL51=[LON_ALL51,longitude(good)]
;               UT_TIME_ALL51=[UT_TIME_ALL51,ut_time(good)]
;               UT_DATE_ALL51=[UT_DATE_ALL51,ut_date(good)]
             endif
;            print,'v5.10 min/max lat ',min(latitude(good)),max(latitude(good)),ngood,' points'
          ENDIF

          kcount=kcount+1L

      endfor  ; loop over orbits
stop
;erase
;plot,SZA_ALL20,LAT_ALL20,psym=8,color=0,title=syr+'-'+sday
;oplot,SZA_ALL51,LAT_ALL51,psym=8,color=.3*mcolor,symsize=0.5
;stop
;
      skipcips:
      icount=icount+1L
goto,jump

plotyear:
;save,filename='cips_l2_minmax_latitude_'+syear+'.sav',sdate_all,doy_all,dfs_all,minlat20_all,minlat51_all,maxlat20_all,maxlat51_all,norbit20,norbit51
quick:
restore,'cips_l2_minmax_latitude_'+syear+'.sav

erase
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='yt_cips_l2_v4.20_v5.10_rawfiles_coverage_'+syear+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

kday=n_elements(sdate_all)
!type=2^2+2^3
set_viewport,xorig(0),xorig(0)+xlen,yorig(0),yorig(0)+ylen
plot,doy_all,minlat20_all,color=0,psym=8,title=syear,ytitle='Latitude',xtitle='DOY',xrange=[144.,max(doy_all)],yrange=[-150,150],charsize=2,charthick=2,/noeras,symsize=2,/nodata
xyouts,310,110,'v4.2',color=0,charsize=3,charthick=3,/data
xyouts,310,70,'v5.1',color=mcolor*.9,charsize=3,charthick=3,/data
for ii=0L,kday-1 do begin
    if minlat20_all(ii) ne 0. and maxlat20_all(ii) ne 0. then begin
    plots,doy_all(ii),minlat20_all(ii)
    plots,doy_all(ii),maxlat20_all(ii),thick=5,color=0,/continue
    endif
endfor
for ii=0L,kday-1 do begin
    if minlat51_all(ii) ne 0. and maxlat51_all(ii) ne 0. then begin
    plots,doy_all(ii),minlat51_all(ii)
    plots,doy_all(ii),maxlat51_all(ii),color=mcolor*.9,/continue
    oplot,[doy_all(ii),doy_all(ii)],[minlat51_all(ii),maxlat51_all(ii)],psym=3,color=0
    endif
endfor

mindiff=minlat20_all-minlat51_all
maxdiff=maxlat20_all-maxlat51_all
set_viewport,xorig(0)+0.1,xorig(0)+xlen/2.+0.1,yorig(0)+0.05,yorig(0)+ylen/2.
index=where(mindiff ne 0.)
plot,doy_all(index),mindiff(index),color=0,xrange=[144.,max(doy_all)],/noeras,yrange=[-13,3],ytitle='Latitude Diff',charsize=1.5,charthick=2,psym=3
index=where(maxdiff ne 0.)
oplot,doy_all(index),maxdiff(index),color=mcolor*.3,psym=3


    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim yt_cips_l2_v4.20_v5.10_rawfiles_coverage_'+syear+'.ps -rotate -90 yt_cips_l2_v4.20_v5.10_rawfiles_coverage_'+syear+'.png'
;      spawn,'rm -f yt_cips_l2_v4.20_v5.10_rawfiles_coverage_'+syear+'.ps'
    endif
endfor	; loop over years
end
