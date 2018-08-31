;
; read CIPS level 2a version 5.00 data and plot timeseries of the percent of points = 1 in the cloud_presence_map array
;
loadct,39
mcolor=!p.color
icolmax=byte(!p.color)
mcolor=icolmax
icmm1=icolmax-1B
icmm2=icolmax-2B
device,decompose=0
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.02
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

ALBLIM=2.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO
SZALIM_HI=92.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT

pth='/Volumes/Data/CIPS_data/Datfiles/cips_sci_2_orbit_'

lstmn=4
lstdy=1
lstyr=2016
ledmn=5
leddy=8
ledyr=2016
lstday=0
ledday=0
;
; Ask interactive questions- get starting/ending date
;
;print, ' '
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
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
percent_clouds_daily=fltarr(kday)	; PERCENT_CLOUDS variable
percent_clouds_daily_max=fltarr(kday)	; PERCENT_CLOUDS variable max
percent_clouds_daily_min=99.+fltarr(kday)	; PERCENT_CLOUDS variable min
clouds_presence_map_daily=fltarr(kday)	; percent of points with cloud_presence_map=1
sdate_all=strarr(kday)
doy_all=fltarr(kday)
nlat=5
dlat=5.
latbin=62.5+dlat*findgen(nlat)
clouds_presence_map_daily_lats=fltarr(kday,nlat)
icount=0L
goto,plotit		; if this is uncommented then also uncomment the restore line and comment out the save line
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
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
      sdate_all(icount)=sdate
      doy_all(icount)=iday
;
; get nc filenames on this day
;
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v05.00_r01_cat*.nc',fnamescat
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v05.00_r01_cld*.nc',fnamescld
      if fnamescat(0) eq '' then goto,jump
      if n_elements(fnamescat) ne n_elements(fnamescld) then goto,jump
      norbit=n_elements(fnamescat)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          FNAME=FNAMESCAT(iorbit)
;         print,fname
;
; read catalog file
;
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for idim=0,result.ndims-1 do begin
              ncdf_diminq,ncid,idim,name,dim
              if name eq 'dim1_UT_TIME' then dim1_UT_TIME=dim
              if name eq 'dim2_UT_TIME' then dim2_UT_TIME=dim
              if name eq 'dim1_NLAYERS' then dim1_NLAYERS=dim
              if name eq 'dim2_NLAYERS' then dim2_NLAYERS=dim
;             if name eq 'dim1_RATALL' then dim1_RATALL=dim
;             if name eq 'dim2_RATALL' then dim2_RATALL=dim
;             if name eq 'dim1_BBOX' then dim1_BBOX=dim
              if name eq 'dim1_LATITUDE' then dim1_LATITUDE=dim
              if name eq 'dim2_LATITUDE' then dim2_LATITUDE=dim
              if name eq 'dim1_LONGITUDE' then dim1_LONGITUDE=dim
              if name eq 'dim2_LONGITUDE' then dim2_LONGITUDE=dim
              if name eq 'dim1_ZENITH_ANGLE_RAY_PEAK' then dim1_ZENITH_ANGLE_RAY_PEAK=dim
              if name eq 'dim2_ZENITH_ANGLE_RAY_PEAK' then dim2_ZENITH_ANGLE_RAY_PEAK=dim
              if name eq 'dim1_COMMON_VOLUME_MAP' then dim1_COMMON_VOLUME_MAP=dim 
              if name eq 'dim2_COMMON_VOLUME_MAP' then dim2_COMMON_VOLUME_MAP=dim
;             print,'read ',name,' dimension ',dim
          endfor
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if result.name eq 'AIM_ORBIT_NUMBER' then AIM_ORBIT_NUMBER=data		; short AIM_ORBIT_NUMBER(structure_elements) ;
              if result.name eq 'VERSION' then VERSION=data				; char VERSION(structure_elements, string) ;
              if result.name eq 'REVISION' then REVISION=data			 	; char REVISION(structure_elements, string) ;
              if result.name eq 'PRODUCT_CREATION_TIME' then PRODUCT_CREATION_TIME=data	; char PRODUCT_CREATION_TIME(structure_elements, string) ;
              if result.name eq 'DEPENDENT_1B_VERSION' then DEPENDENT_1B_VERSION=data	; char DEPENDENT_1B_VERSION(structure_elements, string) ;
              if result.name eq 'UT_DATE' then UT_DATE=data				; int UT_DATE(structure_elements) ;
              if result.name eq 'UT_TIME' then UT_TIME=data				; double UT_TIME(structure_elements) ;
              if result.name eq 'HEMISPHERE' then HEMISPHERE=data			; char HEMISPHERE(structure_elements, string) ;
              if result.name eq 'ORBIT_START_TIME' then ORBIT_START_TIME=data		; double ORBIT_START_TIME(structure_elements) ;
              if result.name eq 'ORBIT_START_TIME_UT' then ORBIT_START_TIME_UT=data	; double ORBIT_START_TIME_UT(structure_elements) ;
              if result.name eq 'ORBIT_END_TIME' then ORBIT_END_TIME=data		; double ORBIT_END_TIME(structure_elements) ;
;             if result.name eq 'STACK_ID' then STACK_ID=data				; short STACK_ID(structure_elements) ;
              if result.name eq 'XDIM' then XDIM=data					; int XDIM(structure_elements) ;
              if result.name eq 'YDIM' then YDIM=data					; int YDIM(structure_elements) ;
              if result.name eq 'NLAYERS' then NLAYERS=data				; short NLAYERS(structure_elements, dim2_NLAYERS, dim1_NLAYERS) ;
              if result.name eq 'RATALL' then RATALL=data				; float RATALL(structure_elements, dim2_RATALL, dim1_RATALL) ;
              if result.name eq 'QUALITY_FLAGS' then QUALITY_FLAGS=data			; int QUALITY_FLAGS(structure_elements) ;
              if result.name eq 'KM_PER_PIXEL' then KM_PER_PIXEL=data			; float KM_PER_PIXEL(structure_elements) ;
              if result.name eq 'BBOX' then BBOX=data					; int BBOX(structure_elements, dim1_BBOX) ;
              if result.name eq 'CENTER_LON' then CENTER_LON=data			; double CENTER_LON(structure_elements) ;
              if result.name eq 'LATITUDE' then LATITUDE=data				; float LATITUDE(structure_elements, dim2_LATITUDE, dim1_LATITUDE) ;
              if result.name eq 'LONGITUDE' then LONGITUDE=data				; float LONGITUDE(structure_elements, dim2_LONGITUDE, dim1_LONGITUDE) ;
              if result.name eq 'ZENITH_ANGLE_RAY_PEAK' then ZENITH_ANGLE_RAY_PEAK=data	; float ZENITH_ANGLE_RAY_PEAK(structure_elements, dim2_ZENITH_ANGLE_RAY_PEAK, dim1_ZENITH_ANGLE_RAY_PEAK) ;
              if result.name eq 'COMMON_VOLUME_MAP' then COMMON_VOLUME_MAP=data		; byte COMMON_VOLUME_MAP(structure_elements, dim2_COMMON_VOLUME_MAP, dim1_COMMON_VOLUME_MAP) ;
              if result.name eq 'NOTES' then NOTES=data					; char NOTES(structure_elements, string) ;
;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
          sorbit=strcompress(AIM_ORBIT_NUMBER,/remove_all)
          latitude_orig=latitude				;save original lats to determine asc/desc
          longitude_orig=longitude				;save original lons to determine asc/desc
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)		;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)	;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
;
; read cld file
;
          FNAME=FNAMESCLD(iorbit)
;         print,fname
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for idim=0,result.ndims-1 do begin
              ncdf_diminq,ncid,idim,name,dim
              if name eq 'dim1_CLOUD_PRESENCE_MAP' then dim1_CLOUD_PRESENCE_MAP=dim	; dim1_CLOUD_PRESENCE_MAP = 1056 ;
              if name eq 'dim2_CLOUD_PRESENCE_MAP' then dim2_CLOUD_PRESENCE_MAP=dim	; dim1_CLOUD_PRESENCE_MAP = 264 ;
;        dim1_CLD_ALBEDO = 1938 ;
;        dim1_CLD_ALBEDO_UNC = 1938 ;
;        dim1_PARTICLE_RADIUS = 1938 ;
;        dim1_PARTICLE_RADIUS_UNC = 1938 ;
;        dim1_ICE_WATER_CONTENT = 1938 ;
;        dim1_ICE_WATER_CONTENT_UNC = 1938 ; 
;        dim1_ICE_COLUMN_DENSITY = 1938 ;
;             print,'read ',name,' dimension ',dim
          endfor
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if result.name eq 'PERCENT_CLOUDS' then PERCENT_CLOUDS=data		; float PERCENT_CLOUDS ;
              if result.name eq 'CLOUD_PRESENCE_MAP' then CLOUD_PRESENCE_MAP=data	; short CLOUD_PRESENCE_MAP(structure_elements, dim1_CLOUD_PRESENCE_MAP) ;
              if result.name eq 'CLD_ALBEDO' then CLD_ALBEDO=data			; float CLD_ALBEDO(structure_elements, dim1_CLD_ALBEDO) ;
              if result.name eq 'CLD_ALBEDO_UNC' then CLD_ALBEDO_UNC=data		; float CLD_ALBEDO_UNC(structure_elements, dim1_CLD_ALBEDO_UNC) ;
              if result.name eq 'PARTICLE_RADIUS' then PARTICLE_RADIUS=data		; float PARTICLE_RADIUS(structure_elements, dim1_PARTICLE_RADIUS) ;
              if result.name eq 'PARTICLE_RADIUS_UNC' then PARTICLE_RADIUS_UNC=data	; float PARTICLE_RADIUS_UNC(structure_elements, dim1_PARTICLE_RADIUS_UNC) ;
              if result.name eq 'ICE_WATER_CONTENT' then ICE_WATER_CONTENT=data		; float ICE_WATER_CONTENT(structure_elements, dim1_ICE_WATER_CONTENT) ;
              if result.name eq 'ICE_WATER_CONTENT_UNC' then ICE_WATER_CONTENT_UNC=data	; float ICE_WATER_CONTENT_UNC(structure_elements, dim1_ICE_WATER_CONTENT_UNC) ;
              if result.name eq 'ICE_COLUMN_DENSITY' then ICE_COLUMN_DENSITY=data	; float ICE_COLUMN_DENSITY(structure_elements, dim1_ICE_COLUMN_DENSITY) ;
              if result.name eq 'CHI_SQ' then CHI_SQ=data				; string chi-squared value calculated from the best fit to the measured phase function used to derived cloud albedo and particle size
;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
;print,'percent_clouds ',PERCENT_CLOUDS
;index=where(cloud_presence_map eq 1)
;if index(0) ne -1L then print,'range of ALB for CPM=1 ',min(cld_albedo(index)),max(cld_albedo(index))
;stop
;
; free memory
; 
          HEAP_GC
;         RESULT=MEMORY(/CURRENT)
;         PRINT,'MEMORY IS: ',RESULT
;         PRINT,' '
;
; filter bad data based on specifications set above
;
          good=WHERE(FINITE(CLD_ALBEDO) EQ 1 and LATITUDE GE 60., NGOOD)
          IF NGOOD GT 0 THEN BEGIN
             LATITUDE=LATITUDE(GOOD)
             LONGITUDE=LONGITUDE(GOOD)
             UT_DATE=UT_DATE(GOOD)
             UT_TIME=UT_TIME(GOOD)
             CLOUD_PRESENCE_MAP=CLOUD_PRESENCE_MAP(good)
             CLD_ALBEDO=CLD_ALBEDO(good)
             CLD_ALBEDO_UNC=CLD_ALBEDO_UNC(good)
             PARTICLE_RADIUS=PARTICLE_RADIUS(good)
             PARTICLE_RADIUS_UNC=PARTICLE_RADIUS_UNC(good)
             ICE_WATER_CONTENT=ICE_WATER_CONTENT(good)
             ICE_WATER_CONTENT_UNC=ICE_WATER_CONTENT_UNC(good)
             ICE_COLUMN_DENSITY=ICE_COLUMN_DENSITY(good)
             ZENITH_ANGLE_RAY_PEAK=ZENITH_ANGLE_RAY_PEAK(good)
;erase
;plot,ZENITH_ANGLE_RAY_PEAK,cld_albedo,psym=1,color=0,xrange=[50,120],yrange=[0,50],title=fname,ytitle='ALB',xtitle='SZA'
;index=where(CLOUD_PRESENCE_MAP eq 1,ncld)
;if index(0) ne -1L then oplot,ZENITH_ANGLE_RAY_PEAK(index),cld_albedo(index),psym=1,color=250 & print,ncld,n_elements(CLOUD_PRESENCE_MAP),100.*float(ncld)/float(n_elements(CLOUD_PRESENCE_MAP))
;stop
;
; retain all cloud_presence_map and latitude values today - but only if there are "good" values!
;
             if iorbit eq 0L then begin
                ALBEDO_ALL=CLD_ALBEDO
                CLOUD_PRESENCE_MAP_ALL=CLOUD_PRESENCE_MAP
                ZENITH_ANGLE_RAY_PEAK_ALL=ZENITH_ANGLE_RAY_PEAK
                LATITUDE_ALL=LATITUDE
             endif
             if iorbit gt 0L then begin
                ALBEDO_ALL=[ALBEDO_ALL,CLD_ALBEDO]
                ZENITH_ANGLE_RAY_PEAK_ALL=[ZENITH_ANGLE_RAY_PEAK_ALL,ZENITH_ANGLE_RAY_PEAK]
                CLOUD_PRESENCE_MAP_ALL=[CLOUD_PRESENCE_MAP_ALL,CLOUD_PRESENCE_MAP]
                LATITUDE_ALL=[LATITUDE_ALL,LATITUDE]
             endif

          ENDIF

          percent_clouds_daily(icount)=percent_clouds_daily(icount)+PERCENT_CLOUDS  ; PERCENT_CLOUDS variable
          if PERCENT_CLOUDS gt percent_clouds_daily_max(icount) then percent_clouds_daily_max(icount)=PERCENT_CLOUDS  ; PERCENT_CLOUDS variable
          if PERCENT_CLOUDS lt percent_clouds_daily_min(icount) then percent_clouds_daily_min(icount)=PERCENT_CLOUDS  ; PERCENT_CLOUDS variable

      ENDFOR  ; LOOP OVER ORBITS
;
; percent clouds today
;
percent_clouds_daily(icount)=percent_clouds_daily(icount)/float(norbit)					; average of orbit-by-orbit single values

;erase
;plot,ZENITH_ANGLE_RAY_PEAK_ALL,albedo_ALL,psym=1,color=0,xrange=[50,120],yrange=[-50,50],title=fname,ytitle='ALB',xtitle='SZA'
;index=where(CLOUD_PRESENCE_MAP_ALL eq 1 and albedo_ALL gt alblim,ncld)
;if index(0) ne -1L then oplot,ZENITH_ANGLE_RAY_PEAK_ALL(index),albedo_ALL(index),psym=1,color=250 & print,ncld,n_elements(CLOUD_PRESENCE_MAP),100.*float(ncld)/float(n_elements(CLOUD_PRESENCE_MAP))
;print,'% ',100.*float(ncld)/float(n_elements(albedo_ALL))
;stop

good=where(CLOUD_PRESENCE_MAP_ALL eq 1. and ALBEDO_ALL gt ALBLIM and ZENITH_ANGLE_RAY_PEAK_ALL lt SZALIM_HI,ngood)
clouds_presence_map_daily(icount)=100.*float(ngood)/float(n_elements(CLOUD_PRESENCE_MAP_ALL))		; percent of points with cloud_presence_map=1
;
; percent clouds in 5 degree latitude bins
;
;print,'		lat	nclouds		npoints		CPM%'
for j=0L,nlat-1L do begin
    index1=where(latitude_all ge latbin(j)-dlat/2.0 and latitude_all lt latbin(j)+dlat/2.0,npoints)
    index2=where(latitude_all ge latbin(j)-dlat/2.0 and latitude_all lt latbin(j)+dlat/2.0 and CLOUD_PRESENCE_MAP_ALL eq 1 and ALBEDO_ALL gt ALBLIM,nclouds)
    clouds_presence_map_daily_lats(icount,j)=100.*float(nclouds)/float(npoints)
;    print,latbin(j),nclouds,npoints,clouds_presence_map_daily_lats(icount,j)
endfor

print,iday,' ',sdate,percent_clouds_daily(icount),clouds_presence_map_daily(icount)
icount=icount+1L

goto,jump

plotit:
restore,'cipsl2a_v5.0_cloud_presence_map_20160401-20160508.sav

if setplot eq 'ps' then begin
   lc=0
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/times,filename='timeseries_cipsl2a_v5.0_cloud_presence_map_'+sdate_all(0)+'-'+sdate_all(-1)+'.ps'
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
; date labels
;
year0=strmid(sdate_all(0),0,4)      ; first 4 characters in the first element of the date array
year1=strmid(sdate_all(-1),0,4)     ; first 4 characters in the last element of date array
yearlabel=year0
if year0 ne year1 then yearlabel=year0+'-'+year1
smon=strmid(sdate_all,4,2)
sday=strmid(sdate_all,6,2)
xindex=where(sday eq '01' or sday eq '15',nxticks)
xlabs=smon(xindex)+'/'+sday(xindex)
dfs_all=doy_all-172.
plot,dfs_all,clouds_presence_map_daily,xrange=[min(dfs_all),max(dfs_all)],yrange=[0,20],ytitle='CIPS L2 V5.00 Percent Clouds',charsize=2,charthick=2,color=0,xtitle='DFS'
;     xticks=nxticks-1,xtickname=xlabs,xtickv=xindex,title=yearlabel,thick=15
loadct,0
for i=0L,kday-1L do begin
    plots,dfs_all(i),percent_clouds_daily_min(i)
    plots,dfs_all(i),percent_clouds_daily_max(i),color=150,thick=30,/continue
endfor
xyouts,xmn+0.02,ymx-0.05,'% CPM=1 & >60N & ALB>2G & SZA<92',color=0,charsize=1.25,charthick=2,/normal
xyouts,xmn+0.02,ymx-0.1,'PERCENT CLOUDS MIN/MEAN/MAX',color=80,charsize=1.25,charthick=2,/normal
oplot,dfs_all,percent_clouds_daily,color=80,thick=20
loadct,39
col1=30+(findgen(nlat)/float(nlat))*mcolor
for j=0L,nlat-1L do begin
    oplot,dfs_all,clouds_presence_map_daily_lats(*,j),color=col1(j),thick=15
    xyouts,xmx-0.125,ymx-0.05-j*0.05,strcompress(long(latbin(j)-dlat/2.0),/r)+'-'+strcompress(long(latbin(j)+dlat/2.0),/r),color=col1(j),charsize=2,charthick=2,/normal
endfor
oplot,dfs_all,clouds_presence_map_daily,color=0,thick=25

;save,filename='cipsl2a_v5.0_cloud_presence_map_'+sdate_all(0)+'-'+sdate_all(-1)+'.sav',latbin,sdate_all,clouds_presence_map_daily,kday,percent_clouds_daily,clouds_presence_map_daily_lats,dlat,dfs_all,doy_all,percent_clouds_daily_max,percent_clouds_daily_min

; Close PostScript file and return control to X-windows
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim timeseries_cipsl2a_v5.0_cloud_presence_map_'+sdate_all(0)+'-'+sdate_all(-1)+'.ps -rotate -90 '+$
                       'timeseries_cipsl2a_v5.0_cloud_presence_map_'+sdate_all(0)+'-'+sdate_all(-1)+'.jpg'
endif
end
