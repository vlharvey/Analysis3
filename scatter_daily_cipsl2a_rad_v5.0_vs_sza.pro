;
; read CIPS level 2a version 5.00 data and plot radius for all orbits each day
;
loadct,0
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
xorig=[0.25]
yorig=[0.3]
xlen=0.6
ylen=0.5
cbaryoff=0.15
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.5*cos(a),.5*sin(a),/fill

ALBLIM=1.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ALBLIM=0.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO^M
SZALIM_HI=91.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT^M
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT^M

;pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'
pth='/Volumes/Data/CIPS_data/Datfiles/cips_sci_2_orbit_'

lstmn=4
lstdy=1
lstyr=2016
ledmn=4
leddy=26
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
      if ndays gt ledday then stop,' normal termination condition '
      syear=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syear+smn+sdy
      print,sdate,' ',sday
;
; get nc filenames on this day
;
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v05.00_r01_cat.nc',fnamescat
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v05.00_r01_cld.nc',fnamescld
      if fnamescat(0) eq '' then goto,jump
      if n_elements(fnamescat) ne n_elements(fnamescld) then goto,jump
      norbit=n_elements(fnamescat)
      orbit_all=lonarr(norbit)
nlvls=norbit
col1=1+indgen(nlvls)*icolmax/(nlvls+10)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          FNAME=FNAMESCAT(iorbit)
          print,fname
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
          print,fname
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
          alb=CLD_ALBEDO
          alb_err=CLD_ALBEDO_UNC
          sza=ZENITH_ANGLE_RAY_PEAK
          orbit_all(iorbit)=AIM_ORBIT_NUMBER
          rad=PARTICLE_RADIUS
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
          good=WHERE(FINITE(ALB) EQ 1 and FINITE(SZA) eq 1, ngood)	;	 and SZA LT SZALIM_HI AND SZA GT SZALIM_LO,ngood)	; AND FINITE(ALB_ERR) EQ 1 AND $
;                   (CLOUD_INDEX EQ 1 OR CLOUD_INDEX EQ 0) AND $
;                    CLOUD_INDEX EQ 1 AND $
;                    ALB GT ALBLIM and nlayers gt 6.,NGOOD)
;
          IF NGOOD GT 0 THEN BEGIN
             latitude=latitude(good)
             longitude=longitude(good)
             ut_time=ut_time(good)
             CLOUD_PRESENCE_MAP=CLOUD_PRESENCE_MAP(good)
             ALB=ALB(good)
             ALB_ERR=ALB_ERR(good)
             sza=sza(good)
             rad=rad(good)
             PARTICLE_RADIUS_UNC=PARTICLE_RADIUS_UNC(good)
             ICE_WATER_CONTENT=ICE_WATER_CONTENT(good)
             ICE_WATER_CONTENT_UNC=ICE_WATER_CONTENT_UNC(good)
             ICE_COLUMN_DENSITY=ICE_COLUMN_DENSITY(good)
          ENDIF
;
; plot clouds color by brightness
;
if iorbit eq 0L then begin
;
; retain vars for all orbits today
;
   rad_all=rad
   alb_all=alb
   alb_err_all=alb_err
   sza_all=sza
   cloud_presence_map_all=cloud_presence_map
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='scatter_daily_cipsl2a_rad_v5.0_vs_sza_'+sdate+'.ps'
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
   plot,sza,rad,xrange=[50,120],yrange=[0,100],psym=3,color=0,xtitle='SZA',ytitle='Radius (nm)',charsize=2,charthick=2
   xyouts,100,90,sdate,/data,charsize=2,charthick=2,color=0
   plots,90,0
   plots,90,100,/continue,color=0,thick=3

rmin=min(rad)
rmax=max(rad)
amin=min(alb)
amax=max(alb)
smin=min(sza)
smax=max(sza)

endif
oplot,sza,rad,psym=8,color=col1(iorbit)

if iorbit gt 0L then begin
   rad_all=[rad_all,rad]
   alb_all=[alb_all,alb]
   alb_err_all=[alb_err_all,alb_err]
   sza_all=[sza_all,sza]
   cloud_presence_map_all=[cloud_presence_map_all,cloud_presence_map]
endif

if min(rad) lt amin then rmin=min(rad)
if max(rad) gt amax then rmax=max(rad)
if min(alb) lt amin then amin=min(alb)
if max(alb) gt amax then amax=max(alb)
if min(sza) lt smin then smin=min(sza)
if max(sza) gt smax then smax=max(sza)

endfor  ; loop over orbits
loadct,39
xyouts,52,90,'ALB Range ('+strcompress(long(amin),/r)+' to '+strcompress(long(amax),/r)+')',/data,color=0,charsize=1.2,charthick=2
xyouts,52,85,'ERR Range ('+strcompress(min(alb_err_all),/r)+' to '+strcompress(long(max(alb_err_all)),/r)+')',/data,color=0,charsize=1.2,charthick=2
xyouts,52,80,'RAD Range ('+strcompress(long(rmin),/r)+' to '+strcompress(long(rmax),/r)+')',/data,color=0,charsize=1.2,charthick=2
index=where(CLOUD_PRESENCE_MAP_ALL eq 1.)
if index(0) ne -1L then oplot,sza_all(index),rad_all(index),psym=8,color=250,symsize=0.5
loadct,0

imin=min(orbit_all)
imax=max(orbit_all)
    ymnb=ymn -cbaryoff
    ymxb=ymnb+cbarydel
    set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xtitle='Version 5.00 CIPS Orbit Number',XTICKFORMAT='(i5)',charthick=2,charsize=1.2
    ybox=[0,10,10,0,0]
    x2=imin
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor

; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim scatter_daily_cipsl2a_rad_v5.0_vs_sza_'+sdate+'.ps -rotate -90 scatter_daily_cipsl2a_rad_v5.0_vs_sza_'+sdate+'.jpg'
        spawn,'rm -f scatter_daily_cipsl2a_rad_v5.0_vs_sza_'+sdate+'.ps'
     endif

goto,jump
end
