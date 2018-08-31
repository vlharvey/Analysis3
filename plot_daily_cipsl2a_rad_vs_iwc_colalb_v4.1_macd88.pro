;
; run on MacD88
; read CIPS level 2a version 4.01 data and plot radius vs IWC colored by albedo
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
yorig=[0.25]
xlen=0.7
ylen=0.6
cbaryoff=0.1
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

ALBLIM=1.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ALBLIM=0.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO^M
SZALIM_HI=91.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT^M
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT^M

;pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on aura
;pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macp98
pth='/Volumes/earth/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macd88

lstmn=6
lstdy=1
lstyr=2007
ledmn=9
leddy=22
ledyr=2007
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
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v04.10_r01_cat.nc',fnamescat
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*v04.10_r01_cld.nc',fnamescld
      if fnamescat(0) eq '' then goto,jump
      if n_elements(fnamescat) ne n_elements(fnamescld) then goto,jump
      norbit=n_elements(fnamescat)
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
              if name eq 'dim1_NLAYERS' then dim1_NLAYERS=dim
              if name eq 'dim2_NLAYERS' then dim2_NLAYERS=dim
              if name eq 'dim1_RATALL' then dim1_RATALL=dim
              if name eq 'dim2_RATALL' then dim2_RATALL=dim
              if name eq 'dim1_BBOX' then dim1_BBOX=dim
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
              if result.name eq 'STACK_ID' then STACK_ID=data				; short STACK_ID(structure_elements) ;
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
              if name eq 'dim1_CLOUD_PRESENCE_MAP' then dim1_CLOUD_PRESENCE_MAP=dim	; dim1_CLOUD_PRESENCE_MAP = 1938 ;
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
;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
          cloud_index=cloud_presence_map
          alb=CLD_ALBEDO
          alb_err=CLD_ALBEDO_UNC
          sza=ZENITH_ANGLE_RAY_PEAK
index=where(finite(sza) eq 0L)
if index(0) ne -1L then sza(index)=0.
          rad=PARTICLE_RADIUS
          iwc=ICE_WATER_CONTENT
          icd=ICE_COLUMN_DENSITY
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
          good=WHERE(FINITE(ALB) EQ 1 and SZA LT SZALIM_HI AND SZA GT SZALIM_LO,ngood)	
;                    FINITE(ALB_ERR) EQ 1 AND $	;; v4.10 alb_err all NaN
;                   (CLOUD_INDEX EQ 1 OR CLOUD_INDEX EQ 0) AND $
;                    ALB Ge ALBLIM,ngood)	; and nlayers gt 6.,NGOOD)

          IF NGOOD GT 0 THEN BEGIN
             latitude=latitude(good)
             longitude=longitude(good)
             ut_date=ut_date(good)
             ut_time=ut_time(good)
             sza=sza(good)
             CLOUD_INDEX=CLOUD_INDEX(good)
             ALB=ALB(good)
             ALB_ERR=ALB_ERR(good)
             RAD=PARTICLE_RADIUS(good)
             RAD_ERR=PARTICLE_RADIUS_UNC(good)
             IWC=ICE_WATER_CONTENT(good)
             IWC_ERR=ICE_WATER_CONTENT_UNC(good)
             ICD=ICE_COLUMN_DENSITY(good)
             nlayers=nlayers(good)
          ENDIF
;
; plot 
;
if iorbit eq 0L then begin
   if setplot eq 'ps' then begin
      xsize=nxdim/100.
      ysize=nydim/100.
      set_plot,'ps'
      !p.font=0
      device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
             /bold,/color,bits_per_pixel=8,/helvetica,filename='plot_daily_cipsl2_rad_vs_iwc_colalb_v4.1_'+sdate+'.ps'
      !p.charsize=1.5
      !p.thick=1.5
      !p.charthick=5
      !p.charthick=5
      !y.thick=1.5
      !x.thick=1.5
   endif
   erase
   !type=2^2+2^3
xmn=xorig(iorbit)
xmx=xorig(iorbit)+xlen
ymn=yorig(iorbit)
ymx=yorig(iorbit)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,rad,iwc,psym=3,color=0,xtitle='Radius (nm)',ytitle='Version 4.10 CIPS IWC (g/m!u2!n)',title=sdate,xrange=[0.,100.],yrange=[0.,1000.],/nodata
xyouts,5.,950.,'Radius Offset by 0.06 Each Orbit',color=0,/data,charsize=1.25
endif
cloud=WHERE(ALB GT ALBLIM and $ ;       ,ncloud)        ; AND CLOUD_INDEX EQ 1,ncloud)
           SZA LT SZALIM_HI AND SZA GT SZALIM_LO,ncloud)        ; AND $nlayers gt 6.,NCLOUD)
offset=0.06*iorbit
if cloud(0) ne -1L then oplot,rad(cloud)+offset,iwc(cloud),psym=3,color=0
;
; color by albedo
;
index=where(alb ge 1. and alb le 3.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.1
index=where(alb gt 3. and alb le 4.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.2
index=where(alb gt 4. and alb le 5.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.3
index=where(alb gt 5. and alb le 10.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.4
index=where(alb gt 10. and alb le 20.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.5
index=where(alb gt 20. and alb le 30.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.6
index=where(alb gt 30. and alb le 40.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.7
index=where(alb gt 40. and alb le 50.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.8
index=where(alb gt 50.)
if index(0) ne -1L then oplot,rad(index)+offset,iwc(index),psym=3,color=mcolor*.9

endfor  ; loop over orbits

slevel=['1','3','4','5','10','20','30','40','50','>50']
nlvls=n_elements(slevel)
col1=1+indgen(nlvls)*icolmax/nlvls
imin=1.
imax=60.
    ymnb=ymn -cbaryoff
    ymxb=ymnb+cbarydel
    set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xticks=nlvls-1,xtickname=slevel,$
          xtitle='Albedo (10!u-6!n str!u-1!n)'
    ybox=[0,10,10,0,0]
    x2=imin
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor

;
; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim plot_daily_cipsl2_rad_vs_iwc_colalb_v4.1_'+sdate+'.ps -rotate -90 '+$
                            'plot_daily_cipsl2_rad_vs_iwc_colalb_v4.1_'+sdate+'.jpg'
        spawn,'rm -f plot_daily_cipsl2_rad_vs_iwc_colalb_v4.1_'+sdate+'.ps'
     endif

goto,jump
end
