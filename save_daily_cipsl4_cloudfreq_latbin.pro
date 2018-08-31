;
; read CIPS level 4 data and store daily average cloud frequency 
; for each orbit binned every 5 degrees latitude
;
; color table and symbol
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
a=findgen(8)*(2*!pi/8.)
usersym,.5*cos(a),.5*sin(a),/fill
;
; latbins
;
nlat=35
latbin=-85.+5.*findgen(nlat)	; -85 to 85
yinc=(latbin(1)-latbin(0))/2.
;
; restore CIPS procedures and functions
;
restore,'read_cips_file.sav
pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_4_orbit_'

lstmn=5
lstdy=25
lstyr=2007
ledmn=4
leddy=1
ledyr=2009
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
; USE THE CLOUD PRESENCE MAP ARRAY TO CALCULATE FREQUENCIES. AND USES THE CPM=1 VALUE TO GET THE ALBEDOS.
; LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
; USING LIM=-99 ESSENTIALLY INCLUDES ALL POINTS THAT ARE FOUND WITH CLOUD_PRESENCE_MAP,
; EVEN IF THE ALBEDO IS NEGATIVE (WHICH DOES HAPPEN) -- BUT THEN THE ALB/ALB_ERR TEST MIGHT CATCH IT.
LIM=1.
ERRLIM=1.0	;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO
;SZALIM=91.	;DATA WITH SZA > SZALIM ARE BAD (IN NH THIS CAN ONLY HAPPEN ON THE ASCENDING NODE)
SZALIM=180.	;DON'T GET RID OF ANY DATA BASED ON SZA.
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
; if save file exists then skip
;
;     dum=findfile(pth+sdate+'_v03.20.sav')
;     if dum(0) ne '' then goto,jump
;
; get nc filenames on this day
;
      spawn,'ls '+pth+'*'+syear+'-'+sday+'_v03.20.nc',fnames
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          FNAME=FNAMES(iorbit)
          print,fname
          data=read_cips_file(fname,/full_path,attributes=attributes)
;
; extract contents from data structure
;
          AIM_ORBIT_NUMBER=data.AIM_ORBIT_NUMBER		;INT Cumulative mission orbit number
          VERSION=data.VERSION					;STRING    '03.20'
          PRODUCT_CREATION_TIME=data.PRODUCT_CREATION_TIME	;STRING    '2009/040-13:23:03' Version number of data product
          DEPENDANT1BVERSION=data.DEPENDANT1BVERSION		;STRING    '03.20'
          UT_DATE=DATA.UT_DATE					;LONG       2009001 UTC date of this orbit
          HEMISPHERE=DATA.HEMISPHERE				;STRING    'S'
          ORBIT_START_TIME=data.ORBIT_START_TIME		;DOUBLE 9.1480689e+14 Orbit start time in gps microseconds
          ORBIT_START_TIME_UT=data.ORBIT_START_TIME_UT		;DOUBLE   2.0548680 UTC time of the start of the orbit
          ORBIT_END_TIME=data.ORBIT_END_TIME			;DOUBLE 9.1481268e+14 Orbit end time in gps microseconds
          STACK_ID=data.STACK_ID				;INT        0 uniquely identify the Level 1B data
          XDIM=data.XDIM					;INT        X dimension of data. Average of 600
          YDIM=data.YDIM					;INT        Y dimension of data. Average is 150
          QUALITY_FLAGS=data.QUALITY_FLAGS			;LONG      TBD
          X_TILE_DIM=data.X_TILE_DIM				;INT       Array size (columns) of tiles is x direction
          Y_TILE_DIM=data.Y_TILE_DIM				;INT       Array size (rows) of tiles in the y direction
          KM_PER_PIXEL=data.KM_PER_PIXEL			;INT              5
          BBOX=data.BBOX					;LONG      Array[4] {x, y} bounding box of map projected image
          CENTER_LON=data.CENTER_LON	;Center longitude of map proj, NOT data. Used for orienting the data horizontally.
          NLAYERS=(*data[0].nlayers)	;# data layers corresponding to a pixel at [xDim, yDim] a diff SCA. Avg 8.
          UT_TIME=(*data[0].ut_time)				;POINTER  Number of seconds elapsed since orbit_start_time_ut
          PERCENT_CLOUDS=data.PERCENT_CLOUDS			;FLOAT    Percentage of tiles that have an identified cloud
          CLOUD_INDEX=(*DATA[0].CLOUD_PRESENCE_MAP)		;1 FOR CLOUD, 0 FOR NO CLOUD
          COMMON_VOLUME_MAP=(*DATA[0].COMMON_VOLUME_MAP)	;1=tile in the CVO, 0=otherwise
          LATITUDE=(*DATA[0].LATITUDE)				;latitude for every pixel
          LONGITUDE=(*DATA[0].LONGITUDE)			;longitude for every pixel
          latitude_orig=latitude				;save original lats to determine asc/desc
          longitude_orig=longitude				;save original lons to determine asc/desc
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)		;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)	;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
          CLD_PHASE_ALBEDO=(*data[0].cld_phase_albedo)		;The cloud phase albedo for each tile
          CLD_PHASE_ALBEDO_ERR=(*data[0].cld_phase_albedo_UNC)	;The cloud phase albedo uncertainty for each tile
          SZA = (*DATA[0].ZENITH_ANGLE_RAY_PEAK)		;Zenith angle at the peak of the Rayleigh contribution
          VIEW = (*DATA[0].VIEW_ANGLE_RAY_PEAK)			;View angle at the peak of the Rayleigh contribution
          SCA = (*DATA[0].SCATTERING_ANGLE)			;Scattering angle at the peak of the Rayleigh contribution
          ALB = (*data[0].cld_albedo)				;Cloud albedo in Garys (10^-6 sr^-1) i.e., alb x 1.e6
          ALB_ERR = (*DATA[0].CLD_ALBEDO_UNC)			;1 sigma formal uncertainty of cld_albedo
          IWC = (*data[0].ICE_WATER_CONTENT)		
          IWC_ERR = (*DATA[0].ICE_WATER_CONTENT_UNC)
          RAD=(*DATA[0].PARTICLE_RADIUS)			;Particle radius for each tile
          RAD_ERR=(*DATA[0].PARTICLE_RADIUS_UNC)		;Particle radius uncertainty for each tile
;
; not in this version?
;
;         OZONE_COL_DENSITY: Ozone column density for each tile.
;         OZONE_COL_DENSITY_UNC: Ozone column density uncertainty for each tile.
;         SCALE_HEIGHT_RATIO: Scale height ratio for each tile.
;         SCALE_HEIGHT_RATIO_UNC: Scale height ratio uncertainty for each tile.
;         PRE_PHASE_CORR_ALB: fillValue = 1
;         PRE_PHASE_CORR_RAD: fillValue = 1
;
; free memory
; 
          HEAP_GC
;         RESULT=MEMORY(/CURRENT)
;         PRINT,'MEMORY IS: ',RESULT
;         PRINT,' '
;
;OMIT ANY ALBEDOS THAT ARE SMALLER THAN LIM.
;DO NOT USE ASCENDING NODE DATA WHERE SZA GT ~91 DEG
;OMIT ANY DATA WHERE ALB_ERR/ALB GT ERRLIM.
;GET RID OF ANY INFINITE DATA AND ANY DATA ON THE ASCENDING NODE WHERE SZA GT SZA_LIM 
;(SZA IS NEVER GT SZA_LIM ON THE DESCENDING NODE).
;
          BAD=WHERE(FINITE(ALB) EQ 0 OR FINITE(ALB_ERR) EQ 0 OR $
                    ABS(ALB_ERR/ALB) GE ERRLIM OR SZA GT SZALIM,nbad)
          IF NBAD GT 0 THEN BEGIN
             ALB(BAD)=-99 & ALB_ERR(BAD)=-99
             RAD(BAD)=-99 & RAD_ERR(BAD)=-99
cloud_index(bad)=-99.
          ENDIF
;
; plot clouds color by brightness
;
;if iorbit eq 0L then begin
;   erase
;   map_set,-90,0,0,/ortho,/contin,/grid,title=sdate
;endif
;amin=lim
;amax=20.
;Y=WHERE(CLOUD_INDEX EQ 1 AND ALB GE LIM AND ALB_ERR NE -99,NY)
;if y(0) ne -1L then begin
;   for i=0L,ny-1L do begin
;       oplot,[longitude(y(i)),longitude(y(i))],[latitude(y(i)),latitude(y(i))],psym=8,$
;              color=((alb(y(i))-amin)/(amax-amin))*mcolor
;   endfor
;endif
;stop
;
; declare arrays to store binned data
;
          if iorbit eq 0L then begin 
             cloud_points=fltarr(nlat,norbit)
             total_points=fltarr(nlat,norbit)
;
; need to retain all raw orbit data for averaging at the end
;
;            ALB_avg=fltarr(nlat,norbit)
;            IWC_avg=fltarr(nlat,norbit)
;            RAD_avg=fltarr(nlat,norbit)
          endif
;
; count clouds points and all points in each latitude bin
; all points include only those where cloud_index is 0 or 1
;
         FOR ilat=0,NLAT-1 DO BEGIN
             all=WHERE(LATITUDE GT latbin(ilat)-yinc AND LATITUDE LE latbin(ilat)+yinc and $
                      (CLOUD_INDEX EQ 0 or CLOUD_INDEX EQ 1),npts)
             if all(0) ne -1L then total_points(ilat,iorbit)=float(npts)

;if all(0) ne -1L then begin
;index=where(LATITUDE GT latbin(ilat)-yinc AND LATITUDE LE latbin(ilat)+yinc and $
;            alb_err eq -99. and (CLOUD_INDEX EQ 0 or CLOUD_INDEX EQ 1),nn)
;print,'min alb_err for total points ',min(alb_err(all)),npts,nn
;endif

             X=WHERE(LATITUDE GT latbin(ilat)-yinc AND LATITUDE LE latbin(ilat)+yinc $
                     AND CLOUD_INDEX EQ 1 AND ALB GE LIM AND ALB_ERR NE -99.,NX)
             if x(0) ne -1L then cloud_points(ilat,iorbit)=float(nx)
;if x(0) ne -1 then print,latbin(ilat),100.*float(nx)/float(npts)
if float(nx) gt float(npts) then stop,'more clouds than data points'
         endfor
  
;if iorbit eq 0L then begin
;!p.background=mcolor
;erase
;!type=2^2+2^3
;plot,latbin,cloud_points(*,iorbit),psym=2,/nodata,title=sdate,yrange=[0.,100.],$
;     ytitle='Cloud Frequency (%)',xtitle='Latitude',xrange=[-90.,90.],color=0
;endif
;oplot,latbin,100.*cloud_points(*,iorbit)/total_points(*,iorbit),psym=0,color=(float(iorbit)/float(norbit))*mcolor,thick=3
stop
      ENDFOR   ;*** END LOOP OVER ORBITS
;
; save daily file
;
      ofile=pth+sdate+'_v03.20.sav'
      print,ofile
      SAVE,file=ofile,latbin,cloud_points,total_points,norbit
goto,jump
end
