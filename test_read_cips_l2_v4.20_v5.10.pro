;
; test new read code
; compare CIPS level 2 v4.2 and v5.10
; VLH 9/13/2016
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
usersym,0.5*cos(a),0.5*sin(a),/fill
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.15,0.15,0.15]
yorig=[0.7,0.4,0.1]
xlen=0.7
ylen=0.225
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
for iyear=2007,2015 do begin
syear=strcompress(long(iyear),/r)

;goto,quick

lstmn=6
lstdy=14	; dfs -40
lstyr=iyear
ledmn=9
leddy=9		; dfs +80
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
sdate_all=strarr(kday)
dfs_all=fltarr(kday)
freq20=fltarr(kday)
freq51=fltarr(kday)
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
      sdate_all(icount)=sdate
      dfs_all(icount)=iday-172.
;
; get nc filenames on this day (I get an error if I try to read the gzipped file)
;
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v04.20_r05_cld.nc',fnames20
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v04.20_r05_cat.nc',fnamescat20
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v05.10_r01_cld.nc',fnames51
      spawn,'ls '+pth+'*'+syr+'-'+sday+'_v05.10_r01_cat.nc',fnamescat51
      if fnames20(0) eq '' or fnames51(0) eq '' then begin
         print,'no orbit files for one of the versions'
         goto,skipcips
      endif
      norbit20=n_elements(fnames20)
      norbit51=n_elements(fnames51)
      if norbit20 ne norbit51 then begin
         print,'different number of orbit files'
         goto,skipcips
      endif
      norbit=norbit20
;
; loop over orbits
;
      norbit=2L       ; testing purposes
      FOR iorbit = 0,norbit-1 DO BEGIN
          FNAME=FNAMESCAT20(iorbit)
          print,fname
;
; read 4.2 catalog file
;
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
            if Execute(result.name + ' = data') eq 0 then $
            Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            

; Invoke some magic to check for string data
 ; masquerading as byte data, but don't convert
 ; byte data blindly, i.e., quality_flags is a 2-dimensional
 ; array of byte data.
 ; (This is done to account for a bug in the ncdf write routine.)
            if ( ( size( data, /n_dimensions )  EQ 1 ) && $
              ( size( data, /type ) EQ 1 ) ) then $
              data = string( data )

;Extract each variable from the "data" structure and name it
;  the corresponding "name" from "result.name":
            if Execute(result.name + ' = data') eq 0 then $
            Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            

;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
;
; read 4.2 cld file
;
          FNAME=FNAMES20(iorbit)
          print,fname
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data

            if ( ( size( data, /n_dimensions )  EQ 1 ) && $
              ( size( data, /type ) EQ 1 ) ) then $
              data = string( data )

;Extract each variable from the "data" structure and name it
;  the corresponding "name" from "result.name":
            if Execute(result.name + ' = data') eq 0 then $
            Print, ' "Execute" command failed -- are you in Virtual Machine mode?'

;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
;
;---DO NOT--- GET RID OF ANY INFINITE DATA YET
;
;         good=WHERE(finite(uttime) eq 1,ngood)	; all data
;         IF NGOOD GT 0 THEN BEGIN
;    
; retain all 4.2 orbit data
;
;            if iorbit eq 0L then begin
;               CLOUD_INDEX_ALL20=cloud_index(good)
;               QUALITY_FLAGS_ALL20=QUALITY_FLAGS(good)
;               ALB_ALL20=alb(good)
;               ALB_ERR_ALL20=alb_err(good)
;               SZA_ALL20=sza(good)
;               IWC_ALL20=iwc(good)
;               IWC_ERR_ALL20=iwc_err(good)
;               RAD_ALL20=rad(good)
;               RAD_ERR_ALL20=rad_err(good)
;               LAT_ALL20=latitude(good)
;               LON_ALL20=longitude(good)
;               LATORIG_ALL20=latitude_orig(good)
;               LONORIG_ALL20=longitude_orig(good)
;               UT_TIME_ALL20=uttime(good)
;               UT_DATE_ALL20=utdate(good)
;            endif
;            if iorbit gt 0L then begin
;               CLOUD_INDEX_ALL20=[CLOUD_INDEX_ALL20,cloud_index(good)]
;               QUALITY_FLAGS_ALL20=[QUALITY_FLAGS_ALL20,QUALITY_FLAGS(good)]
;               ALB_ALL20=[ALB_ALL20,alb(good)]
;               ALB_ERR_ALL20=[ALB_ERR_ALL20,alb_err(good)]
;               SZA_ALL20=[SZA_ALL20,sza(good)]
;               IWC_ALL20=[IWC_ALL20,iwc(good)]
;               IWC_ERR_ALL20=[IWC_ERR_ALL20,iwc_err(good)]
;               RAD_ALL20=[RAD_ALL20,rad(good)]
;               RAD_ERR_ALL20=[RAD_ERR_ALL20,rad_err(good)]
;               LAT_ALL20=[LAT_ALL20,latitude(good)]
;               LON_ALL20=[LON_ALL20,longitude(good)]
;               LATORIG_ALL20=[LATORIG_ALL20,latitude_orig(good)]
;               LONORIG_ALL20=[LONORIG_ALL20,longitude_orig(good)]
;               UT_TIME_ALL20=[UT_TIME_ALL20,uttime(good)]
;               UT_DATE_ALL20=[UT_DATE_ALL20,utdate(good)]
;            endif
;            print,'v4.20 min/max lat ',min(latitude(good)),max(latitude(good)),ngood,' points'
;         ENDIF
;
; read v5.10 catalogue file
;
          FNAME=FNAMESCAT51(iorbit)
          print,fname
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for idim=0,result.ndims-1 do begin
              ncdf_diminq,ncid,idim,name,data
              print,idim,' ',name,size( data, /n_dimensions ),size( data, /type )

              if ( ( size( data, /n_dimensions )  EQ 1 ) && $
                 ( size( data, /type ) EQ 1 ) ) then $
                 data = string( data )

help,data
             if Execute(name + ' = data') eq 0 then $
             Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            

          endfor
stop
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data

            if ( ( size( data, /n_dimensions )  EQ 1 ) && $
              ( size( data, /type ) EQ 1 ) ) then $
              data = string( data )

;Extract each variable from the "data" structure and name it
;  the corresponding "name" from "result.name":
            if Execute(result.name + ' = data') eq 0 then $
            Print, ' "Execute" command failed -- are you in Virtual Machine mode?'

;             if result.name eq 'AIM_ORBIT_NUMBER' then AIM_ORBIT_NUMBER=data           ; short AIM_ORBIT_NUMBER(structure_elements) ;
;             if result.name eq 'VERSION' then VERSION=data                             ; char VERSION(structure_elements, string) ;
;             if result.name eq 'REVISION' then REVISION=data                           ; char REVISION(structure_elements, string) ;
;             if result.name eq 'PRODUCT_CREATION_TIME' then PRODUCT_CREATION_TIME=data ; char PRODUCT_CREATION_TIME(structure_elements, string) ;
;             if result.name eq 'DEPENDENT_1B_VERSION' then DEPENDENT1BVERSION=data	; char DEPENDENT_1B_VERSION(structure_elements, string) ;
;             if result.name eq 'UT_DATE' then UTDATE=data				; int UT_DATE(structure_elements) ;
;             if result.name eq 'UT_TIME' then UTTIME=data				; double UT_TIME(structure_elements) ;
;             if result.name eq 'HEMISPHERE' then HEMISPHERE=data                       ; char HEMISPHERE(structure_elements, string) ;
;             if result.name eq 'ORBIT_START_TIME' then ORBIT_START_TIME=data           ; double ORBIT_START_TIME(structure_elements) ;
;             if result.name eq 'ORBIT_START_TIME_UT' then ORBIT_START_TIME_UT=data     ; double ORBIT_START_TIME_UT(structure_elements) ;
;             if result.name eq 'ORBIT_END_TIME' then ORBIT_END_TIME=data               ; double ORBIT_END_TIME(structure_elements) ;
;             if result.name eq 'STACK_ID' then STACK_ID=data                           ; short STACK_ID(structure_elements) ;
;             if result.name eq 'XDIM' then XDIM=data                                   ; int XDIM(structure_elements) ;
;             if result.name eq 'YDIM' then YDIM=data                                   ; int YDIM(structure_elements) ;
;             if result.name eq 'NLAYERS' then NLAYERS=data                             ; short NLAYERS(structure_elements, dim2_NLAYERS, dim1_NLAYERS) ;
;             if result.name eq 'RATALL' then RATALL=data                               ; float RATALL(structure_elements, dim2_RATALL, dim1_RATALL) ;
;             if result.name eq 'QUALITY_FLAGS' then QUALITY_FLAGS=data                 ; int QUALITY_FLAGS(structure_elements) ;
;             if result.name eq 'KM_PER_PIXEL' then KM_PER_PIXEL=data                   ; float KM_PER_PIXEL(structure_elements) ;
;             if result.name eq 'BBOX' then BBOX=data                                   ; int BBOX(structure_elements, dim1_BBOX) ;
;             if result.name eq 'CENTER_LON' then CENTER_LON=data                       ; double CENTER_LON(structure_elements) ;
;             if result.name eq 'LATITUDE' then LATITUDE=data                           ; float LATITUDE(structure_elements, dim2_LATITUDE, dim1_LATITUDE) ;
;             if result.name eq 'LONGITUDE' then LONGITUDE=data                         ; float LONGITUDE(structure_elements, dim2_LONGITUDE, dim1_LONGITUDE) ;
;             if result.name eq 'ZENITH_ANGLE_RAY_PEAK' then SZA=data			; float ZENITH_ANGLE_RAY_PEAK(structure_elements, dim2_ZENITH_ANGLE_RAY_PEAK, dim1_ZENITH_ANGLE_RAY_PEAK) ;
;             if result.name eq 'COMMON_VOLUME_MAP' then COMMON_VOLUME_MAP=data         ; byte COMMON_VOLUME_MAP(structure_elements, dim2_COMMON_VOLUME_MAP, dim1_COMMON_VOLUME_MAP) ;
;             if result.name eq 'NOTES' then NOTES=data                                 ; char NOTES(structure_elements, string) ;
              print,'read variable ',result.name
          endfor
          ncdf_close,ncid
stop


          sorbit=strcompress(AIM_ORBIT_NUMBER,/remove_all)
          latitude_orig=latitude                                ;save original lats to determine asc/desc
          longitude_orig=longitude                              ;save original lons to determine asc/desc
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)           ;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)   ;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
;
; read 5.1 cld file
;
          FNAME=FNAMES51(iorbit)
          print,fname
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          for idim=0,result.ndims-1 do begin
              ncdf_diminq,ncid,idim,name,dim
              if name eq 'dim1_CLOUD_PRESENCE_MAP' then dim1_CLOUD_PRESENCE_MAP=dim     ; dim1_CLOUD_PRESENCE_MAP = 1056 ;
              if name eq 'dim2_CLOUD_PRESENCE_MAP' then dim2_CLOUD_PRESENCE_MAP=dim     ; dim1_CLOUD_PRESENCE_MAP = 264 ;
;             print,'read ',name,' dimension ',dim
          endfor
          for ivar=0,result.nvars-1 do begin
              result=ncdf_varinq(ncid,ivar)
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if result.name eq 'PERCENT_CLOUDS' then PERCENT_CLOUDS=data               ; float PERCENT_CLOUDS ;
              if result.name eq 'CLOUD_PRESENCE_MAP' then CLOUD_INDEX=data		; short CLOUD_PRESENCE_MAP(structure_elements, dim1_CLOUD_PRESENCE_MAP) ;
              if result.name eq 'CLD_ALBEDO' then ALB=data				; float CLD_ALBEDO(structure_elements, dim1_CLD_ALBEDO) ;
              if result.name eq 'CLD_ALBEDO_UNC' then ALB_ERR=data			; float CLD_ALBEDO_UNC(structure_elements, dim1_CLD_ALBEDO_UNC) ;
              if result.name eq 'PARTICLE_RADIUS' then RAD=data				; float PARTICLE_RADIUS(structure_elements, dim1_PARTICLE_RADIUS) ;
              if result.name eq 'PARTICLE_RADIUS_UNC' then RAD_ERR=data			; float PARTICLE_RADIUS_UNC(structure_elements, dim1_PARTICLE_RADIUS_UNC) ;
              if result.name eq 'ICE_WATER_CONTENT' then IWC=data			; float ICE_WATER_CONTENT(structure_elements, dim1_ICE_WATER_CONTENT) ;
              if result.name eq 'ICE_WATER_CONTENT_UNC' then IWC_ERR=data		; float ICE_WATER_CONTENT_UNC(structure_elements, dim1_ICE_WATER_CONTENT_UNC) ;
              if result.name eq 'ICE_COLUMN_DENSITY' then ICE_COLUMN_DENSITY=data       ; float ICE_COLUMN_DENSITY(structure_elements, dim1_ICE_COLUMN_DENSITY) ;
              if result.name eq 'CHI_SQ' then CHI_SQ=data                               ; string chi-squared value calculated from the best fit to the measured phase function used to derived cloud albedo and particle size
;             print,'read variable ',result.name
          endfor
          ncdf_close,ncid
;
;---DO NOT--- GET RID OF ANY INFINITE DATA YET
;
          good=WHERE(finite(uttime) eq 1,ngood)
          IF NGOOD GT 0 THEN BEGIN
;
; retain 5.1 orbit data
;
             if iorbit eq 0L then begin
                CLOUD_INDEX_ALL51=cloud_index(good)
                QUALITY_FLAGS_ALL51=QUALITY_FLAGS(good)
                ALB_ALL51=alb(good)
                ALB_ERR_ALL51=alb_err(good)
                SZA_ALL51=sza(good)
                IWC_ALL51=iwc(good)
                IWC_ERR_ALL51=iwc_err(good)
                RAD_ALL51=rad(good)
                RAD_ERR_ALL51=rad_err(good)
                LAT_ALL51=latitude(good)
                LON_ALL51=longitude(good)
                LATORIG_ALL51=latitude_orig(good)
                LONORIG_ALL51=longitude_orig(good)
                UT_TIME_ALL51=uttime(good)
                UT_DATE_ALL51=utdate(good)
             endif
             if iorbit gt 0L then begin
                CLOUD_INDEX_ALL51=[CLOUD_INDEX_ALL51,cloud_index(good)]
                QUALITY_FLAGS_ALL51=[QUALITY_FLAGS_ALL51,QUALITY_FLAGS(good)]
                ALB_ALL51=[ALB_ALL51,alb(good)]
                ALB_ERR_ALL51=[ALB_ERR_ALL51,alb_err(good)]
                SZA_ALL51=[SZA_ALL51,sza(good)]
                IWC_ALL51=[IWC_ALL51,iwc(good)]
                IWC_ERR_ALL51=[IWC_ERR_ALL51,iwc_err(good)]
                RAD_ALL51=[RAD_ALL51,rad(good)]
                RAD_ERR_ALL51=[RAD_ERR_ALL51,rad_err(good)]
                LAT_ALL51=[LAT_ALL51,latitude(good)]
                LON_ALL51=[LON_ALL51,longitude(good)]
                LATORIG_ALL51=[LATORIG_ALL51,latitude_orig(good)]
                LONORIG_ALL51=[LONORIG_ALL51,longitude_orig(good)]
                UT_TIME_ALL51=[UT_TIME_ALL51,uttime(good)]
                UT_DATE_ALL51=[UT_DATE_ALL51,utdate(good)]
             endif

;print,'UT time ',min(uttime(good)),max(uttime(good)),ngood,' points'
print,'v5.10 min/max lat ',min(latitude(good)),max(latitude(good)),ngood,' points'
          ENDIF

      endfor  ; loop over orbits
;
; hemispheric cloud frequency
;
      good=where(CLOUD_INDEX_ALL20 eq 1. and ALB_ALL20 gt ALBLIM and RAD_ALL20 gt 20. and SZA_ALL20 GT SZALIM_LO and SZA_ALL20 lt SZALIM_HI,ngood)
      freq20(icount)=100.*float(ngood)/float(n_elements(CLOUD_INDEX_ALL20))
      good=where(CLOUD_INDEX_ALL51 eq 1. and ALB_ALL51 gt ALBLIM and RAD_ALL51 gt 20. and SZA_ALL51 GT SZALIM_LO and SZA_ALL51 lt SZALIM_HI,ngood)
      freq51(icount)=100.*float(ngood)/float(n_elements(CLOUD_INDEX_ALL51))
;
; make arrays 1-d AND FILTER NON-CLOUD DATA based on http://lasp.colorado.edu/aim/cips/data/repository/docs/cips_level2.pdf
;
      good=WHERE(FINITE(ALB_ALL20) EQ 1 and ALB_ALL20 gt ALBLIM and RAD_ALL20 gt 20. and SZA_ALL20 gt SZALIM_LO and SZA_ALL20 lt SZALIM_HI and CLOUD_INDEX_ALL20 EQ 1,ngood)
      if good(0) eq -1L then goto,skipcips
      cloud_index20=cloud_index_all20(good)
      alb20=alb_all20(good)
      ALB_ERR20=alb_err_all20(good)
      SZA20=sza_all20(good)
      IWC20=iwc_all20(good)
      IWC_ERR20=iwc_err_all20(good)
      RAD20=rad_all20(good)
      RAD_ERR20=rad_err_all20(good)
      clat20=lat_all20(good)
      clon20=lon_all20(good)
      olat20=latorig_all20(good)
      olon20=lonorig_all20(good)
      cuttime20=ut_time_all20(good)
      cutdate20=ut_date_all20(good)

      good=WHERE(FINITE(ALB_ALL51) EQ 1 and ALB_ALL51 gt ALBLIM and RAD_ALL51 gt 20. and SZA_ALL51 gt SZALIM_LO and SZA_ALL51 lt SZALIM_HI and CLOUD_INDEX_ALL51 eq 1,ngood) 
      if good(0) eq -1L then goto,skipcips
      cloud_index51=cloud_index_all51(good)
      alb51=alb_all51(good)
      ALB_ERR51=alb_err_all51(good)
      SZA51=sza_all51(good)
      IWC51=iwc_all51(good)
      IWC_ERR51=iwc_err_all51(good)
      RAD51=rad_all51(good)
      RAD_ERR51=rad_err_all51(good)
      clat51=lat_all51(good)
      clon51=lon_all51(good)
      olat51=latorig_all51(good)
      olon51=lonorig_all51(good)
      cuttime51=ut_time_all51(good)
      cutdate51=ut_date_all51(good)
;
; Compute LOCAL TIME and LOCAL DAY from UT time and longitude
;
      lday20=iday+0L*indgen(ngood)
      MKLTIME,CUTTIME20,CLON20,LTIME20,lday20
      clttime20=ltime20
      clday20=lday20
      print,'v4.20 CLTTIME ',min(clttime20),max(clttime20)
      print,'v4.20 CLDAY ',min(clday20),max(clday20)
      lday51=iday+0L*indgen(ngood)
      MKLTIME,CUTTIME51,CLON51,LTIME51,lday51
      clttime51=ltime51
      clday51=lday51
      print,'v5.1 CLTTIME ',min(clttime51),max(clttime51)
      print,'v5.1 CLDAY ',min(clday51),max(clday51)

;erase
;plot,cuttime20,clon20,psym=1,color=0,xrange=[0.,24.]
;oplot,clttime51,clon51,psym=1,color=.3*mcolor
;
; postscript file
; 
      if setplot eq 'ps' then begin
         lc=0
         set_plot,'ps'
         xsize=nxdim/100.
         ysize=nydim/100.
         !p.font=0
         device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                /bold,/color,bits_per_pixel=8,/helvetica,filename='Figures_Level2_v4.2_v5.1/compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps'
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
      plot,sza20,alb20,psym=1,color=0,ytitle='ALB',xtitle='SZA',xrange=[SZALIM_LO,SZALIM_HI],yrange=[0.,100.],title=sdate+' (v4.2 black, v5.1 color)',charsize=1.5,charthick=2
      nbin=60
      szabin=40.+findgen(nbin)
      dbin=szabin(1)-szabin(0)
      medalb20=fltarr(nbin)
      sigalb20=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza20 gt szabin(ibin)-dbin/2. and sza20 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then medalb20(ibin)=median(alb20(index))
          if n_elements(index) ge 3L then sigalb20(ibin)=stdev(alb20(index))
      endfor
      index=where(rad51 gt 20. and rad51 le 40.)
      if index(0) ne -1L then oplot,sza51(index),alb51(index),psym=3,color=.3*mcolor
      index=where(rad51 gt 40. and rad51 le 60.)
      if index(0) ne -1L then oplot,sza51(index),alb51(index),psym=3,color=.5*mcolor
      index=where(rad51 gt 60. and rad51 le 80.)
      if index(0) ne -1L then oplot,sza51(index),alb51(index),psym=3,color=.75*mcolor
      index=where(rad51 gt 80.)
      if index(0) ne -1L then oplot,sza51(index),alb51(index),psym=3,color=.9*mcolor
      xyouts,szalim_hi+1,90,'20-40nm',color=.3*mcolor,/data
      xyouts,szalim_hi+1,80,'40-60nm',color=.5*mcolor,/data
      xyouts,szalim_hi+1,70,'60-80nm',color=.75*mcolor,/data
      xyouts,szalim_hi+1,60,'>80nm',color=.9*mcolor,/data
      xyouts,44.,90,'N='+strcompress(long(n_elements(alb20)),/r),color=0,/data
      xyouts,44.,80,'N='+strcompress(long(n_elements(alb51)),/r),color=250,/data

      medalb51=fltarr(nbin)
      sigalb51=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza51 gt szabin(ibin)-dbin/2. and sza51 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then medalb51(ibin)=median(alb51(index))
          if n_elements(index) ge 3L then sigalb51(ibin)=stdev(alb51(index))
      endfor
      loadct,0
      oplot,szabin,medalb20,color=100,thick=15
      oplot,szabin,medalb20-sigalb20,color=100,thick=8
      oplot,szabin,medalb20+sigalb20,color=100,thick=8
      oplot,szabin,medalb51,color=200,thick=12
      oplot,szabin,medalb51-sigalb51,color=200,thick=8
      oplot,szabin,medalb51+sigalb51,color=200,thick=8
      loadct,39
;
; IWC
;
      xmn=xorig(1)
      xmx=xorig(1)+xlen
      ymn=yorig(1)
      ymx=yorig(1)+ylen
      set_viewport,xmn,xmx,ymn,ymx
      !type=2^2+2^3
      plot,sza20,iwc20,psym=1,color=0,ytitle='IWC',xtitle='SZA',xrange=[SZALIM_LO,SZALIM_HI],yrange=[0.,1000.],charsize=1.5,charthick=2
      mediwc20=fltarr(nbin)
      sigiwc20=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza20 gt szabin(ibin)-dbin/2. and sza20 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then mediwc20(ibin)=median(iwc20(index))
          if n_elements(index) ge 3L then sigiwc20(ibin)=stdev(iwc20(index))
      endfor
      index=where(rad51 gt 20. and rad51 le 40.)
      if index(0) ne -1L then oplot,sza51(index),iwc51(index),psym=3,color=.3*mcolor
      index=where(rad51 gt 40. and rad51 le 60.)
      if index(0) ne -1L then oplot,sza51(index),iwc51(index),psym=3,color=.5*mcolor
      index=where(rad51 gt 60. and rad51 le 80.)
      if index(0) ne -1L then oplot,sza51(index),iwc51(index),psym=3,color=.75*mcolor
      index=where(rad51 gt 80.)
      if index(0) ne -1L then oplot,sza51(index),iwc51(index),psym=3,color=.9*mcolor
      xyouts,szalim_hi+1,900,'20-40nm',color=.3*mcolor,/data
      xyouts,szalim_hi+1,800,'40-60nm',color=.5*mcolor,/data
      xyouts,szalim_hi+1,700,'60-80nm',color=.75*mcolor,/data
      xyouts,szalim_hi+1,600,'>80nm',color=.9*mcolor,/data

      mediwc51=fltarr(nbin)
      sigiwc51=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza51 gt szabin(ibin)-dbin/2. and sza51 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then mediwc51(ibin)=median(iwc51(index))
          if n_elements(index) ge 3L then sigiwc51(ibin)=stdev(iwc51(index))
      endfor
      loadct,0
      oplot,szabin,mediwc20,color=100,thick=15
      oplot,szabin,mediwc20-sigiwc20,color=100,thick=8
      oplot,szabin,mediwc20+sigiwc20,color=100,thick=8
      oplot,szabin,mediwc51,color=200,thick=12
      oplot,szabin,mediwc51-sigiwc51,color=200,thick=8
      oplot,szabin,mediwc51+sigiwc51,color=200,thick=8
      loadct,39
;
; RAD
;
      xmn=xorig(2)
      xmx=xorig(2)+xlen
      ymn=yorig(2)
      ymx=yorig(2)+ylen
      set_viewport,xmn,xmx,ymn,ymx
      !type=2^2+2^3
      plot,sza20,rad20,psym=1,color=0,ytitle='RAD',xtitle='SZA',xrange=[SZALIM_LO,SZALIM_HI],yrange=[20.,100.],charsize=1.5,charthick=2
      medrad20=fltarr(nbin)
      sigrad20=fltarr(nbin)
      num20=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza20 gt szabin(ibin)-dbin/2. and sza20 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then medrad20(ibin)=median(rad20(index))
          if index(0) ne -1L then num20(ibin)=n_elements(index)
          if n_elements(index) ge 3L then sigrad20(ibin)=stdev(rad20(index))
      endfor
      index=where(alb51 gt ALBLIM and alb51 le 25.)
      if index(0) ne -1L then oplot,sza51(index),rad51(index),psym=3,color=.3*mcolor
      index=where(alb51 gt 25. and alb51 le 50.)
      if index(0) ne -1L then oplot,sza51(index),rad51(index),psym=3,color=.5*mcolor
      index=where(alb51 gt 50. and rad51 le 75.)
      if index(0) ne -1L then oplot,sza51(index),rad51(index),psym=3,color=.75*mcolor
      index=where(alb51 gt 75.)
      if index(0) ne -1L then oplot,sza51(index),rad51(index),psym=3,color=.9*mcolor
      xyouts,szalim_hi+1,90,'2-25G',color=.3*mcolor,/data
      xyouts,szalim_hi+1,80,'25-50G',color=.5*mcolor,/data
      xyouts,szalim_hi+1,70,'50-75G',color=.75*mcolor,/data
      xyouts,szalim_hi+1,60,'>75G',color=.9*mcolor,/data

      medrad51=fltarr(nbin)
      sigrad51=fltarr(nbin)
      num51=fltarr(nbin)
      for ibin=0L,nbin-1L do begin
          index=where(sza51 gt szabin(ibin)-dbin/2. and sza51 le szabin(ibin)+dbin/2.)
          if index(0) ne -1L then medrad51(ibin)=median(rad51(index))
          if index(0) ne -1L then num51(ibin)=n_elements(index)
          if n_elements(index) ge 3L then sigrad51(ibin)=stdev(rad51(index))
      endfor
      loadct,0
      oplot,szabin,medrad20,color=100,thick=15
      oplot,szabin,medrad20-sigrad20,color=100,thick=8
      oplot,szabin,medrad20+sigrad20,color=100,thick=8
      oplot,szabin,medrad51,color=200,thick=12
      oplot,szabin,medrad51-sigrad51,color=200,thick=8
      oplot,szabin,medrad51+sigrad51,color=200,thick=8
      loadct,39
;
; save medians as a function of SZA
;
      if kcount eq 0L then begin
         medalb20_all=fltarr(kday,nbin)
         medalb51_all=fltarr(kday,nbin)
         sigalb20_all=fltarr(kday,nbin)
         sigalb51_all=fltarr(kday,nbin)

         mediwc20_all=fltarr(kday,nbin)
         mediwc51_all=fltarr(kday,nbin)
         sigiwc20_all=fltarr(kday,nbin)
         sigiwc51_all=fltarr(kday,nbin)

         medrad20_all=fltarr(kday,nbin)
         medrad51_all=fltarr(kday,nbin)
         sigrad20_all=fltarr(kday,nbin)
         sigrad51_all=fltarr(kday,nbin)

         num20_all=fltarr(kday,nbin)
         num51_all=fltarr(kday,nbin)
         kcount=1
      endif
      medalb20_all(icount,*)=medalb20
      medalb51_all(icount,*)=medalb51
      sigalb20_all(icount,*)=sigalb20
      sigalb51_all(icount,*)=sigalb51

      mediwc20_all(icount,*)=mediwc20
      mediwc51_all(icount,*)=mediwc51
      sigiwc20_all(icount,*)=sigiwc20
      sigiwc51_all(icount,*)=sigiwc51

      medrad20_all(icount,*)=medrad20
      medrad51_all(icount,*)=medrad51
      sigrad20_all(icount,*)=sigrad20
      sigrad51_all(icount,*)=sigrad51

      num20_all(icount,*)=num20
      num51_all(icount,*)=num51

    if setplot ne 'ps' then stop
      if setplot eq 'ps' then begin
         device, /close
         spawn,'convert -trim Figures_Level2_v4.2_v5.1/compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps -rotate -90 Figures_Level2_v4.2_v5.1/compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.jpg'
         spawn,'rm -f Figures_Level2_v4.2_v5.1/compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps'
      endif
      skipcips:
      icount=icount+1L
goto,jump

plotyear:
;save,filename='cips_l2_medians_vs_sza_north_'+syear+'.sav',SZALIM_HI,SZALIM_LO,kday,sdate_all,dfs_all,freq20,freq51,szabin,medalb20_all,medalb51_all,sigalb20_all,sigalb51_all,$
;                                                           mediwc20_all,mediwc51_all,sigiwc20_all,sigiwc51_all,medrad20_all,medrad51_all,sigrad20_all,sigrad51_all,num20_all,num51_all
quick:
restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav

xorig=[.15,.55,.15,.55,.15,.55]
yorig=[.7,.7,.4,.4,.1,.1]
erase
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='time-sza-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

npts=100.
index=where(num20_all lt npts)
if index(0) ne -1L then begin
   medalb20_all(index)=0./0.
   mediwc20_all(index)=0./0.
   medrad20_all(index)=0./0.
endif
index=where(num51_all lt npts)
if index(0) ne -1L then begin
   medalb51_all(index)=0./0.
   mediwc51_all(index)=0./0.
   medrad51_all(index)=0./0.
endif

nlvls=20
col1=1+indgen(nlvls)*mcolor/nlvls
!type=2^2+2^3
level=2.5+2.5*findgen(20)
xlen=0.3
ylen=0.2
set_viewport,xorig(0),xorig(0)+xlen,yorig(0),yorig(0)+ylen
contour,medalb20_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN ALB ver4.2',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3
set_viewport,xorig(1),xorig(1)+xlen,yorig(1),yorig(1)+ylen
contour,medalb51_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN ALB ver5.1',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3
xmnb=xorig(1)+xlen+0.07
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(1),yorig(1)+ylen
!type=2^2+2^3+2^5
omin=min(level)
omax=max(level)
plot,[0,0],[omin,omax],xrange=[0,10],yrange=[omin,omax],color=0,charthick=2,charsize=1.5,title='G',/noeras
xbox=[0,10,10,0,0]
y1=omin
dy=(omax-omin)/float(nlvls)
for j=0,nlvls-1 do begin 
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor

!type=2^2+2^3
level=10+10*findgen(20)
set_viewport,xorig(2),xorig(2)+xlen,yorig(2),yorig(2)+ylen
contour,mediwc20_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN IWC ver4.2',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3
set_viewport,xorig(3),xorig(3)+xlen,yorig(3),yorig(3)+ylen
contour,mediwc51_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN IWC ver5.1',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3
xmnb=xorig(3)+xlen+0.07
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(3),yorig(3)+ylen
!type=2^2+2^3+2^5
omin=min(level)
omax=max(level)
plot,[0,0],[omin,omax],xrange=[0,10],yrange=[omin,omax],color=0,charthick=2,charsize=1.5,title='ug m!u-3!n',/noeras
xbox=[0,10,10,0,0]
y1=omin
dy=(omax-omin)/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor

!type=2^2+2^3
level=20+2.5*findgen(20)
set_viewport,xorig(4),xorig(4)+xlen,yorig(4),yorig(4)+ylen
contour,medrad20_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN RAD ver4.2',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3,xtitle=syear+' DFS'
set_viewport,xorig(5),xorig(5)+xlen,yorig(5),yorig(5)+ylen
contour,medrad51_all,dfs_all,szabin,color=0,ytitle='SZA',xrange=[min(dfs_all),max(dfs_all)],yrange=[SZALIM_LO,SZALIM_HI],title='MEDIAN RAD ver5.1',charsize=1.25,charthick=2,levels=level,/noeras,c_color=col1,thick=3,xtitle=syear+' DFS'

xmnb=xorig(5)+xlen+0.07
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(5),yorig(5)+ylen
!type=2^2+2^3+2^5
omin=min(level)
omax=max(level)
plot,[0,0],[omin,omax],xrange=[0,10],yrange=[omin,omax],color=0,charthick=2,charsize=1.5,title='nm',/noeras
xbox=[0,10,10,0,0]
y1=omin
dy=(omax-omin)/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim time-sza-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps -rotate -90 time-sza-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.jpg'
;      spawn,'rm -f time-sza-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps'
    endif
endfor	; loop over years
end
