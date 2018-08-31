;
; save coincidences between CIPS and MLS. dxc and dtc are space and time criteria.
; find all coincidences with a single MLS profile. 
; 	1) save total number of CIPS points regardless of albedo value. require 50 < sza < 91
; then, points with 50 < sza < 91 and albedo > 5 Garys and albedo ne NaN save:
; 	2) CIPS lon,lat,uttime,ltime,utdate,lday,dist,dt,sza,alb,rad,iwc,nclouds (where cloud_presence=1)
;	3) MLS lon,lat,uttime,ltime. Profiles of temperature and water vapor.
; lon(mprof,ncoinmax)
;
; then save medians and sigmas of the CIPS clusters
;
; CIPS level 4 data
; find coincidences between MLS and CIPS
; save daily files
; VLH 8/4/2009
;
@stddat
@kgmt
@ckday
@kdate
@mkltime
@range_ring
@ks_stats

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
xorig=[0.15,0.6,0.15,0.6]
yorig=[0.55,0.55,0.125,0.125]
xlen=0.3
ylen=0.3
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
;mdir='/aura6/data/MLS_data/Datfiles_SOSST/'
mdir='/ptmp/harvey/MLS_data/Datfiles_SOSST/'
;
; restore CIPS procedures and functions
;
restore,'read_cips_file.sav
;pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_4_orbit_'
pth='/ptmp/harvey/CIPS_data/Datfiles/cips_sci_4_orbit_'
pthout='/ptmp/harvey/CIPS_data/Datfiles_Save/'

lstmn=5
lstdy=1
lstyr=2007
ledmn=9
leddy=1
ledyr=2009
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
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount=0L
;
; USE THE CLOUD PRESENCE MAP ARRAY TO CALCULATE FREQUENCIES. AND USES THE CPM=1 VALUE TO GET THE ALBEDOS.
; LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
; USING LIM=-99 ESSENTIALLY INCLUDES ALL POINTS THAT ARE FOUND WITH CLOUD_PRESENCE_MAP,
; EVEN IF THE ALBEDO IS NEGATIVE (WHICH DOES HAPPEN) -- BUT THEN THE ALB/ALB_ERR TEST MIGHT CATCH IT.
LIM=1.
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO
SZALIM=91.      ;DATA WITH SZA > SZALIM ARE BAD (IN NH THIS CAN ONLY HAPPEN ON THE ASCENDING NODE)
szamin=50.
;SZALIM=180.    ;DON'T GET RID OF ANY DATA BASED ON SZA.

; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then stop,' Normal Termination Condition '
      syr=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syr+smn+sdy
;
; get nc filenames on this day (I get an error if I try to read the gzipped file)
;
      spawn,'ls '+pth+'*'+syr+'-'+sday+'*.nc',fnames
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; average albedo, IWC, radii in each latitude bin
;
;norbit=1L	; testing purposes
      npts=300000L
      CLOUD_INDEX_ALL=-99L+0.*lonarr(norbit,npts)
      ALB_ALL=-99.+0.*fltarr(norbit,npts)
      ALB_ERR_ALL=-99.+0.*fltarr(norbit,npts)
      SZA_ALL=-99.+0.*fltarr(norbit,npts)
      IWC_ALL=-99.+0.*fltarr(norbit,npts)
      IWC_ERR_ALL=-99.+0.*fltarr(norbit,npts)
      RAD_ALL=-99.+0.*fltarr(norbit,npts)
      RAD_ERR_ALL=-99.+0.*fltarr(norbit,npts)
      LAT_ALL=-99.+0.*fltarr(norbit,npts)
      LON_ALL=-99.+0.*fltarr(norbit,npts)
      UT_TIME_ALL=-99.+0.*fltarr(norbit,npts)
      UT_DATE_ALL=-99L+0L*intarr(norbit,npts)
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
          AIM_ORBIT_NUMBER=data.AIM_ORBIT_NUMBER                ;INT Cumulative mission orbit number
          VERSION=data.VERSION                                  ;STRING    '03.20'
          PRODUCT_CREATION_TIME=data.PRODUCT_CREATION_TIME      ;STRING    '2009/040-13:23:03' Version number of data product
          DEPENDANT1BVERSION=data.DEPENDANT1BVERSION            ;STRING    '03.20'
          UTDATE=DATA.UT_DATE                                  ;LONG       2009001 UTC date of this orbit
          HEMISPHERE=DATA.HEMISPHERE                            ;STRING    'S'
          ORBIT_START_TIME=data.ORBIT_START_TIME                ;DOUBLE 9.1480689e+14 Orbit start time in gps microseconds
          ORBIT_START_TIME_UT=data.ORBIT_START_TIME_UT          ;DOUBLE   2.0548680 UTC time of the start of the orbit
          ORBIT_END_TIME=data.ORBIT_END_TIME                    ;DOUBLE 9.1481268e+14 Orbit end time in gps microseconds
          STACK_ID=data.STACK_ID                                ;INT        0 uniquely identify the Level 1B data
          XDIM=data.XDIM                                        ;INT        X dimension of data. Average of 600
          YDIM=data.YDIM                                        ;INT        Y dimension of data. Average is 150
          QUALITY_FLAGS=data.QUALITY_FLAGS                      ;LONG      TBD
          X_TILE_DIM=data.X_TILE_DIM                            ;INT       Array size (columns) of tiles is x direction
          Y_TILE_DIM=data.Y_TILE_DIM                            ;INT       Array size (rows) of tiles in the y direction
          KM_PER_PIXEL=data.KM_PER_PIXEL                        ;INT              5
          BBOX=data.BBOX                                        ;LONG      Array[4] {x, y} bounding box of map projected image
          CENTER_LON=data.CENTER_LON    ;Center longitude of map proj, NOT data. Used for orienting the data horizontally.
          NLAYERS=(*data[0].nlayers)    ;# data layers corresponding to a pixel at [xDim, yDim] a diff SCA. Avg 8.
          UTTIME=(*data[0].ut_time)                            ;POINTER  Number of seconds elapsed since orbit_start_time_ut
          PERCENT_CLOUDS=data.PERCENT_CLOUDS                    ;FLOAT    Percentage of tiles that have an identified cloud
          CLOUD_INDEX=(*DATA[0].CLOUD_PRESENCE_MAP)             ;1 FOR CLOUD, 0 FOR NO CLOUD
;         COMMON_VOLUME_MAP=(*DATA[0].COMMON_VOLUME_MAP)        ;1=tile in the CVO, 0=otherwise
          LATITUDE=(*DATA[0].LATITUDE)                          ;latitude for every pixel
          LONGITUDE=(*DATA[0].LONGITUDE)                        ;longitude for every pixel
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)           ;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)   ;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
;         CLD_PHASE_ALBEDO=(*data[0].cld_phase_albedo)          ;The cloud phase albedo for each tile
;         CLD_PHASE_ALBEDO_ERR=(*data[0].cld_phase_albedo_UNC)  ;The cloud phase albedo uncertainty for each tile
          SZA = (*DATA[0].ZENITH_ANGLE_RAY_PEAK)                ;Zenith angle at the peak of the Rayleigh contribution
;         VIEW = (*DATA[0].VIEW_ANGLE_RAY_PEAK)                 ;View angle at the peak of the Rayleigh contribution
;         SCA = (*DATA[0].SCATTERING_ANGLE)                     ;Scattering angle at the peak of the Rayleigh contribution
          ALB = (*data[0].cld_albedo)                           ;Cloud albedo in Garys (10^-6 sr^-1) i.e., alb x 1.e6
          ALB_ERR = (*DATA[0].CLD_ALBEDO_UNC)                   ;1 sigma formal uncertainty of cld_albedo
          IWC = (*data[0].ICE_WATER_CONTENT)
          IWC_ERR = (*DATA[0].ICE_WATER_CONTENT_UNC)
          RAD=(*DATA[0].PARTICLE_RADIUS)                        ;Particle radius for each tile
          RAD_ERR=(*DATA[0].PARTICLE_RADIUS_UNC)                ;Particle radius uncertainty for each tile
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
;---DO NOT--- GET RID OF ANY INFINITE DATA AND ANY negative DATA.
; Retain all data prior to search for coincidences (to compute cloud frequency)
;
;         good=WHERE(FINITE(ALB) EQ 1 and ALB gt 5. and sza gt szamin and SZA le SZALIM,ngood)
          good=WHERE(finite(uttime) eq 1 and sza gt szamin and SZA le SZALIM,ngood)	; all data
          IF NGOOD GT 0 THEN BEGIN
             if ngood gt npts then stop,'increase npts'
;    
; save petals into daisy
;
             CLOUD_INDEX_ALL(iorbit,0:ngood-1L)=cloud_index(good)
             ALB_ALL(iorbit,0:ngood-1L)=alb(good)
             ALB_ERR_ALL(iorbit,0:ngood-1L)=alb_err(good)
             SZA_ALL(iorbit,0:ngood-1L)=sza(good)
             IWC_ALL(iorbit,0:ngood-1L)=iwc(good)
             IWC_ERR_ALL(iorbit,0:ngood-1L)=iwc_err(good)
             RAD_ALL(iorbit,0:ngood-1L)=rad(good)
             RAD_ERR_ALL(iorbit,0:ngood-1L)=rad_err(good)
             LAT_ALL(iorbit,0:ngood-1L)=latitude(good)
             LON_ALL(iorbit,0:ngood-1L)=longitude(good)
             UT_TIME_ALL(iorbit,0:ngood-1L)=uttime(good)
             UT_DATE_ALL(iorbit,0:ngood-1L)=utdate(good)
;print,'UT time ',min(uttime(good)),max(uttime(good)),ngood,' points'
print,'min/max lat ',min(latitude(good)),max(latitude(good)),ngood,' points'
          ENDIF
      endfor  ; loop over orbits
;
; make arrays 1-d
;
      good=where(lat_all ge -90.,ngood)
      cloud_index=cloud_index_all(good)
      alb=alb_all(good)
      ALB_ERR=alb_err_all(good)
      SZA=sza_all(good)
      IWC=iwc_all(good)
      IWC_ERR=iwc_err_all(good)
      RAD=rad_all(good)
      RAD_ERR=rad_err_all(good)
      clat=lat_all(good)
      clon=lon_all(good)
      cuttime=ut_time_all(good)
      cutdate=ut_date_all(good)
;
; clear memory
;
      ALB_ALL=0 & ALB_ERR_ALL=0 & SZA_ALL=0 & VIEW_ALL=0 & SCA_ALL=0 & IWC_ALL=0 & IWC_ERR_ALL=0
      RAD_ALL=0 & RAD_ERR_ALL=0 & LAT_ALL=0 & LON_ALL=0 & UT_TIME_ALL=0 & UT_DATE_ALL=0 & cloud_index_all=0
;
; Compute LOCAL TIME and LOCAL DAY from UT time and longitude
;
      lday=iday+0L*indgen(ngood)
      MKLTIME,CUTTIME,CLON,LTIME,lday
      clttime=ltime
      clday=lday
      print,'CLTTIME ',min(clttime),max(clttime)
      print,'CLDAY ',min(clday),max(clday)
;;loadct,39
;;erase
;;plot,cuttime,clon,psym=1,color=0,xrange=[0.,24.]
;;oplot,clttime,clon,psym=1,color=.3*mcolor
;
; plot CIPS locations colored by local time
;
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
                /bold,/color,bits_per_pixel=8,/helvetica,filename='save_coin_mls_cips_'+sdate+'.ps'
         !p.charsize=1.25
         !p.thick=2
         !p.charthick=5
         !p.charthick=5
         !y.thick=2
         !x.thick=2
      endif

      erase
      loadct,39
      xmn=xorig(0)
      xmx=xorig(0)+xlen
      ymn=yorig(0)
      ymx=yorig(0)+ylen
      set_viewport,xmn,xmx,ymn,ymx
      !type=2^2+2^3
      if min(clat) gt 0. then map_set,90,0,0,/ortho,/contin,/grid,title='CIPS',color=0,limit=[50.,0.,80.,360.],/noerase
      if min(clat) lt 0. then map_set,-90,0,0,/ortho,/contin,/grid,title='CIPS',color=0,limit=[-50.,0.,-80.,360.],/noerase
      utmin=0. & utmax=24.
;     for ii=0L,ngood-1L do $
;         oplot,[clon(ii),clon(ii)],[clat(ii),clat(ii)],psym=8,color=((clttime(ii)-utmin)/(utmax-utmin))*mcolor,symsize=0.1
      ymxb=ymn-cbaryoff
      ymnb=ymxb+cbarydel
      set_viewport,xmn,xmx,ymnb,ymxb
      imin=utmin
      imax=utmax
      !type=2^2+2^3+2^6
      plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Local Time (hours)'
      ybox=[0,10,10,0,0]
      x2=imin
      nlvls=11
      col1=1+indgen(nlvls)*mcolor/nlvls
      dx=(imax-imin)/(float(nlvls)-1)
      for j=1,nlvls-1 do begin
          xbox=[x2,x2,x2+dx,x2+dx,x2]
          polyfill,xbox,ybox,color=col1(j)
          x2=x2+dx
      endfor
;
; restore MLS data on this day
;
      dum=findfile(mdir+'cat_mls_v2.2_'+sdate+'.sav')
      if dum(0) eq '' then goto,skipmls
      restore,mdir+'cat_mls_v2.2_'+sdate+'.sav'
      restore,mdir+'h2o_mls_v2.2_'+sdate+'.sav'
      restore,mdir+'tpd_mls_v2.2_'+sdate+'.sav'
      print,sdate
;
; apply mask
;
      good=where(mix ne -99.)
      if good(0) eq -1L then goto,jump
      bad=where(mask eq -99.)
      if bad(0) ne -1L then mix(bad)=-99.
      bad=where(temperature_mask eq -99.)
      if bad(0) ne -1L then temperature(bad)=-99.
      mh2o=mix
      good=where(mh2o ne -99.)
      mh2o(good)=mh2o(good)*1.e6
      mtemp=temperature
      mpress=pressure
      mprof=n_elements(longitude)
      mlev=n_elements(altitude)
      muttime=time
      mlat=latitude
      mlon=longitude
;
; eliminate bad UT times and SH
;
      index=where(muttime gt 0. and mlat ge min(clat)-4. and mlat le max(clat)+4.,mprof)
      if index(0) eq -1L then goto,jump
      muttime=reform(muttime(index))
      mlat=reform(mlat(index))
      mlon=reform(mlon(index))
      mtemp=reform(mtemp(index,*))
      mpress=reform(mpress(index,*))
      mh2o=reform(mh2o(index,*))
      mtheta=mtemp*(1000./mpress)^0.286
      index=where(mtemp lt 0.)
      if index(0) ne -1L then mtheta(index)=-99.
;
; Compute LOCAL TIME and LOCAL DAY from UT time and longitude
;
      lday=iday+0L*indgen(mprof)
      MKLTIME,MUTTIME,MLON,LTIME,lday
      mlttime=ltime
      mlday=lday
;     print,'MLTTIME ',min(mlttime),max(mlttime)
;     print,'MLDAY ',min(mlday),max(mlday)
;;oplot,muttime,mlon,psym=1,color=.9*mcolor
;;oplot,mlttime,mlon,psym=1,color=.6*mcolor
;;stop
;
; coincident arrays
;
      ncoins=lonarr(mprof)
      nclouds=lonarr(mprof)
      xcoinmls=-99.+fltarr(mprof)
      ycoinmls=-99.+fltarr(mprof)
      utcoinmls=-99.+fltarr(mprof)
      ltcoinmls=-99.+fltarr(mprof)
      tempcoinmls=-99.+fltarr(mprof,mlev)
      h2ocoinmls=-99.+fltarr(mprof,mlev)
      
      ncoinmax=3000L & dxc=400. & dtc=2.
      sdxc=strcompress(long(dxc),/remove_all)
      sdtc=strcompress(long(dtc),/remove_all)
      ldoy=-99L+lonarr(mprof)
      xcoincips=-99.+fltarr(mprof,ncoinmax)
      ycoincips=-99.+fltarr(mprof,ncoinmax)
      utcoincips=-99.+fltarr(mprof,ncoinmax)
      ltcoincips=-99.+fltarr(mprof,ncoinmax)
      dtcoincips=-99.+fltarr(mprof,ncoinmax)
      dxcoincips=-99.+fltarr(mprof,ncoinmax)
      albcoincips=-99.+fltarr(mprof,ncoinmax)
      radcoincips=-99.+fltarr(mprof,ncoinmax)
      iwccoincips=-99.+fltarr(mprof,ncoinmax)
      szacoincips=-99.+fltarr(mprof,ncoinmax)

      xyouts,.125,.9,sdate+' Coins within '+sdxc+' km and '+sdtc+' hrs',/normal,color=0,charsize=2
      loadct,39
      xmn=xorig(1)
      xmx=xorig(1)+xlen
      ymn=yorig(1)
      ymx=yorig(1)+ylen
      set_viewport,xmn,xmx,ymn,ymx
      !type=2^2+2^3
      if min(mlat) gt 0. then map_set,90,0,0,/ortho,/contin,/grid,title='MLS',color=0,limit=[50.,0.,80.,360.],/noerase
      if min(mlat) lt 0. then map_set,-90,0,0,/ortho,/contin,/grid,title='MLS',color=0,limit=[-50.,0.,-80.,360.],/noerase
      for ii=0L,mprof-1L do $
          oplot,[mlon(ii),mlon(ii)],[mlat(ii),mlat(ii)],psym=8,color=((mlttime(ii)-utmin)/(utmax-utmin))*mcolor,symsize=0.75
;
; find coincidences
; loop over MLS profiles
; 
      loadct,0
      utdate=long(sdate)	; only need 1 UT date each day
      ncoin=0L
      for i=0,mprof-1L do begin
          lday0=mlday(i)
          ltime0=mlttime(i)
          xh=mlon(i) & yh=mlat(i)
          dxf=re*abs(xh-clon)*dtr*cos(yh*dtr)
          dyf=re*abs(yh-clat)*dtr
          dist=sqrt(dxf*dxf+dyf*dyf)
;         allindex=where(dist le dxc and clday eq lday0 and abs(clttime-ltime0) le dtc,ncoin)
          allindex=where(dist le dxc and clday eq lday0 and abs(clttime-ltime0) le dtc and sza gt szamin and SZA le SZALIM,ncoin)
          if ncoin ge ncoinmax then stop,'increase ncoinmax'
          cloudindex=WHERE(dist le dxc and clday eq lday0 and abs(clttime-ltime0) le dtc and $
                           FINITE(ALB) EQ 1 and ALB gt 5. and sza gt szamin and SZA le SZALIM and $
                           cloud_index eq 1L,ncloud)

          if allindex(0) ne -1 then ncoins(i)=ncoin
          if cloudindex(0) ne -1L then begin
             nclouds(i)=ncloud
             !linetype=0
             range_ring,yh,xh,dxc,360,bear,lats,lons
             oplot,lons,lats,color=mcolor*.3,psym=0
             print,'ncoin ncloud frac ',ncoin,ncloud,100.*float(ncloud)/float(ncoin),' x y = ',clon(cloudindex(0)),clat(cloudindex(0))
             ldoy(i)=lday0
             xcoincips(i,0:ncloud-1L)=clon(cloudindex)
             ycoincips(i,0:ncloud-1L)=clat(cloudindex)
             utcoincips(i,0:ncloud-1L)=cuttime(cloudindex)
             ltcoincips(i,0:ncloud-1L)=clttime(cloudindex)
             dxcoincips(i,0:ncloud-1L)=dist(cloudindex)
             dtcoincips(i,0:ncloud-1L)=clttime(cloudindex)-ltime0
             albcoincips(i,0:ncloud-1L)=alb(cloudindex)
             radcoincips(i,0:ncloud-1L)=rad(cloudindex)
             iwccoincips(i,0:ncloud-1L)=iwc(cloudindex)
             szacoincips(i,0:ncloud-1L)=sza(cloudindex)
             xcoinmls(i)=mlon(i)
             ycoinmls(i)=mlat(i)
             utcoinmls(i)=muttime(i)
             ltcoinmls(i)=mlttime(i)
             tempcoinmls(i,*)=mtemp(i,*)
             h2ocoinmls(i,*)=mh2o(i,*)
          endif
      endfor
;
; remove mls profiles w/ no coincidences.  include cloud and no cloud.
;
      good=where(ncoins gt 0L,mprof)
      if good(0) eq -1L then goto,jump
      if mprof gt 0L then begin
         ncoins=ncoins(good)
         nclouds=nclouds(good)
         ldoy=ldoy(good)
         xcoincips=xcoincips(good,*)
         ycoincips=ycoincips(good,*)
         utcoincips=utcoincips(good,*)
         ltcoincips=ltcoincips(good,*)
         dxcoincips=dxcoincips(good,*)
         dtcoincips=dtcoincips(good,*)
         albcoincips=albcoincips(good,*)
         radcoincips=radcoincips(good,*)
         iwccoincips=iwccoincips(good,*)
         szacoincips=szacoincips(good,*)
         xcoinmls=xcoinmls(good)
         ycoinmls=ycoinmls(good)
         utcoinmls=utcoinmls(good)
         ltcoinmls=ltcoinmls(good)
         tempcoinmls=tempcoinmls(good,*)
         h2ocoinmls=h2ocoinmls(good,*)
;
; overplot coincidences
;
;  oplot,xcoincips,ycoincips,color=0,psym=8,symsize=0.2
;  oplot,xcoinmls,ycoinmls,color=0,psym=4
      endif
;
; plot color bar
;
      loadct,39
      ymxb=ymn-cbaryoff
      ymnb=ymxb+cbarydel
      set_viewport,xmn,xmx,ymnb,ymxb
      imin=utmin
      imax=utmax
      !type=2^2+2^3+2^6
      plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Local Time (hours)'
      ybox=[0,10,10,0,0]
      x2=imin
      nlvls=11
      col1=1+indgen(nlvls)*mcolor/nlvls
      dx=(imax-imin)/(float(nlvls)-1)
      for j=1,nlvls-1 do begin
          xbox=[x2,x2,x2+dx,x2+dx,x2]
          polyfill,xbox,ybox,color=col1(j)
          x2=x2+dx
      endfor
;;
;; SAVE ALL CIPS COINCIDENCES WITH MLS ON THIS DAY
;;
      comment=strarr(3)
      comment(0)='First dimension is number of MLS profiles. Second dimension is either maximum number of coincidences allowed (for CIPS arrays) or number of altitudes (for MLS arrays).'
      comment(1)='Coincidence Criteria = '+sdxc+' km and '+sdtc+' hours'
      comment(2)='Water Vapor is in ppmv.'
      save,file=pthout+'cips_mls_coins_'+sdate+'_all.sav',comment,ncoins,nclouds,$
           xcoinmls,ycoinmls,utcoinmls,ltcoinmls,tempcoinmls,h2ocoinmls,ldoy,$
           xcoincips,ycoincips,utcoincips,ltcoincips,dtcoincips,dxcoincips,$
           albcoincips,radcoincips,iwccoincips,szacoincips,altitude,mprof,ncoinmax
;
; calculate medians and sigmas of CIPS albedo, radii, IWC, SZA for clusters of coincidences
; these arrays will be 1D with dimension mprof
;
      xcoincips_median=-99.+0.*fltarr(mprof)		; accommodate points crossing GM
      ycoincips_median=-99.+0.*fltarr(mprof)
      utcoincips_median=-99.+0.*fltarr(mprof)
      ltcoincips_median=-99.+0.*fltarr(mprof)
      dxcoincips_median=-99.+0.*fltarr(mprof)
      dtcoincips_median=-99.+0.*fltarr(mprof)
      albcoincips_median=-99.+0.*fltarr(mprof)
      radcoincips_median=-99.+0.*fltarr(mprof)
      iwccoincips_median=-99.+0.*fltarr(mprof)
      szacoincips_median=-99.+0.*fltarr(mprof)
      xcoincips_sigma=-99.+0.*fltarr(mprof)
      ycoincips_sigma=-99.+0.*fltarr(mprof)
      utcoincips_sigma=-99.+0.*fltarr(mprof)
      ltcoincips_sigma=-99.+0.*fltarr(mprof)
      dxcoincips_sigma=-99.+0.*fltarr(mprof)
      dtcoincips_sigma=-99.+0.*fltarr(mprof)
      albcoincips_sigma=-99.+0.*fltarr(mprof)
      radcoincips_sigma=-99.+0.*fltarr(mprof)
      iwccoincips_sigma=-99.+0.*fltarr(mprof)
      szacoincips_sigma=-99.+0.*fltarr(mprof)
      for iprof=0L,mprof-1L do begin

          nclouds_prof=nclouds(iprof)
          if nclouds_prof ge 3L then begin	; require 3 or more cloud points
             pts=reform(xcoincips(iprof,0:nclouds_prof-1L))
;
; logic if cluster crosses GM
;
             if max(pts) gt 360. and min(pts) lt 10. then begin
                if xcoinmls(iprof) lt 180. then begin
                   index=where(pts gt 360.)
                   pts(index)=pts(index)-360.
                endif
                if xcoinmls(iprof) gt 180. then begin
                   index=where(pts lt 10.)
                   pts(index)=pts(index)+360.
                endif
                print,'GM logic used ',xcoinmls(iprof),median(pts)
           endif
           xcoincips_median(iprof)=abs(median(pts))
           xcoincips_sigma(iprof)=stdev(pts)
           pts=reform(ycoincips(iprof,0:nclouds_prof-1L))
           ycoincips_median(iprof)=median(pts)
           ycoincips_sigma(iprof)=stdev(pts)
           pts=reform(utcoincips(iprof,0:nclouds_prof-1L))
           utcoincips_median(iprof)=median(pts)
           utcoincips_sigma(iprof)=stdev(pts)
           pts=reform(ltcoincips(iprof,0:nclouds_prof-1L))
           ltcoincips_median(iprof)=median(pts)
           ltcoincips_sigma(iprof)=stdev(pts)
           pts=reform(dxcoincips(iprof,0:nclouds_prof-1L))
           dxcoincips_median(iprof)=median(pts)
           dxcoincips_sigma(iprof)=stdev(pts)
           pts=reform(dtcoincips(iprof,0:nclouds_prof-1L))
           dtcoincips_median(iprof)=median(pts)
           dtcoincips_sigma(iprof)=stdev(pts)
           pts=reform(albcoincips(iprof,0:nclouds_prof-1L))
           albcoincips_median(iprof)=median(pts)
           albcoincips_sigma(iprof)=stdev(pts)
           pts=reform(radcoincips(iprof,0:nclouds_prof-1L))
           radcoincips_median(iprof)=median(pts)
           radcoincips_sigma(iprof)=stdev(pts)
           pts=reform(iwccoincips(iprof,0:nclouds_prof-1L))
           iwccoincips_median(iprof)=median(pts)
           iwccoincips_sigma(iprof)=stdev(pts)
           pts=reform(szacoincips(iprof,0:nclouds_prof-1L))
           szacoincips_median(iprof)=median(pts)
           szacoincips_sigma(iprof)=stdev(pts)
        endif
    endfor
;
; SAVE MEDIAN and SIGMA of clusters of CIPS CLOUD COINCIDENCES WITH MLS ON THIS DAY
;
    comment=strarr(3)
    comment(0)='First dimension is number of MLS profiles. Second dimension is either maximum number of coincidences allowed (for CIPS arrays) or number of altitudes (for MLS arrays).'
    comment(1)='Coincidence Criteria = '+sdxc+' km and '+sdtc+' hours'
    comment(2)='Water Vapor is in ppmv.'
    save,file=pthout+'cips_mls_coins_'+sdate+'_median.sav',comment,ncoins,nclouds,$
         xcoinmls,ycoinmls,utcoinmls,ltcoinmls,tempcoinmls,h2ocoinmls,ldoy,$
         xcoincips_median,ycoincips_median,utcoincips_median,ltcoincips_median,$
         dtcoincips_median,dxcoincips_median,albcoincips_median,radcoincips_median,$
         iwccoincips_median,szacoincips_median,xcoincips_sigma,ycoincips_sigma,$
         utcoincips_sigma,ltcoincips_sigma,dtcoincips_sigma,dxcoincips_sigma,$
         albcoincips_sigma,radcoincips_sigma,iwccoincips_sigma,szacoincips_sigma,$
         altitude,mprof,ncoinmax
;
; free up memory
;
    xcoincips=0 & ycoincips=0 & utcoincips=0 & ltcoincips=0 & dtcoincips=0
    dxcoincips=0 & albcoincips=0 & radcoincips=0 & iwccoincips=0 & szacoincips=0
;
; plot scatter plots and PDFs
;
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    !linetype=0
    nlvls=13
    x=30.*findgen(nlvls)
    plot,x,x,xrange=[0.,360.],yrange=[0.,360.],charsize=1.2,$
         ytitle='MLS Longitude',xtitle='CIPS Median Longitude',title='Scatter Plot',/noerase,color=0
;
; zero values where nclouds was lt 3
;
    good=where(nclouds ge 3L,mprof)
    if mprof lt 3L then goto,skippdf
    xcoincips=xcoincips_median(good)
    ycoincips=ycoincips_median(good)
    xcoinmls=xcoinmls(good)
    result=correlate(xcoincips,xcoinmls)
    r=result(0)
    xyouts,20.,330.,'N ='+strcompress(string(mprof)),/data,charsize=1.2,color=0
    xyouts,20.,300.,'r = '+strcompress(string(format='(f6.3)',r)),/data,charsize=1.2,color=0
    thmax=80. & thmin=60.
    for icoin=0L,mprof-1L do begin
       xx=xcoincips(icoin)
       yy=xcoinmls(icoin)
       oplot,[xx,xx],[yy,yy],psym=8,color=((ycoincips(icoin)-thmin)/(thmax-thmin))*icolmax,symsize=0.8
    endfor
    ymxb=ymn-cbaryoff-0.06
    ymnb=ymxb+cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    imin=thmin
    imax=thmax
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Latitude'
    ybox=[0,10,10,0,0]
    x2=imin
    col1=1+indgen(nlvls)*mcolor/nlvls
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor
;
; PDFs
;
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    x=30.*findgen(nlvls)
    y1=histogram(xcoincips,min=min(x),max=max(x),binsize=x(1)-x(0))/(1.*mprof)
    y1=smooth(y1,3)     ; CIPS PDF
    y2=histogram(xcoinmls,min=min(x),max=max(x),binsize=x(1)-x(0))/(1.*mprof)
    y2=smooth(y2,3)     ; MLS PDF
    ymax=max(y1,y2)+0.1*max(y1,y2)
    plot,x,y1,xtitle='Longitude',ytitle='Frequency',charsize=1.2,$
         title='PDFs',xrange=[min(x),max(x)],yrange=[0.,ymax],/noerase,color=0
    !linetype=1
    y2=histogram(xcoinmls,min=min(x),max=max(x),binsize=x(1)-x(0))/(1.*mprof)
    y2=smooth(y2,3)     ; MLS PDF
    oplot,x,y2,color=0,thick=3
    plots,0.6*max(x),.8*ymax,/data,color=0
    plots,0.8*max(x),.8*ymax,/continue,/data,color=0,thick=3

    !linetype=0
    oplot,x,y1,color=0
    plots,0.6*max(x),.9*ymax,/data,color=0
    plots,0.8*max(x),.9*ymax,/continue,/data,color=0
    xyouts,0.8*max(x),.8*ymax,'MLS',/data,charsize=1.2,color=0
    xyouts,0.8*max(x),.9*ymax,'CIPS',/data,charsize=1.2,color=0
;ks_stats,xcoincips,xcoinmls,kstest,cprob
;xyouts,0.1*max(x),.9*ymax,'KS='+strmid(string(kstest),5,4),/data,color=0
;xyouts,0.1*max(x),.8*ymax,'KS sig='+$
;       strcompress(string(format='(f5.3)',100.*cprob),/remove_all)+'%',/data,color=0
;print,'KS=',kstest
;print,'KS significance=',100.*cprob,' %'
;cipsbar=total(xcoincips)/n_elements(xcoincips)
;mlsbar=total(xcoinmls)/n_elements(xcoinmls)
;xcoincips=xcoincips-cipsbar+mlsbar
;ks_stats,xcoincips,xcoinmls,kstest,cprob
;xyouts,.7,.7*ymax,'w/o Mean Bias:',/data,color=0
;xyouts,.7,.6*ymax,'KS='+strmid(string(kstest),5,4),/data,color=0
;xyouts,.7,.5*ymax,'KS sig='+strcompress(string(format='(f5.3)',$
;       100.*cprob),/remove_all)+'%',/data,color=0

    skippdf:
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim save_coin_mls_cips_'+sdate+'.ps -rotate -90 save_coin_mls_cips_'+sdate+'.jpg'
       spawn,'rm -f save_coin_mls_cips_'+sdate+'.ps'
    endif
      skipmls:
      icount=icount+1L
goto,jump
end
