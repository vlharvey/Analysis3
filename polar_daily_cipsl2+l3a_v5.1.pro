;
; read CIPS level 2a and 3a version 5.10 data and oplot geolocation information where albedos are not NaN
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
xorig=[0.1]
yorig=[0.1]
xlen=0.8
ylen=0.8
cbaryoff=0.02
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

pth2='/atmos/harvey/CIPS_data/Datfiles/Level_2/cips_sci_2_orbit_'
pth3='/atmos/harvey/CIPS_data/Datfiles/Level_3a_Daisies/cips_sci_3a_'
;
; NH
;
;lstmn=6
;lstdy=29
;lstyr=2016
;ledmn=6
;leddy=29
;ledyr=2016
;
; SH
;
lstmn=12
lstdy=22
lstyr=2016
ledmn=12
leddy=22
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
; get level 2 nc filenames on this day
;
      spawn,'ls '+pth2+'*'+syear+'-'+sday+'*v05.10_r01_cat.nc',fnamescat		; 52822
      spawn,'ls '+pth2+'*'+syear+'-'+sday+'*v05.10_r01_cld.nc',fnamescld
      if fnamescat(0) eq '' then goto,jump
      if n_elements(fnamescat) ne n_elements(fnamescld) then goto,jump
      norbit=n_elements(fnamescat)
      orbit_all=lonarr(norbit)
      nlvls=norbit
      col1=1+indgen(nlvls)*icolmax/(nlvls)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          FNAME=FNAMESCAT(iorbit)
          print,fname
;
; read L2 catalog file
;
          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          nvars=result.nvars        ;# variables in the file
          for ivar=0,nvars-1 do begin
              result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if ( ( size( data, /n_dimensions )  EQ 1 ) && $
                   ( size( data, /type ) EQ 1 ) ) then $
                     data = string( data )
              if Execute(result.name + ' = data') eq 0 then $
                 Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
          endfor
          ncdf_close,ncid
;
; adjust lon/lat
;
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
; read L2 cld file
;
          FNAME=FNAMESCLD(iorbit)
          print,fname

          ncid=ncdf_open(fname)
          result=ncdf_inquire(ncid)
          nvars=result.nvars        ;# variables in the file
          for ivar=0,nvars-1 do begin
              result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
              ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
              if ( ( size( data, /n_dimensions )  EQ 1 ) && $
                   ( size( data, /type ) EQ 1 ) ) then $
                     data = string( data )
              if Execute(result.name + ' = data') eq 0 then $
                 Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
          endfor
          ncdf_close,ncid

          alb=CLD_ALBEDO
          alb_err=CLD_ALBEDO_UNC
          sza=ZENITH_ANGLE_RAY_PEAK
          orbit_all(iorbit)=AIM_ORBIT_NUMBER
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
          good=where(FINITE(ALB) EQ 1 and alb ge 10.,ngood)
          IF NGOOD GT 0 THEN BEGIN
             latitude=latitude(good)
             longitude=longitude(good)
          ENDIF
;
; at the beginning of each day...
;
          if iorbit eq 0L then begin
;
; retain vars for all orbits today
;
             lon_all=longitude
             lat_all=latitude
;
; set postscript file, call map
;
             if setplot eq 'ps' then begin
                lc=0
                xsize=nxdim/100.
                ysize=nydim/100.
                set_plot,'ps'
                !p.font=0
                device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                       /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_daily_cipsl2+l3a_v5.1_'+sdate+'.ps'
                !p.charsize=1.25
                !p.thick=2
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
             map_set,-90,0,0,/ortho,/contin,/grid,/noeras,color=0,title=sdate+' DOY='+sday,charsize=2
         endif
         oplot,longitude,latitude,psym=8,symsize=0.5,color=0	;col1(iorbit)
;
; retain all lon/lat information from each orbit
;
         if iorbit gt 0L then begin
            lon_all=[lon_all,longitude]
            lat_all=[lat_all,latitude]
         endif

         print,'L2 ',iorbit,bbox
     endfor  ; loop over orbits
     loadct,0
;
; read 3a daisy file
;
     spawn,'ls '+pth3+syear+'-'+sday+'*v05.10_r01.nc',fname3a
     if fname3a(0) eq '' then goto,jump
     ncid=ncdf_open(fname3a)
     result=ncdf_inquire(ncid)
     nvars=result.nvars        ;# variables in the file
     for ivar=0,nvars-1 do begin
         result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
         ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
         if ( ( size( data, /n_dimensions )  EQ 1 ) && $
              ( size( data, /type ) EQ 1 ) ) then $
                data = string( data )
         if Execute(result.name + ' = data') eq 0 then $
            Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
;help,result.name,data
     endfor
     ncdf_close,ncid
     print,'L3 ',bbox
;
; read appropriate lon/lat grid for L3 daisy
;
;restore,'level_3a_lat_lon_north_v5.sav
restore,'level_3a_lat_lon_south_v5.sav

     lat=latitude
     lon=longitude
     alb=albedo
     qf=quality_flags
     good=where(finite(alb) eq 1 and alb ge 10,ngood)
     lon=lon(good)
     lat=lat(good)
     index=where(lon lt 0.)
     if index(0) ne -1L then lon(index)=lon(index)+360.
;
; need to rotate daisy in the SH
;
     map_set,-90,0,-90,/ortho,/noeras	;,/isotropic,SCALE=104E6
     oplot,lon,lat,psym=8,symsize=0.5,color=100
;
; color bar
;
     loadct,39
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
        spawn,'convert -trim polar_daily_cipsl2+l3a_v5.0_'+sdate+'.ps -rotate -90 polar_daily_cipsl2+l3a_v5.0_'+sdate+'.jpg'
;       spawn,'rm -f polar_daily_cipsl2+l3a_v5.0_'+sdate+'.ps'
     endif

goto,jump
end
