;
; read one days worth of orbits of files like "cips_ray_gw_..." and plot SZA maps
;
@date2doy

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
nxdim=700
nydim=700
xorig=[0.1]
yorig=[0.2]
xlen=0.8
ylen=0.7
cbaryoff=0.04
cbarydel=0.03
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

ALBLIM=1.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ALBLIM=0.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO^M
SZALIM_HI=90.	;90.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT^M
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT^M
;
; level 3 variance grid
;
;nlat=171L
;nlon=360L
;latgrid=-85+findgen(nlat)
;longrid=findgen(nlon)
nlat=86L
;nlon=180L
latgrid=-85+2.*findgen(nlat)
;longrid=2.*findgen(nlon)

nlon=90L
longrid=4.*findgen(nlon)		; AIRS level3 is 4x2 degree

pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'
pth='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/cips_ray_gw_north_'

lstmn=6
lstdy=5
lstyr=2016
ledmn=8
leddy=4
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
      spawn,'ls '+pth+syear+'_?????_'+smn+sdy+'*.sav',fnames	; all orbits on this day
;     spawn,'ls '+pth+syear+'_49899_'+smn+sdy+'*.sav',fnames	; non-orographic scene 13
;     spawn,'ls '+pth+syear+'_49885_'+smn+sdy+'*.sav',fnames	; MN thunderstorm scene 7
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          file=fnames(iorbit)

          ;EXTRACT DATE, ORBIT, LAT/LON INFORMATION FROM FILENAME
          dum=strsplit(file,'_',/extract)
          syear=dum(-4)
          dum2=strsplit(dum(-2),'.',/extract)
          monday=dum2(0)
          Date2DOY, syear+monday, CIPS_doy
          orbitnum = dum(-3)      ;strmid(file,strlen(dir)+34,5)
          restore, file
          CIPS_frac_day = float(dum2(1))/100.     ;float(strmid(file,strlen(dir)+44,3))
          CIPS_time = 24.*CIPS_frac_day           ;float(strmid(file,strlen(dir)+44,3))

orbitstarttimestr=strmid(scene.ORBIT_START_TIME_UT,9,8)                                                                                                         ; extract HH:MM:SS
orbitstartsecs=float(strmid(orbitstarttimestr,0,2))*60.*60. + float(strmid(orbitstarttimestr,3,2))*60. + float(strmid(orbitstarttimestr,6,2))                   ; convert orbit start to seconds

timesinceorbitstart=(scene.time - scene.orbit_start_time )/1000000.                                                                                             ; seconds since orbit start
scenesecs=timesinceorbitstart + orbitstartsecs                                                                                                                  ; add seconds since to orbit start

iscene=0
scenetime=(scenesecs(0)/86400.)*24.                                                                                                                                ; scene time in frac hours
scenemins=long((scenetime-long(scenetime))*60.)
scenetimestr=string(format = '(I2.2)',long(scenetime))+':'+string(format = '(I2.2)', scenemins)
;
; extract scenes for this orbit

          latitude_scene=scene.lat
          longitude_scene=scene.lon
          alb_scene=scene.alb
          sza_scene=scene.sza*180./!pi		; convert to degrees
;
; retain only data where ALB is not Nan and SZA<= 90 (and convert to 1D arrays for concatenation)
;
          good=where(FINITE(alb_scene) EQ 1 and sza_scene le SZALIM_HI,ngood)
          if good(0) ne -1L then begin

             if iorbit eq 0L then begin
                latitude_scene_daily=reform(latitude_scene(good))
                longitude_scene_daily=reform(longitude_scene(good))
                alb_scene_daily=reform(alb_scene(good))
                sza_scene_daily=reform(sza_scene(good))
             endif
             if iorbit gt 0L then begin
                latitude_scene_daily=[latitude_scene_daily,reform(latitude_scene(good))]
                longitude_scene_daily=[longitude_scene_daily,reform(longitude_scene(good))]
                alb_scene_daily=[alb_scene_daily,reform(alb_scene(good))]
                sza_scene_daily=[sza_scene_daily,reform(sza_scene(good))]
             endif
          endif
;         help,latitude_scene_daily
      ENDFOR
;
; make sure latitudes range from -90 to 90 and longitudes from 0 to 360
;
       X=WHERE(latitude_scene_daily GT 90,NX)
       IF NX GT 0 THEN latitude_scene_daily(X)=180-latitude_scene_daily(X)           ;correct latitude for crossing over the NP
       X=WHERE(latitude_scene_daily lt -90.,nx)
       if nx gt 0L then latitude_scene_daily(x)=-90.-(latitude_scene_daily(x)+90.)   ;correct latitude for crossing over the SP
       X=WHERE(longitude_scene_daily LT 0,NX)
       IF NX GT 0 THEN longitude_scene_daily(X)=longitude_scene_daily(X)+360
;
; calculate variance in each bin from all scene data today
;
cips_mean=fltarr(nlon,nlat)/0.
cips_variance=fltarr(nlon,nlat)/0.
cips_counts=fltarr(nlon,nlat)

       dx=(longrid(1)-longrid(0))/2.
       dy=(latgrid(1)-latgrid(0))/2.
       FOR J=0l,NLAT-1l DO BEGIN									; loop over latitudes
           index=where(latitude_scene_daily ge latgrid(j)-dy and latitude_scene_daily lt latgrid(j)+dy)
           if n_elements(index) lt 3 then goto,jumplat
;
; CIPS data in this latitude band
;
           latitude_scene_lat=reform(latitude_scene_daily(index))
           longitude_scene_lat=reform(longitude_scene_daily(index))
           alb_scene_lat=reform(alb_scene_daily(index))
;
; GM logic for i=0, longrid(0) equal to 0 includes longitudes gt 359.5 and le 0.5
;
           index=where(longitude_scene_lat gt max(longrid)+dx or longitude_scene_lat le min(longrid)+dx,npoints)
           if index(0) ne -1L then begin
              result=moment(alb_scene_lat(index))
              cips_mean(0,j)=result(0)
              cips_variance(0,j)=result(1)	;/result(0)		; percent variance = 100 * variance/mean
              cips_counts(0,j)=npoints
           endif

           for i=1L,nlon-1L do begin									; loop over longitudes
               index=where(longitude_scene_lat ge longrid(i)-dx and longitude_scene_lat lt longrid(i)+dx,npoints)
               if index(0) ne -1L then begin
                  result=moment(alb_scene_lat(index))
                  cips_mean(i,j)=result(0)
                  cips_variance(i,j)=result(1)	;/result(0)               ; percent variance = 100 * variance/mean
                  cips_counts(i,j)=npoints
               endif
           endfor

           jumplat:
       endfor
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
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='moll_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps'
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
map_set,0,0,0,/moll,/contin,/grid,color=0,/noeras,title=sdate
level=[0.1,0.5,1,2,3,4,5,6,7,8,9,10,15,20]
nlvls=n_elements(level)
col1=1+indgen(nlvls)*icolmax/nlvls
;
; wrap
;
cips_variance1=fltarr(nlon+1,nlat)
cips_variance1(0:nlon-1,0:nlat-1)=cips_variance
cips_variance1(nlon,*)=cips_variance1(0,*)
longrid1=fltarr(nlon+1)
longrid1(0:nlon-1)=longrid
longrid1(nlon)=longrid1(0)+360.
y2d=fltarr(nlon+1,nlat)
for i=0,nlon-1 do y2d(i,*)=latgrid
y2d(nlon,*)=y2d(0,*)
;cips_variance1=smooth(cips_variance1,3,/Nan,/edge_truncate)
contour,cips_variance1,longrid1,latgrid,level=level,c_color=col1,/cell_fill,/overplot
contour,cips_variance1,longrid1,latgrid,level=[3],color=mcolor,thick=3,/foll,/overplot,c_labels=[0]
map_set,0,0,0,/moll,/contin,/grid,color=0,/noeras

!type=2^2+2^3+2^6                     ; no y title or ticsks
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
imin=min(level)
imax=max(level)
slab=' '+strarr(n_elements(level))
!p.title = ' '
plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level)-1L,color=0,$
     position = [xmn+0.01,ymnb,xmx-0.01,ymxb],xstyle=1,xtickname=slab,xtitle= 'CIPS Perturbation Albedo (%) Variance'
ybox=[0,10,10,0,0]
x2=0
for j=1,n_elements(col1)-1 do begin
    dx= 1./(n_elements(level)-1.)
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j-1]
    x2=x2+dx
endfor

slab=strcompress(string(format='(f3.1)',level),/remove_all)
slabcolor = fltarr(n_elements(level))*0.
slabcolor[0:3] = 255
x1=dx/2
for i=0L,n_elements(slab)-2L do begin
    slab0=slab[i]
    if level(i) ge 1. then slab0=strcompress(string(format='(i2)',level(i)),/remove_all)
    xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5, charthick=2
    x1=x1+dx
endfor

; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim moll_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps -rotate -90 '+$
                            'moll_daily_cips_ray_gw_scenes2variance_'+sdate+'.jpg'
;       spawn,'rm -f moll_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps'
     endif

goto,jump
end
