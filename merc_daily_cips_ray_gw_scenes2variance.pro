;
; read one days worth of orbits of files like "cips_ray_gw_..." and calculate daily grid of % variance
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
ylen=0.5
cbaryoff=0.02
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

cips_mean=fltarr(nlon,nlat)/0.
cips_variance=fltarr(nlon,nlat)/0.
cips_counts=fltarr(nlon,nlat)

pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'
pth='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/cips_ray_gw_north_'

lstmn=6
lstdy=12
lstyr=2016
ledmn=6
leddy=15
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
      spawn,'ls '+pth+syear+'_?????_'+smn+sdy+'*.sav',fnames
;     spawn,'ls '+pth+syear+'_49885_'+smn+sdy+'*.sav',fnames	; MN thunderstorm scene 7
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; loop over orbits
;
      sorbit=strarr(norbit)
      FOR iorbit = 0,norbit-1 DO BEGIN
          restore,fnames(iorbit)
          dum=strsplit(fnames(iorbit),'_',/extract)
          sorbit(iorbit)=dum(-3)
;
; extract scenes for this orbit

          latitude_scene=scene.lat
          longitude_scene=scene.lon
          alb_scene=scene.alb
          sza_scene=scene.sza*180./!pi		; convert to degrees
          sca_scene=scene.sca*180./!pi		; convert to degrees
;
; retain only data where ALB is not Nan and SZA<= 85 (and convert to 1D arrays)
;
          good=where(FINITE(alb_scene) EQ 1 and sza_scene le 85.,ngood)
          if good(0) ne -1L then begin

             if iorbit eq 0L then begin
                latitude_scene_daily=reform(latitude_scene(good))
                longitude_scene_daily=reform(longitude_scene(good))
                alb_scene_daily=reform(alb_scene(good))
                sza_scene_daily=reform(sza_scene(good))
                sca_scene_daily=reform(sca_scene(good))
             endif
             if iorbit gt 0L then begin
                latitude_scene_daily=[latitude_scene_daily,reform(latitude_scene(good))]
                longitude_scene_daily=[longitude_scene_daily,reform(longitude_scene(good))]
                alb_scene_daily=[alb_scene_daily,reform(alb_scene(good))]
                sza_scene_daily=[sza_scene_daily,reform(sza_scene(good))]
                sca_scene_daily=[sca_scene_daily,reform(sca_scene(good))]
             endif
          endif
          print,'orbit ',iorbit+1
          help,latitude_scene_daily
      ENDFOR
;
; check
;
;erase
;plot,longitude_scene_daily,LATITUDE_SCENE_DAILY,psym=3,color=0
;stop
;plot,abs(alb_scene_daily),psym=3,color=0,yrange=[.1,1.e12],/ylog
;stop
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
       dx=longrid(1)-longrid(0)
       dy=latgrid(1)-latgrid(0)
       FOR J=0l,NLAT-1l DO BEGIN									; loop over latitudes
           index=where(latitude_scene_daily ge latgrid(j)-dy and latitude_scene_daily lt latgrid(j)+dy)
           if n_elements(index) lt 3 then goto,jumplat

           latitude_scene_lat=reform(latitude_scene_daily(index))
           longitude_scene_lat=reform(longitude_scene_daily(index))
           alb_scene_lat=reform(alb_scene_daily(index))
;help,alb_scene_lat
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
;print,longrid(i),latgrid(j)
;help,alb_scene_lat(index)
;print,result
;stop
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
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='merc_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps'
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
;map_set,0,0,0,/merc,/contin,/grid,title=sdate,color=0
;amin=-5.	;min(cips_mean,/nan)
;amax=5.	;max(cips_mean,/nan)
;nlvls=21
;level=amin+((amax-amin)/float(nlvls))*findgen(nlvls)
;col1=1+indgen(nlvls)*icolmax/nlvls
;contour,cips_mean,longrid,latgrid,level=level,c_color=col1,/cell_fill,/overplot
;map_set,0,0,0,/merc,/contin,/grid,color=0,/noeras
;ymnb=ymn -cbaryoff
;ymxb=ymnb+cbarydel
;set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
;!type=2^2+2^3+2^6
;plot,[amin,amax],[0,0],yrange=[0,10],xrange=[amin,amax],/noeras,color=0,xtitle='CIPS Perturbation Albedo (%) Mean'
;ybox=[0,10,10,0,0]
;x2=amin
;dx=(amax-amin)/(float(nlvls)-1)
;for j=1,nlvls-1 do begin
;    xbox=[x2,x2,x2+dx,x2+dx,x2]
;    polyfill,xbox,ybox,color=col1(j)
;    x2=x2+dx
;endfor
;
;xmn=xorig(1)
;xmx=xorig(1)+xlen
;ymn=yorig(1)
;ymx=yorig(1)+ylen
;set_viewport,xmn,xmx,ymn,ymx
;!type=2^2+2^3
map_set,0,0,0,/merc,/contin,/grid,color=0,/noeras,title=sdate
amin=min(cips_variance,/nan)
amax=max(cips_variance,/nan)
amax=10.
nlvls=21
level=amin+((amax-amin)/float(nlvls))*findgen(nlvls)
col1=1+indgen(nlvls)*icolmax/nlvls
contour,cips_variance,longrid,latgrid,level=level,c_color=col1,/cell_fill,/overplot
map_set,0,0,0,/merc,/contin,/grid,color=0,/noeras
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[amin,amax],[0,0],yrange=[0,10],xrange=[amin,amax],/noeras,color=0,xtitle='CIPS Perturbation Albedo (%) Variance'
ybox=[0,10,10,0,0]
x2=amin
dx=(amax-amin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor


; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim merc_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps -rotate -90 '+$
                            'merc_daily_cips_ray_gw_scenes2variance_'+sdate+'.jpg'
;       spawn,'rm -f merc_daily_cips_ray_gw_scenes2variance_'+sdate+'.ps'
     endif

goto,jump
end
