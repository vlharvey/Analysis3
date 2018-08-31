;
; compare individual scene albedo perturbation that has GWs with gridded variance for just that scene - and neighboring scenes without GWs
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
xorig=[0.1,0.6]
yorig=[0.2,0.2]
xlen=0.4
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
lstdy=11
lstyr=2016
ledmn=6
leddy=11
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
;     spawn,'ls '+pth+syear+'_?????_'+smn+sdy+'*.sav',fnames	; all orbits on this day
;     spawn,'ls '+pth+syear+'_49899_*.sav',fnames	; non-orographic scene 13
;     spawn,'ls '+pth+syear+'_49885_*.sav',fnames	; MN thunderstorm scene 7
      spawn,'ls '+pth+syear+'_49883_*.sav',fnames	; oro scene 14
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; loop over orbits
;
      FOR iorbit = 0,norbit-1 DO BEGIN
          file=fnames(iorbit)

          ;EXTRACT DATE, ORBIT, LAT/LON INFORMATION FROM FILENAME^M
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

;iscene=14	; non-oro case (13)
;iscene=8	; squall case (7)
iscene=13	; oro case (14)
scenetime=(scenesecs(iscene)/86400.)*24.                                                                                                                                ; scene time in frac hours
scenemins=long((scenetime-long(scenetime))*60.)
scenetimestr=string(format = '(I2.2)',long(scenetime))+':'+string(format = '(I2.2)', scenemins)
;
; extract one GW candidate scene
;
          lat_cips=scene.lat[*,*,iscene]
          lon_cips=scene.lon[*,*,iscene]
          alb_pert_cips = scene.alb[*,*,iscene]	; 13 for non-oro case
          sza_cips=scene.sza[*,*,iscene]*180./!pi
          scenestr = string(iscene,format = '(I2.2)')
;
; remove SZA>90
;
          bad=where(sza_cips gt 90.)
          if bad(0) ne -1L then begin
             alb_pert_cips(bad)=0./0.
             lon_cips(bad)=0./0.
             lat_cips(bad)=0./0.
          endif

          ;UNIT CIRCLE TRIGONOMETRY TO GET AVERAGE LONGITUDE
          coslon = cos(!dtor*(lon_cips))
          sinlon = sin(!dtor*(lon_cips))
          ;x and y of unit cirle:
          x= mean(coslon,/nan)
          y= mean(sinlon,/nan)
          ;x value indicates absolute value of longitude between 0 and 180, sign of y indicates what hemisphere
          cips_lon_mean = acos(x)/!dtor * (y/abs(y)) ; determine x component on unit circle and y direction
          cips_lat_mean = mean(lat_cips,/nan)
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
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='cips_scenes2variance_'+orbitnum+'_'+scenestr+'.ps'
          !p.charsize=1.25
          !p.thick=2
          !p.charthick=5
          !y.thick=2
          !x.thick=2
       endif

          xyouts,.25,.92, sdate+' DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr,/normal,color=0,charsize=2,charthick=2
          xmn=xorig(0)
          xmx=xorig(0)+xlen
          ymn=yorig(0)
          ymx=yorig(0)+ylen
          set_viewport,xmn,xmx,ymn,ymx
          !type=2^2+2^3
          limit = [cips_lat_mean-15,cips_lon_mean-10,cips_lat_mean+15,cips_lon_mean+10]; Set plot limits based on the mean CIPS lat/lon
          Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, /usa,$
                    /ISOTROPIC, color=0, limit = limit, /horizon,/CONTINENTS,/grid,londel=4,latdel=2
          loadct,70
          ;Set CIPS levels
          level2=[-5,-4,-3,-2,-1,0,1,2,3,4,5]	;/5. ; divide by 5 for squall
          nlvls2  = n_elements(level2)
          col2 = [250,220,190,170,125,125,100,80,60,30,0]
          contour, smooth(alb_pert_cips,1,/nan), lon_cips,lat_cips, /overplot, $
                   levels = level2,c_color = col2,/noerase, /close,/cell_fill,  color = 0,c_thick = 1
;
; CALCULATE GRIDDED VARIANCE
;
; reform to 1d arrays w/o Nans
;
          good=where(finite(alb_pert_cips))	; and abs(alb_pert_cips) le 20.)
          latitude_scene_daily=reform(lat_cips(good))
          longitude_scene_daily=reform(lon_cips(good))
          alb_scene_daily=reform(alb_pert_cips(good))
          sza_scene_daily=reform(sza_cips(good))
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
           sza_scene_lat=reform(sza_scene_daily(index))
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
;if result(1) gt 10. then stop
                  cips_mean(i,j)=result(0)
                  cips_variance(i,j)=result(1)	;/result(0)               ; percent variance = 100 * variance/mean
                  cips_counts(i,j)=npoints
               endif
           endfor

           jumplat:
       endfor
;
; overplot variance on scene
;
loadct,39
cips_variance1=fltarr(nlon+1,nlat)
cips_variance1(0:nlon-1,0:nlat-1)=cips_variance
cips_variance1(nlon,*)=cips_variance1(0,*)
longrid1=fltarr(nlon+1)
longrid1(0:nlon-1)=longrid
longrid1(nlon)=longrid1(0)+360.
y2d=fltarr(nlon+1,nlat)
for i=0,nlon-1 do y2d(i,*)=latgrid
y2d(nlon,*)=y2d(0,*)
vlevel=[0.1,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6]
vlevel=[0.1,0.5,1,1.5,2,2.5,3,4,5,6,7,8,9,10]
nlvls=n_elements(vlevel)
col1=1+indgen(nlvls)*icolmax/nlvls
;cips_variance1=smooth(cips_variance1,3,/Nan,/edge_truncate)
contour,cips_variance1,longrid1,latgrid,level=vlevel,c_color=col1,thick=2,/foll,/overplot,c_labels=1+0*vlevel
;
; scene color bar
;
loadct,70
;
; CIPS color bar
;
          level = level2
!type=2^2+2^3+2^6                     ; no y title or ticsks
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
level=level2
imin=min(level)
imax=max(level)
slab=' '+strarr(n_elements(level))
!p.title = ' '
plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level)-1L,color=0,charsize=1.25,charthick=2,$
     position = [xmn+0.01,ymnb,xmx-0.01,ymxb],xstyle=1,xtickname=slab,xtitle= 'Scene Perturbation Albedo (%)'
ybox=[0,10,10,0,0]
x2=0
for j=1,n_elements(col2)-1 do begin
    dx= 1./(n_elements(level)-1.)
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col2[j-1]
    x2=x2+dx
endfor

loadct,0
slab=strcompress(string(format='(f5.1)',level),/remove_all)
slabcolor = fltarr(n_elements(level))*0.
slabcolor[0:3] = 255
x1=dx/2
for i=0L,n_elements(slab)-2L do begin
    slab0=slab[i]
    xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5, charthick=2
    x1=x1+dx
endfor
;
; variance grid
;
loadct,39
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, /usa,$
         /ISOTROPIC, color=0, limit = limit, /horizon,/CONTINENTS,/grid,londel=4,latdel=2
;cips_variance1=smooth(cips_variance1,3,/Nan,/edge_truncate)
contour,cips_variance1,longrid1,latgrid,level=vlevel,c_color=col1,/cell_fill,/overplot
contour,cips_variance1,longrid1,latgrid,level=vlevel,color=0,thick=2,/foll,/overplot,c_labels=1+0*vlevel

!type=2^2+2^3+2^6                     ; no y title or ticsks
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
imin=min(vlevel)
imax=max(vlevel)
slab=' '+strarr(n_elements(vlevel))
!p.title = ' '
plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(vlevel)-1L,color=0,charsize=1.25,charthick=2,$
     position = [xmn+0.01,ymnb,xmx-0.01,ymxb],xstyle=1,xtickname=slab,xtitle= 'Gridded Perturbation Albedo (%) Variance'
ybox=[0,10,10,0,0]
x2=0
for j=1,n_elements(col1)-1 do begin
    dx= 1./(n_elements(vlevel)-1.)
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j-1]
    x2=x2+dx
endfor

slab=strcompress(string(format='(f3.1)',vlevel),/remove_all)
slabcolor = fltarr(n_elements(vlevel))*0.
slabcolor[0:3] = 255
x1=dx/2
for i=0L,n_elements(slab)-2L do begin
    slab0=slab[i]
    if vlevel(i) ge 1. then slab0=strcompress(string(format='(f3.1)',vlevel(i)),/remove_all)
    xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5, charthick=2
    x1=x1+dx
endfor

; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim cips_scenes2variance_'+orbitnum+'_'+scenestr+'.ps -rotate -90 '+$
                            'cips_scenes2variance_'+orbitnum+'_'+scenestr+'.jpg'
;       spawn,'rm -f cips_scenes2variance_'+orbitnum+'_'+scenestr+'.ps'
     endif

endfor	; loop over orbits
goto,jump
end
