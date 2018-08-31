;
; read one days worth of orbits of files like "cips_ray_gw_..." and plot SZA maps
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
SZALIM_HI=120.	;90.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT^M
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT^M
;
; level 3 variance grid
;
nlat=86L
latgrid=-85+2.*findgen(nlat)
nlon=90L
longrid=4.*findgen(nlon)		; AIRS level3 is 4x2 degree

pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'
pth='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/cips_ray_gw_north_'

lstmn=3
lstdy=22
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
print,fnames(iorbit)
          dum=strsplit(fnames(iorbit),'_',/extract)
          sorbit(iorbit)=dum(-3)
;
; extract scenes for this orbit

          latitude_scene=scene.lat
          longitude_scene=scene.lon
          alb_scene=scene.alb
          sza_scene=scene.sza*180./!pi		; convert to degrees
good=where(finite(ALB_SCENE) and finite(SZA_SCENE))                
print,max(SZA_SCENE(good)),max(ALB_SCENE(good))
;if iorbit eq 0L then begin
;   erase
;   xmn=xorig(0)
;   xmx=xorig(0)+xlen
;   ymn=yorig(0)
;   ymx=yorig(0)+ylen
;   set_viewport,xmn,xmx,ymn,ymx
;   !type=2^2+2^3
;   plot,SZA_SCENE(good),ALB_SCENE(good),psym=3,color=0,yrange=[-200,200],xrange=[50,105],charsize=2,title=syear+smn+sdy
;;  oplot,85+0*findgen(21),-1000+100*findgen(21),color=210,thick=5
;   oplot,SZALIM_HI+0*findgen(21),-1000+100*findgen(21),color=250,thick=5
;endif
;oplot,SZA_SCENE(good),ALB_SCENE(good),psym=3,color=0
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
; daily global SZA
;
       if setplot eq 'ps' then begin
          lc=0
          xsize=nxdim/100.
          ysize=nydim/100.
          set_plot,'ps'
          !p.font=0
          device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='moll_daily_cips_scenes_sza_'+sdate+'.ps'
          !p.charsize=1.5
          !p.thick=2
          !p.charthick=5
          !y.thick=2
          !x.thick=2
       endif
       !p.charsize=1.5
       !p.thick=2
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
map_set,0,0,0,/moll,/contin,/grid,title=sdate,color=0
smin=45.
smax=90.
for i=0L,n_elements(LATITUDE_SCENE_DAILY)-1L do oplot,[longitude_scene_daily(i),longitude_scene_daily(i)],[LATITUDE_SCENE_DAILY(i),LATITUDE_SCENE_DAILY(i)],$
    color=((sza_scene_daily(i)-smin)/(smax-smin))*mcolor,psym=8
index=where(sza_scene_daily gt 90.)
if index(0) ne -1L then oplot,longitude_scene_daily(index),latitude_scene_daily(index),color=0,psym=8
nlvls=21
level=smin+((smax-smin)/float(nlvls))*findgen(nlvls)
col1=indgen(nlvls)*icolmax/(nlvls-1)
col1(-1)=col1(-1)-1L
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[smin,smax],[0,0],yrange=[0,10],xrange=[smin,smax],/noeras,color=0,xtitle='SZA'
ybox=[0,10,10,0,0]
x2=smin
dx=(smax-smin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim moll_daily_cips_scenes_sza_'+sdate+'.ps -rotate -90 '+$
                            'moll_daily_cips_scenes_sza_'+sdate+'.jpg'
        spawn,'rm -f moll_daily_cips_scenes_sza_'+sdate+'.ps'
     endif
;
; remove SZA > 90
;
     good=where(sza_scene_daily le 90,ngood)
     if good(0) ne -1L then begin
        latitude_scene_daily=latitude_scene_daily(good)
        longitude_scene_daily=longitude_scene_daily(good)
        alb_scene_daily=alb_scene_daily(good)
        sza_scene_daily=sza_scene_daily(good)
     endif
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
cips_sza=fltarr(nlon,nlat)/0.
cips_variance=fltarr(nlon,nlat)/0.
cips_counts=fltarr(nlon,nlat)

       dx=(longrid(1)-longrid(0))/2.
       dy=(latgrid(1)-latgrid(0))/2.
       FOR J=0l,NLAT-1l DO BEGIN                                                                        ; loop over latitudes
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
              cips_variance(0,j)=result(1)      ;/result(0)             ; percent variance = 100 * variance/mean
              cips_counts(0,j)=npoints

              result=moment(sza_scene_lat(index))
              cips_sza(0,j)=result(0)
           endif

           for i=1L,nlon-1L do begin                                                                    ; loop over longitudes
               index=where(longitude_scene_lat ge longrid(i)-dx and longitude_scene_lat lt longrid(i)+dx,npoints)
               if index(0) ne -1L then begin
                  result=moment(alb_scene_lat(index))
                  cips_mean(i,j)=result(0)
                  cips_variance(i,j)=result(1)  ;/result(0)               ; percent variance = 100 * variance/mean
                  cips_counts(i,j)=npoints
;print,longrid(i),latgrid(j)
;help,alb_scene_lat(index)
;print,result
;stop
                  result=moment(sza_scene_lat(index))
                  cips_sza(i,j)=result(0)
               endif
           endfor

           jumplat:
       endfor

       if setplot eq 'ps' then begin
          lc=0
          xsize=nxdim/100.
          ysize=nydim/100.
          set_plot,'ps'
          !p.font=0
          device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='moll_daily_cips_gridded_sza_'+sdate+'.ps'
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
map_set,0,0,0,/moll,/contin,/grid,title=sdate,color=0
contour,cips_sza,longrid,latgrid,/overplot,levels=level,c_color=col1,/cell_fill
contour,cips_sza,longrid,latgrid,/overplot,levels=[85],color=mcolor,/foll

contour,smooth(cips_sza,3,/Nan),longrid,latgrid,/overplot,levels=level,c_color=col1,/cell_fill
contour,smooth(cips_sza,3,/Nan),longrid,latgrid,/overplot,levels=[85],color=mcolor,/foll
map_set,0,0,0,/moll,/contin,/grid,color=0,/noeras
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[smin,smax],[0,0],yrange=[0,10],xrange=[smin,smax],/noeras,color=0,xtitle='Gridded SZA < 90'
ybox=[0,10,10,0,0]
x2=smin
dx=(smax-smin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim moll_daily_cips_gridded_sza_'+sdate+'.ps -rotate -90 '+$
                            'moll_daily_cips_gridded_sza_'+sdate+'.jpg'
;       spawn,'rm -f moll_daily_cips_gridded_sza_'+sdate+'.ps'
     endif

goto,jump
end
