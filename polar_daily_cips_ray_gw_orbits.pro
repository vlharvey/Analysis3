;
; read one days worth of orbits of files like "cips_ray_gw_..." and plot global perturbation albedo
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
xorig=[0.15,0.15]
yorig=[0.525,0.1]
xlen=0.3
ylen=0.4
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

pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'
pth='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/cips_ray_gw_north_'

lstmn=6
lstdy=9
lstyr=2016
ledmn=6
leddy=9
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
      FOR iorbit = 2,2 do begin	;norbit-1 DO BEGIN
          restore,fnames(iorbit)
dum=strsplit(fnames(iorbit),'_',/extract)
sorbit=dum(-3)

;
latitude_orbit=orbit.lat
longitude_orbit=orbit.lon
alb_orbit=orbit.alb
sza_orbit=orbit.sza*180./!pi
bad=WHERE(sza_orbit ge 90.,nbad)
IF NBAD GT 0 THEN BEGIN
   latitude_orbit(bad)=0./0.
   longitude_orbit(bad)=0./0.
   ALB_orbit(bad)=0./0.
ENDIF


latitude_scene=scene.lat
longitude_scene=scene.lon
alb_scene=scene.alb
sza_scene=scene.sza
sca_scene=scene.sca
time_scene=scene.time

for iscene=12L,13 do begin	;n_elements(time_scene)-1L do begin

alb=reform(alb_scene(*,*,iscene))
sza=reform(sza_scene(*,*,iscene))*180./!pi
sca=reform(sca_scene(*,*,iscene))*180./!pi
longitude=reform(longitude_scene(*,*,iscene))
latitude=reform(latitude_scene(*,*,iscene))

          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)		;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)	;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
;
; filter bad data based on specifications set above
;
          good=WHERE(FINITE(ALB) EQ 1 and sza le 85.,ngood)
          if ngood lt 1000. then goto,skipscene
          bad=WHERE(sza ge 90.,nbad)
          IF NBAD GT 0 THEN BEGIN
             latitude(bad)=0./0.
             longitude(bad)=0./0.
             ALB(bad)=0./0.
          ENDIF
;
; plot clouds color by brightness
;
;if iscene eq 12L then begin	; global map
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
              /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_daily_cips_ray_gw_'+sdate+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif

if iscene eq 12 then  erase
   xmn=xorig(12-iscene)
   xmx=xorig(12-iscene)+xlen
   ymn=yorig(12-iscene)
   ymx=yorig(12-iscene)+ylen
   set_viewport,xmn,xmx,ymn,ymx
   !type=2^2+2^3
ymin=-58	;min(latitude,/nan)-7
ymax=-28	;max(latitude,/nan)+1
xmin=157	;min(longitude,/nan)-2
xmax=180	;max(longitude,/nan)+2
   map_set,mean(latitude,/nan),mean(longitude,/nan),0,/ortho,/contin,/grid,title=sdate+' '+sorbit+' sc '+strcompress(iscene+1),color=0,/usa,limit=[ymin,xmin,ymax,xmax],/noeras	; for scene by scene
print,iscene+1,mean(latitude,/nan),mean(longitude,/nan)
;  map_set,-90,0,-90,/ortho,/contin,/grid,title=sdate,color=0,/usa	;	; SH
;endif

amin=min(alb,/nan)
amax=max(alb,/nan)
nlvls=11
level=amin+((amax-amin)/float(nlvls))*findgen(nlvls)
print,'albedo ',amin,amax
col1=1+indgen(nlvls)*icolmax/nlvls

;contour,alb,longitude,latitude,level=[-1,-.5,0,.5,1],c_color=[0,100,160,200,250],/follow,/overplot
contour,alb,longitude,latitude,level=-5+0.5*findgen(9),c_color=20*findgen(9),/follow,/overplot
contour,alb,longitude,latitude,level=1+0.5*findgen(9),c_color=90+20*findgen(9),/follow,/overplot

;print,'sza ',min(sza,/nan),max(sza,/nan)
;print,'sca ',min(sca,/nan),max(sca,/nan)
;contour,sza,longitude,latitude,level=0.2+0.2*findgen(9),c_color=90+20*findgen(9),/follow,/overplot

;index=where(alb lt 0.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.3
;index=where(alb gt 0.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.9
;index=where(alb gt 100.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.9

skipscene:
endfor  ; loop over scenes
endfor  ; loop over orbits

set_viewport,.55,.55+xlen,max(yorig),max(yorig)+ylen
!type=2^2+2^3
ymin=-58        ;min(latitude,/nan)-7
ymax=-28        ;max(latitude,/nan)+1
xmin=157        ;min(longitude,/nan)-2
xmax=180        ;max(longitude,/nan)+2
map_set,mean(latitude,/nan),mean(longitude,/nan),0,/ortho,/contin,/grid,title=sdate+' '+sorbit+' sc 13+14',color=0,/usa,limit=[ymin,xmin,ymax,xmax],/noeras ; for scene by scene
iscene=12
alb=reform(alb_scene(*,*,iscene))
sza=reform(sza_scene(*,*,iscene))*180./!pi
sca=reform(sca_scene(*,*,iscene))*180./!pi
longitude=reform(longitude_scene(*,*,iscene))
latitude=reform(latitude_scene(*,*,iscene))
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)           ;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)   ;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
          good=WHERE(FINITE(ALB) EQ 1 and sza le 85.,ngood)
          if ngood lt 1000. then goto,skipscene
          bad=WHERE(sza ge 90.,nbad)
          IF NBAD GT 0 THEN BEGIN
             latitude(bad)=0./0.
             longitude(bad)=0./0.
             ALB(bad)=0./0.
          ENDIF
contour,alb,longitude,latitude,level=-5+0.5*findgen(9),c_color=20*findgen(9),/follow,/overplot
contour,alb,longitude,latitude,level=1+0.5*findgen(9),c_color=90+20*findgen(9),/follow,/overplot

iscene=13
alb=reform(alb_scene(*,*,iscene))
sza=reform(sza_scene(*,*,iscene))*180./!pi
sca=reform(sca_scene(*,*,iscene))*180./!pi
longitude=reform(longitude_scene(*,*,iscene))
latitude=reform(latitude_scene(*,*,iscene))
          X=WHERE(LATITUDE GT 90,NX)
          IF NX GT 0 THEN LATITUDE(X)=180-LATITUDE(X)           ;correct latitude for crossing over the NP
          X=WHERE(LATITUDE lt -90.,nx)
          if nx gt 0L then latitude(x)=-90.-(latitude(x)+90.)   ;correct latitude for crossing over the SP
          X=WHERE(LONGITUDE LT 0,NX)
          IF NX GT 0 THEN LONGITUDE(X)=LONGITUDE(X)+360
          good=WHERE(FINITE(ALB) EQ 1 and sza le 85.,ngood)
          if ngood lt 1000. then goto,skipscene
          bad=WHERE(sza ge 90.,nbad)
          IF NBAD GT 0 THEN BEGIN
             latitude(bad)=0./0.
             longitude(bad)=0./0.
             ALB(bad)=0./0.
          ENDIF
contour,alb,longitude,latitude,level=-5+0.5*findgen(9),c_color=20*findgen(9),/follow,/overplot
contour,alb,longitude,latitude,level=1+0.5*findgen(9),c_color=90+20*findgen(9),/follow,/overplot


set_viewport,.55,.55+xlen,min(yorig),min(yorig)+ylen
!type=2^2+2^3
ymin=-58        ;min(latitude,/nan)-7
ymax=-28        ;max(latitude,/nan)+1
xmin=157        ;min(longitude,/nan)-2
xmax=180        ;max(longitude,/nan)+2
map_set,mean(latitude,/nan),mean(longitude,/nan),0,/ortho,/contin,/grid,title=sdate+' '+sorbit,color=0,/usa,limit=[ymin,xmin,ymax,xmax],/noeras ; for scene by scene
contour,alb_orbit,longitude_orbit,latitude_orbit,level=-5+0.5*findgen(9),c_color=20*findgen(9),/follow,/overplot
contour,alb_orbit,longitude_orbit,latitude_orbit,level=1+0.5*findgen(9),c_color=90+20*findgen(9),/follow,/overplot


;slevel=string(level)
;ymnb=ymn -cbaryoff
;ymxb=ymnb+cbarydel
;set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
;!type=2^2+2^3+2^6
;plot,[amin,amax],[0,0],yrange=[0,10],xrange=[amin,amax],/noeras,color=0,xticks=nlvls-1,xtickname=slevel,$
;      xtitle='CIPS Perturbation Albedo'
;ybox=[0,10,10,0,0]
;x2=amin
;dx=(amax-amin)/(float(nlvls)-1)
;for j=1,nlvls-1 do begin
;    xbox=[x2,x2,x2+dx,x2+dx,x2]
;    polyfill,xbox,ybox,color=col1(j)
;    x2=x2+dx
;endfor

; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim polar_daily_cips_ray_gw_'+sdate+'.ps -rotate -90 '+$
                            'polar_daily_cips_ray_gw_'+sdate+'.jpg'
        spawn,'rm -f polar_daily_cips_ray_gw_'+sdate+'.ps'
     endif



goto,jump
end
