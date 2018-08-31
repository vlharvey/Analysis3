;
; read CIPS level 2a data and store daily average radius, albedo, IWC
; from all orbits binned every 5 degrees latitude
;
; color table and symbol
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
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.02
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill
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
pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2a_orbit_'

lstmn=7
lstdy=13
lstyr=2009
ledmn=7
leddy=16
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
SZALIM=91.	;DATA WITH SZA > SZALIM ARE BAD (IN NH THIS CAN ONLY HAPPEN ON THE ASCENDING NODE)
;SZALIM=180.	;DON'T GET RID OF ANY DATA BASED ON SZA.
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
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_daily_cipsl2a_alb_'+sdate+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif
;
; get nc filenames on this day
;
      spawn,'ls '+pth+'*'+syear+'-'+sday+'*.nc',fnames
      if fnames(0) eq '' then goto,jump
      norbit=n_elements(fnames)
;
; average albedo, IWC, radii in each latitude bin
;
      ALB_avg=fltarr(nlat)
      IWC_avg=fltarr(nlat)
      RAD_avg=fltarr(nlat)
      tot_avg=fltarr(nlat)
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
;IDL> help,/str,data
;** Structure <20db38>, 18 tags, length=176, data length=156, refs=1:
;   AIM_ORBIT_NUMBER INT          12109
;   VERSION         STRING    '03.20'
;   PRODUCT_CREATION_TIME STRING    '2009/202-12:23:19'
;   DEPENDANT1BVERSION STRING    '03.20'
;   DEPENDANT1CVERSION STRING    '03.20'
;   QUALITY_FLAGS   LONG                 0
;   STACK_ID        INT              0
;   UT_DATE         LONG          20090716
;   HEMISPHERE      STRING    'N'
;   STACK_START_TIME DOUBLE       9.3173728e+14
;   UT_TIME         DOUBLE          0.14681250
;   LA_TIME         STRING    '2009/197-00:08:49'
;   ALBEDO          POINTER   <PtrHeapVar28>
;   CENTER_LON      DOUBLE           82.797813
;   KM_PER_PIXEL    FLOAT           5.00000
;   BBOX            INT       Array[4]                                             
;   LATITUDE        POINTER   <PtrHeapVar29>
;   LONGITUDE       POINTER   <PtrHeapVar30>

          AIM_ORBIT_NUMBER=data.AIM_ORBIT_NUMBER		;INT Cumulative mission orbit number
          VERSION=data.VERSION					;STRING    '03.20'
          PRODUCT_CREATION_TIME=data.PRODUCT_CREATION_TIME	;STRING    '2009/040-13:23:03' Version number of data product
          DEPENDANT1BVERSION=data.DEPENDANT1BVERSION		;STRING    '03.20'
          UT_DATE=DATA.UT_DATE					;LONG       2009001 UTC date of this orbit
          HEMISPHERE=DATA.HEMISPHERE				;STRING    'S'
          STACK_ID=data.STACK_ID				;INT        0 uniquely identify the Level 1B data
          QUALITY_FLAGS=data.QUALITY_FLAGS			;LONG      TBD
          KM_PER_PIXEL=data.KM_PER_PIXEL			;INT              5
          BBOX=data.BBOX					;LONG      Array[4] {x, y} bounding box of map projected image
          CENTER_LON=data.CENTER_LON	;Center longitude of map proj, NOT data. Used for orienting the data horizontally.
          UT_TIME=(data[0].ut_time)				;POINTER  Number of seconds elapsed since orbit_start_time_ut
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
          ALB = (*data[0].albedo)				;Cloud albedo in Garys (10^-6 sr^-1) i.e., alb x 1.e6
;
; free memory
; 
          HEAP_GC
;         RESULT=MEMORY(/CURRENT)
;         PRINT,'MEMORY IS: ',RESULT
;         PRINT,' '
;
;GET RID OF ANY INFINITE DATA AND ANY negative DATA
;
          good=WHERE(FINITE(ALB) EQ 1 and ALB gt 0,ngood)
          IF NGOOD GT 0 THEN BEGIN
             ALB=alb(good)
             latitude=latitude(good)
             longitude=longitude(good)
             ut_date=ut_date(good)
             ut_time=ut_time(good)
          ENDIF
;
; plot clouds color by brightness
;
if iorbit eq 0L then begin
   erase
   xmn=xorig(0)
   xmx=xorig(0)+xlen
   ymn=yorig(0)
   ymx=yorig(0)+ylen
   set_viewport,xmn,xmx,ymn,ymx
   !type=2^2+2^3
   map_set,90,0,-90,/stereo,/contin,/grid,title=sdate,color=0,/usa,limit=[30.,0.,90.,360.]
endif
amin=0.
amax=60.
;if ngood gt 2 then begin
;   for i=0L,ngood-1L do begin
;       oplot,[longitude(i),longitude(i)],[latitude(i),latitude(i)],psym=8,$
;              color=((alb(i)-amin)/(amax-amin))*mcolor
;   endfor
;endif
oplot,longitude,latitude,psym=3,color=0
;index=where(alb gt 50.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.9
;index=where(alb gt 40. and alb le 50.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.8
;index=where(alb gt 30. and alb le 40.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.7
;index=where(alb gt 20. and alb le 30.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.6
;index=where(alb gt 10. and alb le 20.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.5
;index=where(alb gt 5. and alb le 10.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.4
;index=where(alb gt 4. and alb le 5.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.3
;index=where(alb gt 3. and alb le 4.)
;if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.2
index=where(alb gt 1. and alb le 3.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.1
index=where(alb gt 3. and alb le 4.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.2
index=where(alb gt 4. and alb le 5.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.3
index=where(alb gt 5. and alb le 10.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.4
index=where(alb gt 10. and alb le 20.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.5
index=where(alb gt 20. and alb le 30.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.6
index=where(alb gt 30. and alb le 40.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.7
index=where(alb gt 40. and alb le 50.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.8
index=where(alb gt 50.)
if index(0) ne -1L then oplot,longitude(index),latitude(index),psym=3,color=mcolor*.9

endfor	; loop over orbits
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
          xtitle='CIPS Albedo (10!u-6!n str!u-1!n)'
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
        spawn,'convert -trim polar_daily_cipsl2a_alb_'+sdate+'.ps -rotate -90 '+$
                            'polar_daily_cipsl2a_alb_'+sdate+'.jpg'
;       spawn,'/usr/bin/rm polar_daily_cipsl2a_alb_'+sdate+'.ps'
     endif

goto,jump
end
