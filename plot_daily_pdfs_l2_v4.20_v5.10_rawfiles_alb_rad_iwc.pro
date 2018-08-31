;
; plot daily pdfs for alb, iwc, rad - as % of points
; compare CIPS level 2 v4.2 and v5.10
; VLH 4/18/2017
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
yorig=[0.35,0.35,0.35]
xorig=[0.15,0.45,0.75]
ylen=0.4
xlen=0.2
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
pthout='/Volumes/Data/CIPS_data/Datfiles_PDFs/cips_sci_2_'
;
; loop over years
;
for iyear=2007,2017 do begin
syear=strcompress(long(iyear),/r)

icount=0L
;goto,quick

lstmn=5
lstdy=1
lstyr=iyear
ledmn=9
leddy=30
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
ALBLIM=2.
RADLIM=20.
;ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO - not used here
SZALIM_HI=94.    ;DATA WITH SZA outside this range of SZALIM ARE SUSPECT
SZALIM_LO=42.

; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,nextyear
      syr=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syr+smn+sdy
      print,sdate
      dum=findfile(pthout+sdate+'_pdfs.sav')
      if dum(0) eq '' then goto,skipcips
      restore,pthout+sdate+'_pdfs.sav'

      if setplot eq 'ps' then begin
         lc=0
         set_plot,'ps'
         xsize=nxdim/100.
         ysize=nydim/100.
         !p.font=0
         device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                /bold,/color,bits_per_pixel=8,/helvetica,filename='compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps'
         !p.charsize=1.25
         !p.thick=2
         !p.charthick=5
         !p.charthick=5
         !y.thick=2
         !x.thick=2
      endif

erase
xyouts,.4,.8,sdate,/normal,color=0,charsize=3,charthick=2
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlev=n_elements(ALB_PDF42)
y20=ALB_PDF42
y51=ALB_PDF51
amax=max([y20,y51])
abin=albbin(1)-albbin(0)
alevs=alblim+abin*findgen(nlev)
plot,alevs,y20,thick=10,color=0,/ylog,xrange=[min(alevs),max(alevs)],yrange=[.1,100.],ytickname=['10!u-1!n','10!u0!n','10!u1!n','10!u2!n'],xtitle='Albedo (G)',ytitle='% of L2 points'		; normalized
oplot,alevs,y51,thick=8,color=250

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
y20=IWC_PDF42
y51=IWC_PDF51
amax=max([y20,y51])
abin=iwcbin(1)-iwcbin(0)
alevs=abin*findgen(nlev)
plot,alevs,y20,thick=10,color=0,/ylog,xrange=[min(alevs),max(alevs)],yrange=[.1,100.],xtitle='IWC (ug/m2)',ytickname=['10!u-1!n','10!u0!n','10!u1!n','10!u2!n']
oplot,alevs,y51,thick=8,color=250

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
y20=RAD_PDF42
y51=RAD_PDF51
amax=max([y20,y51])
abin=radbin(1)-radbin(0)
alevs=abin*findgen(nlev)
plot,alevs,y20,thick=10,color=0,/ylog,xrange=[min(alevs),max(alevs)],yrange=[.1,100.],xtitle='RAD (nm)',ytickname=['10!u-1!n','10!u0!n','10!u1!n','10!u2!n']
oplot,alevs,y51,thick=8,color=250

      if setplot ne 'ps' then stop
      if setplot eq 'ps' then begin
         device, /close
         spawn,'convert -trim compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps -rotate -90 compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.jpg'
;        spawn,'rm -f compare_cips_l4_v4.20_v5.10_rawfiles_'+sdate+'.ps'
      endif

      icount=icount+1
      skipcips:
goto,jump

nextyear:
endfor	; loop over years
end
