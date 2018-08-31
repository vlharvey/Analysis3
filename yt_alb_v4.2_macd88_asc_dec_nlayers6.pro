;
; latitude-time plot
; save latitude-time plot of 2007 cloud information
; Albedo > 2G (2 x 10-6 sr-1)
; Quality Flag = 0 (equivalent to nlayers>=6)
; SZA range: 42-94
; Radius > 20
; run on MacD88
; read CIPS level 2a version 4.2 data and plot albedo vs SZA each day
; data starts on day 144 (May 23) 
; data ends on day 263 (Sep 19)
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
yorig=[0.35]
xlen=0.7
ylen=0.5
cbaryoff=0.1
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

pth='/Volumes/data/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macd88

lstmn=5
lstdy=24
lstyr=2007
ledmn=9
leddy=19
ledyr=2007
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
; set postscript
;
if setplot eq 'ps' then begin
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='yt_alb_v4.2_2007.ps'
   !p.charsize=1.5
   !p.thick=1.5
   !p.charthick=5
   !p.charthick=5
   !y.thick=1.5
   !x.thick=1.5
endif
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1

erase
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1+findgen(kday),findgen(50),color=0,xtitle='2007 DOY',title='Version 4.20 CIPS Albedo (10!u-6!n str!u-1!n)',ytitle='Latitude',$
     xrange=[iday,iday+kday],yrange=[40,90.],/nodata
;
; loop over days
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,plotit
      syear=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syear+smn+sdy
      print,sdate,' ',sday
      ofile=pth+sdate+'_albgt2_sza42-94.sav
      dum=findfile(ofile)
      if dum(0) eq '' then goto,jump
;
; save file contents
; latitude_all,longitude_all,ut_time_all,sza_all,CLOUD_INDEX_all,ALB_all,ALB_ERR_all,RAD_all,RAD_ERR_all,IWC_all,IWC_ERR_all,ICD_all

      restore,ofile
;
; color by albedo
;
index=where(alb_all ge 2. and alb_all le 3.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.1
index=where(alb_all gt 3. and alb_all le 4.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.2
index=where(alb_all gt 4. and alb_all le 5.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.3
index=where(alb_all gt 5. and alb_all le 10.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.4
index=where(alb_all gt 10. and alb_all le 20.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.5
index=where(alb_all gt 20. and alb_all le 30.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.6
index=where(alb_all gt 30. and alb_all le 40.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.7
index=where(alb_all gt 40. and alb_all le 50.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.8
index=where(alb_all gt 50.)
if index(0) ne -1L then oplot,iday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.9

goto,jump
;
; plot
;
plotit:
slevel=['2','3','4','5','10','20','30','40','50','>50']
nlvls=n_elements(slevel)
col1=1+indgen(nlvls)*icolmax/nlvls
imin=1.
imax=60.
    ymnb=ymn -cbaryoff
    ymxb=ymnb+cbarydel
    set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xticks=nlvls-1,xtickname=slevel,$
          xtitle='Albedo (10!u-6!n str!u-1!n)'
    ybox=[0,10,10,0,0]
    x2=imin
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor
;
; Close PostScript file and return control to X-windows
 if setplot ne 'ps' then stop
 if setplot eq 'ps' then begin
    device, /close
    spawn,'convert -trim yt_alb_v4.2_2007.ps -rotate -90 yt_alb_v4.2_2007.jpg'
    spawn,'rm -f yt_alb_v4.2_2007.ps'
 endif
end
