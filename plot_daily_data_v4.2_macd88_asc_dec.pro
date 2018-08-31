;
; save all CIPS level2 v4.20 data into daily files
; Quality Flag = 0 (equivalent to nlayers>=6)
; SZA range: 42-94
; run on MacD88
; save plot of daily percent clouds as a function of latitude
; 2 panel, 1 for asc and 1 for dec 
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
xorig=[0.20,0.20]
yorig=[0.60,0.18]
xlen=0.6
ylen=0.275
cbaryoff=0.11
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,1.2*cos(a),1.2*sin(a),/fill
nr=181L
latbin=-90.+findgen(nr)

;pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on aura
;pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macp98
pth='/Volumes/data/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macd88

lstmn=1
lstdy=1
lstyr=2007
ledmn=1
leddy=1
ledyr=2011
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
      ofile=pth+sdate+'_all.sav
      dum=findfile(ofile)
      if dum(0) eq '' then goto,jump
;
; read data
; latitude_orig_all,latitude_all,longitude_all,ut_time_all,SZA_all,CLOUD_INDEX_all,ALB_all,ALB_ERR_all,$
; RAD_all,RAD_ERR_all,IWC_all,IWC_ERR_all,ICD_all,QUALITY_FLAGS_all,NLAYERS_all
;
      restore,ofile
;
; plot
;
if setplot eq 'ps' then begin
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='zonal_mean_cloud_freq_v4.2_'+sdate+'.ps'
   !p.charsize=1.5
   !p.thick=1.5
   !p.charthick=5
   !p.charthick=5
   !y.thick=1.5
   !x.thick=1.5
endif
;
; ascending and descending cloud frequencies as a function of latitude
;
asc_freq=fltarr(nr)
des_freq=fltarr(nr)
for j=0L,nr-1L do begin
    ymin=latbin(j)-0.5
    ymax=latbin(j)+0.5
    index=where(SZA_all ge 42. and SZA_all le 94. and quality_flags_all eq 0 and alb_all gt 0. and (latitude_orig_all gt 90. or latitude_orig_all lt -90.) and latitude_all ge ymin and latitude_all lt ymax,nasc)
    all=where(SZA_all ge 42. and SZA_all le 94. and (latitude_orig_all gt 90. or latitude_orig_all lt -90.) and latitude_all ge ymin and latitude_all lt ymax,nascall)
    if index(0) ne -1L then asc_freq(j)=100.*float(nasc)/float(nascall)
    index=where(SZA_all ge 42. and SZA_all le 94. and quality_flags_all eq 0 and alb_all gt 0. and latitude_orig_all le 90. and latitude_orig_all ge -90. and latitude_all ge ymin and latitude_all lt ymax,ndes)
    all=where(SZA_all ge 42. and SZA_all le 94. and latitude_orig_all le 90. and latitude_orig_all ge -90. and latitude_all ge ymin and latitude_all lt ymax,ndesall)
    if index(0) ne -1L then des_freq(j)=100.*float(ndes)/float(ndesall)
endfor

erase
xyouts,.2,.95,'CIPS Version 4.20 on '+sdate,/normal,color=0,charsize=2
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,latbin,asc_freq,color=0,psym=8,xtitle='Latitude',ytitle='Cloud Frequency (%)',title='Ascending Node',xrange=[-90.,90.],yrange=[0.01,100.],/noeras,/ylog
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,latbin,des_freq,color=0,psym=8,xtitle='Latitude',ytitle='Cloud Frequency (%)',title='Decending Node',xrange=[-90.,90.],yrange=[0.01,100.],/noeras,/ylog
xyouts,.2,.05,'QF=0 and ALB GT 0 and SZA 42 to 94',/normal,color=0,charsize=1.5
;
; Close PostScript file and return control to X-windows
 if setplot ne 'ps' then stop
 if setplot eq 'ps' then begin
    device, /close
    spawn,'convert -trim zonal_mean_cloud_freq_v4.2_'+sdate+'.ps -rotate -90 '+$
                        'zonal_mean_cloud_freq_v4.2_'+sdate+'.jpg'
    spawn,'rm -f zonal_mean_cloud_freq_v4.2_'+sdate+'.ps'
 endif
goto,jump
end
