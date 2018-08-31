;
; day 173 is June 21 (summer solstice in 2007)
; separate ASC from DES
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
xorig=[0.15,0.15]
yorig=[0.60,0.18]
xlen=0.7
ylen=0.3
cbaryoff=0.11
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

pth='/Volumes/data/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macd88

lstmn=5
lstdy=13
lstyr=2007
ledmn=9
leddy=10
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
          /bold,/color,bits_per_pixel=8,/helvetica,filename='yt_alb_v4.2_2007_asc_des.ps'
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
xyouts,.22,.96,'CIPS Level 2 Version 4.20',/normal,color=0,charsize=2
solday=173L
nr=181L
latbin=-90.+findgen(nr)
asc_tot=fltarr(kday,nr)
des_tot=fltarr(kday,nr)
icount=-1L
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
      ofile=pth+sdate+'_albgt2_sza42-94.sav
      dum=findfile(ofile)
      icount=icount+1L
      print,sdate,' ',sday,' ',icount,iday-solday
      if dum(0) eq '' then goto,jump
;
; save file contents
; latitude_all,longitude_all,ut_time_all,sza_all,CLOUD_INDEX_all,ALB_all,ALB_ERR_all,RAD_all,RAD_ERR_all,IWC_all,IWC_ERR_all,ICD_all

      restore,ofile
;
; count up number per 1 degree latitude bin
;
for j=0L,nr-1L do begin
    ymin=latbin(j)-0.5
    ymax=latbin(j)+0.5
    index=where(alb_all ge 2. and latitude_orig_all gt 90. and latitude_all ge ymin and latitude_all lt ymax,nasc)
    if index(0) ne -1L then asc_tot(icount,j)=nasc
    index=where(alb_all ge 2. and latitude_orig_all le 90. and latitude_all ge ymin and latitude_all lt ymax,ndes)
    if index(0) ne -1L then des_tot(icount,j)=ndes
endfor
;
; color by albedo
;
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1+findgen(kday),latbin,color=0,title='Ascending Node',ytitle='Latitude',$
     xrange=[-40,80],yrange=[40,90.],/nodata,/noeras
index=where(alb_all ge 2. and alb_all le 3. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.1
index=where(alb_all gt 3. and alb_all le 4. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.2
index=where(alb_all gt 4. and alb_all le 5. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.3
index=where(alb_all gt 5. and alb_all le 10. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.4
index=where(alb_all gt 10. and alb_all le 20. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.5
index=where(alb_all gt 20. and alb_all le 30. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.6
index=where(alb_all gt 30. and alb_all le 40. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.7
index=where(alb_all gt 40. and alb_all le 50. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.8
index=where(alb_all gt 50. and latitude_orig_all gt 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.9

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1+findgen(kday),latbin,color=0,xtitle='DFS',title='Descending Node',ytitle='Latitude',$
     xrange=[-40,80],yrange=[40,90.],/nodata,/noeras
index=where(alb_all ge 2. and alb_all le 3. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.1
index=where(alb_all gt 3. and alb_all le 4. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.2
index=where(alb_all gt 4. and alb_all le 5. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.3
index=where(alb_all gt 5. and alb_all le 10. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.4
index=where(alb_all gt 10. and alb_all le 20. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.5
index=where(alb_all gt 20. and alb_all le 30. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.6
index=where(alb_all gt 30. and alb_all le 40. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.7
index=where(alb_all gt 40. and alb_all le 50. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.8
index=where(alb_all gt 50. and latitude_orig_all le 90.)
if index(0) ne -1L then oplot,iday-solday+ut_time_all(index)/24.,latitude_all(index),psym=3,color=mcolor*.9

goto,jump
;
; plot
;
plotit:
index=where(asc_tot eq 0.)
if index(0) ne -1L then asc_tot(index)=0./0.
asc_tot=smooth(asc_tot,3,/nan)
index=where(des_tot eq 0.)
if index(0) ne -1L then des_tot(index)=0./0.
des_tot=smooth(des_tot,3,/nan)
;
; overplot frequency
;
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1+findgen(kday),latbin,color=0,xrange=[-40,80],yrange=[40,90.],/nodata,/noeras
contour,asc_tot,-40.+findgen(kday),latbin,/overplot,levels=[1000.,10000.+10000.*findgen(10)],/follow,c_color=0,c_labels=1+intarr(11),thick=3
xyouts,-35.,42.,'2007',/data,color=0,charsize=2
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1+findgen(kday),latbin,color=0,xrange=[-40,80],yrange=[40,90.],/nodata,/noeras
contour,des_tot,-40.+findgen(kday),latbin,/overplot,levels=[1000.,10000.+10000.*findgen(10)],/follow,c_color=0,c_labels=1+intarr(11),thick=3
xyouts,-35.,42.,'2007',/data,color=0,charsize=2

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
    spawn,'convert -trim yt_alb_v4.2_2007_asc_des.ps -rotate -90 yt_alb_v4.2_2007_asc_des.jpg'
    spawn,'rm -f yt_alb_v4.2_2007_asc_des.ps'
 endif
end
