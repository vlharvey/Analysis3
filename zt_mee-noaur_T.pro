;
; at a given latitude, plot altitude time section of Tmee-Tnoaur
; NOT EVEN STARTED
;
@stddat
@kgmt
@ckday
@kdate

loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
!p.background=icolmax
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.20,0.20,0.20]
yorig=[0.75,0.45,0.15]
xlen=0.7
ylen=0.25
cbaryoff=0.05
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
sdir='/aura6/data/HIRDLS_data/Datfiles_SOSST/'
dira='/aura3/data/ACE_data/Datfiles_SOSST/v2.2/'
lstmn=1
lstdy=1
lstyr=2007
ledmn=4
leddy=1
ledyr=2007
lstday=0
ledday=0
;goto,quick
;
; restore year of ACE data
;
restore,dira+'cat_ace_v2.2.2007'
restore,dira+'no2_ace_v2.2.2007'
dateace_all=date
yace_all=latitude
xace_all=longitude
modea_all=sctype
no2ace_all=mix
;
; Ask interactive questions- get starting/ending date and p surface
;
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
icount=0L
kcount=0L

; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,plotit
      syr=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sdate=syr+smn+sdy
;
; restore HIRDLS
;
; DATE            LONG      =     20070101
; FDOY            DOUBLE    = Array[3494]
; ID              STRING    = Array[3494]
; LATITUDE        FLOAT     = Array[3494]
; LONGITUDE       FLOAT     = Array[3494]
; TIME            DOUBLE    = Array[3494]
; ALTITUDE        FLOAT     = Array[121]
; COMMENT         STRING    = Array[7]
; DENTOT          FLOAT     = Array[3494, 121]
; ID              STRING    = Array[3494]
; PRESSURE        FLOAT     = Array[3494, 121]
; TEMPERATURE     FLOAT     = Array[3494, 121]
; TEMPERATURE_ERROR
; TEMPERATURE_MASK
;
      dum=findfile(sdir+'tpd_hirdls_v2.04.19_'+sdate+'.sav')
      if dum(0) eq '' then goto,skip
      restore,sdir+'tpd_hirdls_v2.04.19_'+sdate+'.sav'
      restore,sdir+'cat_hirdls_v2.04.19_'+sdate+'.sav'	; latitude
      restore,sdir+'no2_hirdls_v2.04.19_'+sdate+'.sav'	; no2
      print,sdate
      nlv=n_elements(altitude)
      index=where(mask eq -99.)
      if index(0) ne -1L then mix(index)=-99.
      hirno2mix=mix 
;
; compute solar zenith angle for each HIRDLS profile
;
      doy=iday
      pi=3.14159265
      dtor=pi/180.
      earinc=23.5
      zangle=fltarr(n_elements(latitude))
      for ii=0L,n_elements(latitude)-1 do begin
          rlat=latitude(ii)
          rlon=longitude(ii)
          gmt=time(ii)
          sinlat=sin(rlat*dtor)
          coslat=sqrt(1.-sinlat^2.)
          sinlon=sin(rlon*dtor)
          coslon=cos(rlon*dtor)
          soya=(doy-81.25)*pi/182.5           ; day angle
          soha=2.*pi*(gmt-12.)/24.            ; hour angle
          soha=-soha
          sininc=sin(earinc*dtor)
          sindec=sininc*sin(soya)
          cosdec= sqrt(1.-sindec^2.)
          coszen=cos(soha)*coslon+sin(soha)*sinlon
          coszen=coszen*cosdec*coslat
          coszen=sindec*sinlat+coszen
          coszen=min([max([coszen,-1.]),1.])
          chi = acos(coszen)
          zangle(ii) = chi/dtor
      endfor
;erase
;!type=2^2+2^3
;set_viewport,.1,.9,.1,.9
;map_set,0,0,0,/contin,/grid,title=sdate,/noeras,color=0
;index=where(zangle ge 90.)
;oplot,longitude(index),latitude(index),psym=4,color=50
;index=where(zangle le 90.)
;oplot,longitude(index),latitude(index),psym=4,color=250
;stop
;
; declare time period arrays on first day
;
      if kcount eq 0L then begin
         hirpolarno2_zt_day=fltarr(kday,nlv)
         hirpolarno2_zt_night=fltarr(kday,nlv)
         acepolarno2_zt=fltarr(kday,nlv)
         sdate_all=strarr(kday)
         kcount=1
      endif
      sdate_all(icount)=sdate
;
; compute daily polar averages
;
      polarno2_day=fltarr(nlv)
      polarno2_night=fltarr(nlv)
      nno2prof_day=lonarr(nlv)
      nno2prof_night=lonarr(nlv)
      for ii=0L,n_elements(id)-1L do begin
          if latitude(ii) ge 60. and zangle(ii) le 80. then begin
             no2_prof=reform(hirno2mix(ii,*))
             good=where(no2_prof ne -99. and no2_prof lt 1.e-7,ngood)
             if good(0) ne -1L then begin
                polarno2_day(good)=polarno2_day(good)+reform(no2_prof(good))
                nno2prof_day(good)=nno2prof_day(good)+1L
             endif
          endif
          if latitude(ii) ge 60. and zangle(ii) gt 100. then begin
             no2_prof=reform(hirno2mix(ii,*))
             good=where(no2_prof ne -99. and no2_prof lt 1.e-7,ngood)
             if good(0) ne -1L then begin
                polarno2_night(good)=polarno2_night(good)+reform(no2_prof(good))
                nno2prof_night(good)=nno2prof_night(good)+1L
             endif
          endif
      endfor
      good=where(nno2prof_day gt 0L)
      if good(0) ne -1L then polarno2_day(good)=polarno2_day(good)/float(nno2prof_day(good))
      hirpolarno2_zt_day(icount,*)=polarno2_day
      good=where(nno2prof_night gt 0L)
      if good(0) ne -1L then polarno2_night(good)=polarno2_night(good)/float(nno2prof_night(good))
      hirpolarno2_zt_night(icount,*)=polarno2_night
;
; check 
;
erase
!type=2^2+2^3
set_viewport,.15,.85,.2,.85
plot,polarno2_day,altitude,color=0,/noerase,title=sdate+' NO!l2!n',xrange=[1.e-10,1.e-7],$
     yrange=[10.,60.],/xlog,thick=3
xyouts,1.2e-10,57.,'Day',color=250,charthick=3,/data,charsize=2
xyouts,1.2e-10,54.,'Nighttime',color=50,charthick=3,/data,charsize=2
xyouts,1.2e-10,51.,'Day+Night',color=0,charthick=3,/data,charsize=2
oplot,polarno2_day,altitude,color=250,thick=3
oplot,polarno2_night,altitude,color=50,thick=3
index=where(polarno2_day ne 0. and polarno2_night ne 0.)
polarno2_all=0.*polarno2_night
if index(0) ne -1L then begin
   polarno2_all(index)=polarno2_day(index)+polarno2_night(index)
   oplot,polarno2_all,altitude,color=0,thick=3
endif
skip:
;
; extract ACE data today
;
      index=where(dateace_all eq date,nace)
      if index(0) eq -1L then goto,skipace
      yace_day=yace_all(index)
      aceno2mix=no2ace_all(index,*)
;
; ACE polar
;
      polarno2=fltarr(nlv)
      nno2prof=lonarr(nlv)
      for ii=0L,nace-1L do begin
          if yace_day(ii) ge 50. then begin
             no2_prof=reform(aceno2mix(ii,*))
             good=where(no2_prof ne -99.,ngood)
             if good(0) ne -1L then begin
                polarno2(good)=polarno2(good)+reform(no2_prof(good))
                nno2prof(good)=nno2prof(good)+1L
             endif
          endif
      endfor
      good=where(nno2prof gt 0L)
      if good(0) ne -1L then polarno2(good)=polarno2(good)/float(nno2prof(good))
      if good(0) ne -1L then oplot,polarno2(good),altitude(good),psym=1,color=0
      acepolarno2_zt(icount,*)=polarno2
skipace:

      icount=icount+1L
goto,jump

plotit:
index=where(hirpolarno2_zt_day ge 1.e-7 or hirpolarno2_zt_day eq 0.)
if index(0) ne -1L then hirpolarno2_zt_day(index)=0./0.
hirpolarno2_zt_day=hirpolarno2_zt_day*1.e9
hirpolarno2_zt_day=smooth(hirpolarno2_zt_day,7,/NaN,/edge_truncate)
index=where(finite(hirpolarno2_zt_day) ne 1)
hirpolarno2_zt_day(index)=0.
index=where(hirpolarno2_zt_day gt 1000.)
if index(0) ne -1L then hirpolarno2_zt_day(index)=0.
index=where(hirpolarno2_zt_night ge 1.e-7 or hirpolarno2_zt_night eq 0.)
if index(0) ne -1L then hirpolarno2_zt_night(index)=0./0.
hirpolarno2_zt_night=hirpolarno2_zt_night*1.e9
hirpolarno2_zt_night=smooth(hirpolarno2_zt_night,7,/NaN,/edge_truncate)
index=where(finite(hirpolarno2_zt_night) ne 1)
hirpolarno2_zt_night(index)=0.
index=where(hirpolarno2_zt_night gt 1000.)
if index(0) ne -1L then hirpolarno2_zt_night(index)=0.

index=where(acepolarno2_zt ge 1.e-7 or acepolarno2_zt eq 0.)
if index(0) ne -1L then acepolarno2_zt(index)=0./0.
acepolarno2_zt=acepolarno2_zt*1.e9
acepolarno2_zt=smooth(acepolarno2_zt,7,/NaN,/edge_truncate)
index=where(finite(acepolarno2_zt) ne 1)
acepolarno2_zt(index)=0.
index=where(acepolarno2_zt gt 1000.)
if index(0) ne -1L then acepolarno2_zt(index)=0.
save,file='zt_hirdls_polarno2+aceno2_2007_day.sav',hirpolarno2_zt_day,hirpolarno2_zt_night,$
     acepolarno2_zt,kday,altitude,sdate_all
quick:
restore,'zt_hirdls_polarno2+aceno2_2007_day.sav'
sdate0=sdate_all(0)
sdate1=sdate_all(n_elements(sdate_all)-1)
syear=strmid(sdate_all,0,4)
smon=strmid(sdate_all,4,2)
sday=strmid(sdate_all,6,2)
xindex=where(sday eq '15',nxticks)
xlabs=smon(xindex)
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='zt_hirdls_polarno2+aceno2_'+syear(0)+'_day.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize
endif
;
; plot Arctic means
;
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nlvls=17
col1=1+indgen(nlvls)*icolmax/nlvls
level=2.5*findgen(nlvls)
contour,hirpolarno2_zt_day,1.+findgen(kday),altitude,/noeras,xrange=[1.,kday],yrange=[15.,57.],$
      charsize=1.5,color=0,ytitle='Altitude (km)',title='Daytime HIRDLS (color); ACE (white)',/fill,c_color=col1,$
      levels=level,xticks=nxticks-1,xtickname=xlabs,xtickv=xindex,min_value=0.
contour,hirpolarno2_zt_day,1.+findgen(kday),altitude,levels=level,color=mcolor,/follow,/overplot,c_labels=1+fltarr(nlvls)
contour,acepolarno2_zt,1.+findgen(kday),altitude,levels=[0.1,0.5+0.5*findgen(15)],color=mcolor,/follow,/overplot,$
        min_value=0.,c_labels=1+0*indgen(nlvls),thick=5
xyouts,xmn+0.02,ymn+0.02,syear(0),/normal,color=0,charsize=3,charthick=3

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nlvls=17
col1=1+indgen(nlvls)*icolmax/nlvls
level=2.5*findgen(nlvls)
contour,hirpolarno2_zt_night,1.+findgen(kday),altitude,/noeras,xrange=[1.,kday],yrange=[15.,57.],$
      charsize=1.5,color=0,ytitle='Altitude (km)',title='Night HIRDLS (color); ACE (white)',/fill,c_color=col1,$
      levels=level,xticks=nxticks-1,xtickname=xlabs,xtickv=xindex,min_value=0.
contour,hirpolarno2_zt_night,1.+findgen(kday),altitude,levels=level,color=mcolor,/follow,/overplot,c_labels=1+fltarr(nlvls)
contour,acepolarno2_zt,1.+findgen(kday),altitude,levels=[0.1,0.5+0.5*findgen(15)],color=mcolor,/follow,/overplot,$
        min_value=0.,c_labels=1+0*indgen(nlvls),thick=5
xyouts,xmn+0.02,ymn+0.02,syear(0),/normal,color=0,charsize=3,charthick=3
imin=min(level)
imax=max(level)
ymnb=yorig(1) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='7-day Avg NO!l2!n > 60 N (ppbv)',charsize=1.5
ybox=[0,10,10,0,0]
x1=imin
dx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+dx,x1+dx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+dx
endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim zt_hirdls_polarno2+aceno2_'+syear(0)+'_day.ps -rotate -90 zt_hirdls_polarno2+aceno2_'+syear(0)+'_day.jpg'
    endif
end
