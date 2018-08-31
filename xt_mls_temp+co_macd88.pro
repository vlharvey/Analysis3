;
; Hovmoller of MLS temperature and CO
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
xorig=[0.25]
yorig=[0.20]
xlen=0.5
ylen=0.7
cbaryoff=0.12
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
mdir='/Volumes/earth/aura6/data/MLS_data/Datfiles_SOSST/'
lstmn=5
lstdy=1
lstyr=2012
ledmn=5
leddy=31
ledyr=2012
lstday=0
ledday=0
;
; Ask interactive questions- get starting/ending date and p surface
;
read,' Enter starting year ',lstyr
;read,' Enter ending year ',ledyr
ledyr=lstyr
if lstyr lt 91 then lstyr=lstyr+2000
if ledyr lt 91 then ledyr=ledyr+2000
if lstyr lt 1900 then lstyr=lstyr+1900
if ledyr lt 1900 then ledyr=ledyr+1900
if lstyr lt 1991 then stop,'Year out of range '
if ledyr lt 1991 then stop,'Year out of range '
minyear=lstyr
maxyear=ledyr
yearlab=strcompress(maxyear,/remove_all)
if minyear ne maxyear then yearlab=strcompress(minyear,/remove_all)+'-'+strcompress(maxyear,/remove_all)
;goto,quick

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
;
; longitude grid
;
dx=15.
nc=long(360./dx)+1
longrid=dx*findgen(nc)

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
; restore MLS CO on this day
; ALTITUDE        FLOAT     = Array[121]
; COMMENT         STRING    = Array[4]
; DATE            LONG      =     20070101
; ERR             FLOAT     = Array[3491, 121]
; FDOY            FLOAT     = Array[3491]
; ID              STRING    = Array[3491]
; LATITUDE        FLOAT     = Array[3491]
; LONGITUDE       FLOAT     = Array[3491]
; MASK            FLOAT     = Array[3491, 121]
; MIX             FLOAT     = Array[3491, 121]
; TIME            FLOAT     = Array[3491]
;
      dum=findfile(mdir+'cat_mls_v3.3_'+sdate+'.sav')
      if dum(0) eq '' then goto,skipmls
      restore,mdir+'cat_mls_v3.3_'+sdate+'.sav'
      restore,mdir+'co_mls_v3.3_'+sdate+'.sav'
      restore,mdir+'tpd_mls_v3.3_'+sdate+'.sav'
      print,sdate
;
; apply mask
;
      index=where(mask eq -99.)
      if index(0) ne -1L then mix(index)=-99.
      mlscomix=mix
      index=where(temperature_mask eq -99.)
      if index(0) ne -1L then temperature(index)=-99.
      mlscomix=mix
      nlv=n_elements(altitude)
;
; declare time period arrays on first day
;
      if kcount eq 0L then begin
         print,altitude
         ralt=83.
;        read,'Enter desired altitude ',ralt
         index=where(abs(altitude-ralt) eq min(abs(altitude-ralt)))
         ilev=index(0)
         salt=strcompress(long(ralt),/remove_all)
         mlspolarco_xt=fltarr(nc,kday)
         mlspolartp_xt=fltarr(nc,kday)
         sdate_all=strarr(kday)
         kcount=1
      endif
      sdate_all(icount)=sdate
;
; compute polar CO and temp
;
      mlspolarco=fltarr(nc)
      mlsncoprof=lonarr(nc)
      mlspolartp=fltarr(nc)
      mlsntpprof=lonarr(nc)
      for ii=0L,n_elements(id)-1L do begin
          if latitude(ii) ge 80. then begin
             for i=0L,nc-2L do begin
                 if longitude(ii) ge longrid(i) and longitude(ii) le longrid(i+1) and mlscomix(ii,ilev) ne -99. then begin
                    mlspolarco(i)=mlspolarco(i)+mlscomix(ii,ilev)
                    mlsncoprof(i)=mlsncoprof(i)+1L
                 endif
                 if longitude(ii) ge longrid(i) and longitude(ii) le longrid(i+1) and temperature(ii,ilev) ne -99. then begin
                    mlspolartp(i)=mlspolartp(i)+temperature(ii,ilev)
                    mlsntpprof(i)=mlsntpprof(i)+1L
                 endif
             endfor
          endif
      endfor
      good=where(mlsncoprof gt 0L)
      if good(0) ne -1L then mlspolarco(good)=mlspolarco(good)/float(mlsncoprof(good))
      good=where(mlsntpprof gt 0L)
      if good(0) ne -1L then mlspolartp(good)=mlspolartp(good)/float(mlsntpprof(good))
      mlspolarco_xt(*,icount)=mlspolarco
      mlspolartp_xt(*,icount)=mlspolartp
skipmls:
      icount=icount+1L
goto,jump

plotit:
;
; wrap around point
;
mlspolarco_xt(nc-1,*)=mlspolarco_xt(0,*)
mlspolartp_xt(nc-1,*)=mlspolartp_xt(0,*)
;
; interpolate small gaps in time
;
for k=0,nc-1 do begin
    dlev=reform(mlspolarco_xt(k,*))
    for i=1,kday-1 do begin
        if dlev(i) eq 0. and dlev(i-1) ne 0. then begin
           for ii=i+1,kday-1 do begin
               naway=float(ii-i)
               if naway le 5.0 and dlev(ii) ne 0. then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump1
               endif
           endfor
jump1:
        endif
    endfor
    mlspolarco_xt(k,*)=dlev
    dlev=reform(mlspolartp_xt(k,*))
    for i=1,kday-1 do begin
        if dlev(i) eq 0. and dlev(i-1) ne 0. then begin
           for ii=i+1,kday-1 do begin
               naway=float(ii-i)
               if naway le 5.0 and dlev(ii) ne 0. then begin
                  dlev(i)=(naway*dlev(i-1)+dlev(ii))/(naway+1.0)
                  goto,jump2
               endif
           endfor
jump2:
        endif
    endfor
    mlspolartp_xt(k,*)=dlev
endfor
;
; year date label
;
syear=strmid(sdate_all,0,4)
good=where(long(syear) ne 0L)
minyear=long(min(long(syear(good))))
maxyear=long(max(long(syear)))
yearlab=strcompress(maxyear,/remove_all)
if minyear ne maxyear then yearlab=strcompress(minyear,/remove_all)+'-'+strcompress(maxyear,/remove_all)
mlspolarco_xt=mlspolarco_xt*1.e6
;
; save temp, co, etc
;
save,file='xt_mls_temp+co_'+yearlab+'.sav',mlspolarco_xt,mlspolartp_xt,kday,longrid,sdate_all
quick:
restore,'xt_mls_temp+co_'+yearlab+'.sav'
sdate0=sdate_all(0)
sdate1=sdate_all(n_elements(sdate_all)-1)
syear=strmid(sdate_all,0,4)
smon=strmid(sdate_all,4,2)
sday=strmid(sdate_all,6,2)
xindex=where(sday eq '01' or sday eq '05' or sday eq '10' or sday eq '15' or sday eq '20' or sday eq '25' or sday eq '30',nxticks)
xlabs=smon(xindex)+'/'+sday(xindex)
good=where(long(syear) ne 0L)
minyear=long(min(long(syear(good))))
maxyear=long(max(long(syear)))
yearlab=strcompress(maxyear,/remove_all)
if minyear ne maxyear then yearlab=strcompress(minyear,/remove_all)+'-'+strcompress(maxyear,/remove_all)
;
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='xt_mls_temp+co_'+yearlab+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif
;
; plot Arctic mean temperature and CO
;
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3+2^7	; ticks outward
level=[0.001,0.01,0.025,0.05,0.1,0.25,0.5,1.]	;,2.,3.,4.,5.,6.,7.,8.,9.,10.,15.]
index=where(mlspolarco_xt eq 0.)
if index(0) ne -1L then mlspolarco_xt(index)=0./0.
mlspolarco_xt=smooth(mlspolarco_xt,3,/NaN,/edge_truncate)
if index(0) ne -1L then mlspolarco_xt(index)=0./0.
index=where(mlspolartp_xt eq 0.)
if index(0) ne -1L then mlspolartp_xt(index)=0./0.
mlspolartp_xt=smooth(mlspolartp_xt,3,/NaN,/edge_truncate)
if index(0) ne -1L then mlspolartp_xt(index)=0./0.

tlevel=130.+2.5*findgen(23)
nlvls=n_elements(tlevel)
col1=1+indgen(nlvls)*icolmax/nlvls
contour,mlspolartp_xt,longrid,1.+findgen(kday),/noeras,yrange=[kday,1.],xrange=[0.,360.],$
      charsize=1.5,color=0,xtitle='Longitude',/cell_fill,c_color=col1,title=yearlab+' 80-82N 83km',$
      levels=tlevel,yticks=nxticks-1,ytickname=xlabs,ytickv=xindex,min_value=-99.
contour,mlspolartp_xt,longrid,1.+findgen(kday),levels=tlevel,color=0,/follow,/overplot,c_labels=fltarr(nlvls)
contour,mlspolartp_xt,longrid,1.+findgen(kday),levels=[150.],color=mcolor,/follow,/overplot,c_labels=[1],thick=4
;contour,mlspolarco_xt,longrid,1.+findgen(kday),levels=[0.05,0.1,0.25,0.5,1,2,5,7.5],color=mcolor,/follow,/overplot,$
;        max_value=99.,c_labels=[1,1,1,1,1,1,1,1],thick=7
imin=min(tlevel)
imax=max(tlevel)
ymnb=yorig(0) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='MLS Temperature (K)'
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
       spawn,'convert -trim xt_mls_temp+co_'+yearlab+'.ps -rotate -90 xt_mls_temp+co_'+yearlab+'.png'
    endif
end
