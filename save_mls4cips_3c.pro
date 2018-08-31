;
; save MLS T and H2O for each CIPS level 3c summary file
; VLH 10/10/2011
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
usersym,cos(a),sin(a),/fill
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.15,0.15,0.55,0.55]
yorig=[0.55,0.15,0.55,0.15]
xlen=0.225
ylen=0.3
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
mdir='/Volumes/earth/aura6/data/MLS_data/Datfiles_SOSST/'
cdir='/Volumes/Data/CIPS_data/Datfiles/cips_3c_'
odir='/Volumes/Data/CIPS_data/Datfiles_MLS_DMP/cips_3c_mls_'
;
; Ask interactive questions- get starting/ending date and p surface
;
shem='n'
lstyr=9
ialb=1
read,' Enter hemisphere (n,s) ',shem
read,' Enter PMC onset year (yy) ',lstyr
;read,' Enter Albedo threshhold (1,2,5,10) ',ialb
salb=string(format='(i2.2)',ialb)+'G'
if shem eq 'n' then begin
   shem='north'
   syr=string(format='(i2.2)',lstyr)
endif
if shem eq 's' then begin
   shem='south'
   syr=string(format='(i2.2)',lstyr)+string(format='(i2.2)',lstyr+1)
endif
;
; restore CIPS season
;
; ALB             FLOAT     = Array[589, 70]
; ALB_STD         FLOAT     = Array[589, 70]
; DFS             LONG      = Array[589]
; DOY             INT       = Array[589]
; IWC             FLOAT     = Array[589, 70]
; IWC_STD         FLOAT     = Array[589, 70]
; LATHI           INT       = Array[70]
; LATLO           INT       = Array[70]
; LON             FLOAT     = Array[589, 70]
; LTIME           FLOAT     = Array[589, 70]
; NBIN            INT       =       70
; NREV            LONG      =          589
; NUM_CLD         INT       = Array[589, 70]
; NUM_OBS         INT       = Array[589, 70]
; RAD             FLOAT     = Array[589, 70]
; RAD_STD         FLOAT     = Array[589, 70]
; REV             INT       = Array[589]
; SZA             FLOAT     = Array[589, 70]
; UT              FLOAT     = Array[589, 70]
; YEAR            INT       = Array[589]
;
restore,cdir+shem+'_'+syr+'_v04.20_r04_'+salb+'_cld.sav'
print,'restored '+cdir+shem+'_'+syr+'_v04.20_r04_'+salb+'_cld.sav'
mls_tp=-999.+0.*alb
mls_h2o=-999.+0.*alb
if shem eq 'south' then latlo=-1.*latlo
if shem eq 'south' then lathi=-1.*lathi
;
LIM=-999.
;SZALIM=91.	; DATA WITH SZA > SZALIM ARE BAD (IN NH THIS CAN ONLY HAPPEN ON THE ASCENDING NODE)
SZALIM=180.	; DON'T GET RID OF ANY DATA BASED ON SZA.
;
; loop over DOYs in the cips summary file
;
n=findgen(nrev)
n1=1+findgen(nrev)
index=where(doy(n)-doy(n1) ne 0)
days=[doy(index),doy(nrev-1)]
years=[year(index),year(nrev-1)]
nday=n_elements(days)
;
; days array has to be monotonically increasing
;
index=where(days lt 100.)
if index(0) ne -1L then days(index)=days(index)+max(days)
;
; loop over days
;
icount=0L
for iday=days(0),days(nday-1) do begin
;
; adjust iday back to doy
;
    kday=iday
    if year(0) mod 4 eq 0 then begin
       if iday gt 365 then kday=iday-365L
    endif
    if year(0) mod 4 ne 0 then begin
       if iday gt 366 then kday=iday-366L
    endif
;
; check for data on this day
;
    today=where(doy eq kday,nprof)
    if today(0) eq -1L then goto,jumpday

    kdate,float(kday),years(icount),imn,idy
    ckday,kday,years(icount)
    z = stddat(imn,idy,years(icount),ndays)
    syrr=string(FORMAT='(I4)',years(icount))
    smn=string(FORMAT='(I2.2)',imn)
    sdy=string(FORMAT='(I2.2)',idy)
    sday=string(FORMAT='(I3.3)',kday)
    sdate=syrr+smn+sdy
    print,sdate,' ',dfs(today(0))
;
; extract position information this day of orbit data
;
    sza_today=sza(today,*)	; (nprof,nlat)
    bad=where(sza_today eq -999.)
    if bad(0) ne -1L then sza_today(bad)=0./0.
    UT_today=ut(today,*)
    lon_today=lon(today,*)
    good=where(lon_today ne -999.)
    if good(0) eq -1L then goto,jumpday
;
; read MLS data today and apply mask
;
    dum=findfile(mdir+'cat_mls_v3.3_'+sdate+'.sav')
    if dum(0) eq '' then goto,jumpday
    restore,mdir+'cat_mls_v3.3_'+sdate+'.sav'
    restore,mdir+'h2o_mls_v3.3_'+sdate+'.sav'
    restore,mdir+'tpd_mls_v3.3_'+sdate+'.sav'
    index=where(mask eq -99.)
    if index(0) ne -1L then mix(index)=-99.
    mh2o=mix
    good=where(mh2o ne -99.)
    mh2o(good)=mh2o(good)*1.e6
    index=where(temperature_mask eq -99.)
    if index(0) ne -1L then temperature(index)=-99.
    mtemp=temperature
    mpress=pressure
    mprof=n_elements(longitude)
    mlev=n_elements(altitude)
    muttime=time
    mlat=latitude
    mlon=longitude
;
; compute solar zenith angle for each MLS profile
;
    pi=3.14159265
    dtor=pi/180.
    earinc=23.5
    msza=-99.+0*fltarr(mprof)
    for ii=0L,mprof-1 do begin
        rlat=mlat(ii)
        rlon=mlon(ii)
        gmt=muttime(ii)
        sinlat=sin(rlat*dtor)
        coslat=sqrt(1.-sinlat^2.)
        sinlon=sin(rlon*dtor)
        coslon=cos(rlon*dtor)
        soya=(kday-81.25)*pi/182.5           ; day angle
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
        msza(ii) = chi/dtor
    endfor
;
; check
;
;   erase
;   xyouts,.4,.9,sdate,charsize=2,color=0,/normal
;   xmn=xorig(0)
;   xmx=xorig(0)+xlen
;   ymn=yorig(0)
;   ymx=yorig(0)+ylen
;   set_viewport,xmn,xmx,ymn,ymx
;   !type=2^2+2^3
;   if shem eq 'north' then map_set,90,0,0,/ortho,/contin,/grid,title='CIPS',color=0,limit=[50.,0.,80.,360.],/noerase
;   if shem eq 'south' then map_set,-90,0,0,/ortho,/contin,/grid,title='CIPS',color=0,limit=[-50.,0.,-80.,360.],/noerase
;   utmin=0. & utmax=24.
;   utmean=0
;   for ii=0L,nprof-1L do begin
;       good=where(ut_today(ii,*) ne -999.)
;       if good(0) eq -1L then goto,jumporbit
;       if good(0) ne -1L then utmean=mean(ut_today(ii,good))
;       good=where(lon_today(ii,*) ne -999.)
;       oplot,lon_today(ii,good),latlo(good),psym=8,color=mcolor*(utmean/utmax)
;       jumporbit:
;   endfor
;   ymxb=ymn-cbaryoff
;   ymnb=ymxb+cbarydel
;   set_viewport,xmn,xmx,ymnb,ymxb
;   imin=utmin
;   imax=utmax
;   !type=2^2+2^3+2^6
;   plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Local Time (hours)'
;   ybox=[0,10,10,0,0]
;   x2=imin
;   nlvls=11
;   col1=1+indgen(nlvls)*mcolor/nlvls
;   dx=(imax-imin)/(float(nlvls)-1)
;   for j=1,nlvls-1 do begin
;       xbox=[x2,x2,x2+dx,x2+dx,x2]
;       polyfill,xbox,ybox,color=col1(j)
;       x2=x2+dx
;   endfor
;
;   ALB_today=alb(today,*)
;   SZA_today=sza(today,*)
;   good=WHERE(ALB_today ne lim and lon_today ne lim and SZA_today ne lim and SZA_today le SZALIM,ngood)
;    
;
; eliminate bad UT times and SH
;
    if shem eq 'north' then index=where(muttime gt 0. and mlat ge min(latlo)-4. and mlat le max(lathi)+4.,mprof)
    if shem eq 'south' then index=where(muttime gt 0. and mlat le max(latlo)+4. and mlat ge min(lathi)-4.,mprof)
    if index(0) eq -1L then goto,jumpday
    muttime=reform(muttime(index))
    mlat=reform(mlat(index))
    mlon=reform(mlon(index))
    mtemp=reform(mtemp(index,*))
    mh2o=reform(mh2o(index,*))
    msza=reform(msza(index))
;
; change -99 to NaN
;
    bad=where(mtemp eq -99.)
    if bad(0) ne -1L then mtemp(bad)=0./0.
    bad=where(mh2o eq -99.)
    if bad(0) ne -1L then mh2o(bad)=0./0.
    bad=where(msza eq -99.)
    if bad(0) ne -1L then msza(bad)=0./0.
;
;   xmn=xorig(1)
;   xmx=xorig(1)+xlen
;   ymn=yorig(1)
;   ymx=yorig(1)+ylen
;   set_viewport,xmn,xmx,ymn,ymx
;   !type=2^2+2^3
;   if min(mlat) gt 0. then map_set,90,0,0,/ortho,/contin,/grid,title='MLS',color=0,limit=[50.,0.,80.,360.],/noerase
;   if min(mlat) lt 0. then map_set,-90,0,0,/ortho,/contin,/grid,title='MLS',color=0,limit=[-50.,0.,-80.,360.],/noerase
;   for ii=0L,mprof-1L do $
;       oplot,[mlon(ii),mlon(ii)],[mlat(ii),mlat(ii)],psym=8,color=((muttime(ii)-utmin)/(utmax-utmin))*mcolor,symsize=0.75
;   ymxb=ymn-cbaryoff
;   ymnb=ymxb+cbarydel
;   set_viewport,xmn,xmx,ymnb,ymxb
;   imin=utmin
;   imax=utmax
;   !type=2^2+2^3+2^6
;   plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Local Time (hours)'
;   ybox=[0,10,10,0,0]
;   x2=imin
;   nlvls=11
;   col1=1+indgen(nlvls)*mcolor/nlvls
;   dx=(imax-imin)/(float(nlvls)-1)
;   for j=1,nlvls-1 do begin
;       xbox=[x2,x2,x2+dx,x2+dx,x2]
;       polyfill,xbox,ybox,color=col1(j)
;       x2=x2+dx
;   endfor
;
; loop over CIPS latitude bins
; 
    for j=0,nbin-1L do begin
        if lathi(j) gt latlo(j) then begin
           y0=latlo(j)
           y1=lathi(j)
        endif
        if lathi(j) lt latlo(j) then begin
           y0=lathi(j)
           y1=latlo(j)
        endif
        index=where(mlat ge y0 and mlat lt y1,nn)
        if index(0) ne -1L and finite(min(sza_today(*,j),/nan)) eq 0 and finite(max(sza_today(*,j),/nan)) eq 0 then begin
;print,'CIPS SZA at Lat= ',y0,min(sza_today(*,j),/nan),mean(sza_today(*,j),/nan),max(sza_today(*,j),/nan)
;print,'MLS SZA at Lat= ',y0,min(msza(index),/nan),max(msza(index),/nan)
           mls_tp(today,j)=mean(mtemp(index,82:86),/NaN)
           mls_h2o(today,j)=mean(mh2o(index,82:86),/NaN)
        endif
        if index(0) ne -1L and finite(min(sza_today(*,j),/nan)) ne 0 and finite(max(sza_today(*,j),/nan)) ne 0 then begin
;print,'CIPS SZA at Lat= ',y0,min(sza_today(*,j),/nan),mean(sza_today(*,j),/nan),max(sza_today(*,j),/nan)
           good=where(abs(msza(index)-mean(sza_today(*,j))) le 10.) 	; require MLS SZA to be within 10 degrees of CIPS mean SZA 
;print,'MLS SZA at Lat= ',y0,min(msza(index(good)),/nan),max(msza(index(good)),/nan)
           mls_tp(today,j)=mean(mtemp(index(good),82:86),/NaN)
           mls_h2o(today,j)=mean(mh2o(index(good),82:86),/NaN)
        endif
    endfor
    icount=icount+1L
    jumpday:
endfor
;
; postscript file
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='save_mls4cips_3c_'+syr+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif
;
; seasonal plots
;
index=where(mls_tp eq -999.)
if index(0) ne -1L then mls_tp(index)=0./0.
index=where(mls_h2o eq -999.)
if index(0) ne -1L then mls_h2o(index)=0./0.

rlat=75
if shem eq 'south' then rlat=-75
yindex=where(latlo eq rlat)
xyouts,.35,.925,syr+' at '+strcompress(long(rlat),/remove_all)+' Deg',/normal,charsize=2,color=0
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(0)),psym=8,yrange=[0.,40.],xtitle='DFS',color=0,title='ASC',ytitle='Albedo'
axis,/yax,/save,yrange=[120.,170.],color=90,ytitle='MLS Temperature',yminor=1
oplot,dfs,mls_tp(*,yindex(0)),psym=8,color=90,symsize=0.75
oplot,dfs,mls_tp(*,yindex(0)),thick=2,color=90
;
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(0)),psym=8,yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'      ; 75 degrees latitude
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='MLS Water Vapor',yminor=1
oplot,dfs,mls_h2o(*,yindex(0)),psym=8,color=90,symsize=0.75
oplot,dfs,mls_h2o(*,yindex(0)),thick=2,color=90

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,yrange=[0.,40.],xtitle='DFS',color=0,title='DES',ytitle='Albedo'        
axis,/yax,/save,yrange=[120.,170.],color=90,ytitle='MLS Temperature',yminor=1
oplot,dfs,mls_tp(*,yindex(1)),psym=8,color=90,symsize=0.75
oplot,dfs,mls_tp(*,yindex(1)),thick=2,color=90
;
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'      ; 75 degrees latitude
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='MLS Water Vapor',yminor=1
oplot,dfs,mls_h2o(*,yindex(1)),psym=8,color=90,symsize=0.75
oplot,dfs,mls_h2o(*,yindex(1)),thick=2,color=90


if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim save_mls4cips_3c_'+syr+'.ps -rotate -90 save_mls4cips_3c_'+syr+'.jpg'
;  spawn,'rm -f save_mls4cips_3c_'+sdate+'.ps'
endif

;
; SAVE MLS T and H2O for corresponding CIPS summary file
;
index=where(finite(mls_tp) eq 0)
if index(0) ne -1L then mls_tp(index)=-999.
index=where(finite(mls_h2o) eq 0)
if index(0) ne -1L then mls_h2o(index)=-999.

comment=strarr(2)
comment(0)='MLS T and H2O averaged between 82 and 86 km'
comment(1)='For each CIPS latitude bin, MLS SZA <= 10 degrees of the mean CIPS SZA'
ofile=odir+shem+'_'+syr+'_v04.20_r04.sav'
print,'saved '+ofile
save,file=ofile,mls_tp,mls_h2o,latlo,lathi,doy,dfs,comment
end
