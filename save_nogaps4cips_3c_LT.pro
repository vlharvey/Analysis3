;
; instead of separating ASC/DES by SZA, use LT
; save NOGAPS T and H2O for each CIPS level 3c summary file
; VLH 11/03/2011
;
@stddat
@kgmt
@ckday
@kdate
@mkltime
@read_nav_macd88

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
ndir='/Volumes/earth/harvey/NOGAPS_Alpha/Datfiles/'
cdir='/Volumes/Data/CIPS_data/Datfiles/cips_3c_'
odir='/Volumes/Data/CIPS_data/Datfiles_NOGAPS_DMP/cips_3c_nogaps_'
;
; Ask interactive questions- get starting/ending date and p surface
;
shem='n'
lstyr=7
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
restore,cdir+shem+'_'+syr+'_v04.20_r04_'+salb+'_all.sav'
print,'restored '+cdir+shem+'_'+syr+'_v04.20_r04_'+salb+'_all.sav'
nogaps_tp=-999.+0.*alb
nogaps_h2o=-999.+0.*alb
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
; compute DFS
;
dfs=0.*doy
if shem eq 'north' then dfs=doy-172.			; June 21
if shem eq 'south' then begin
   index=where(doy gt 100.)
   dfs(index)=doy(index)-355.				; Dec 21
   index=where(doy lt 100.)
   dfs(index)=doy(index)+max(doy)-355. 
endif
;
; NOGAPS-Alpha variables
;
svars=['H2O','T','Z']
nvars=n_elements(svars)
;
; loop over days
;
icount=0L
for iday=days(0),days(nday-1L) do begin		; does not account for missing CIPS data!
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
    kdate,float(kday),years(icount),imn,idy
    ckday,kday,years(icount)
    z = stddat(imn,idy,years(icount),ndays)
    syrr=string(FORMAT='(I4)',years(icount))
    smn=string(FORMAT='(I2.2)',imn)
    sdy=string(FORMAT='(I2.2)',idy)
    sday=string(FORMAT='(I3.3)',kday)
    sdate=syrr+smn+sdy
;
; check for data on this day
;
    today=where(doy eq kday,nprof)
    if today(0) eq -1L then begin
       print,'Missing CIPS data on '+sdate
       goto,jumpcount
    endif
    lt_today=ltime(today,*)

;   print,nday,icount,' ',sdate,' ',doy(today(0)),dfs(today(0))
;
; read NOGAPS data today 
;
    for ivar=0L,nvars-1L do begin
        filename=ndir+svars(ivar)+'_'+sdate+'12_360x181x60_aim9c.NAV'
        dum=findfile(filename)
        if dum(0) eq '' then begin
           print,'Missing '+sdate+' '+svars(ivar)
           goto,jumpday
        endif
        data=read_NAV_macd88(filename,lat=alat,lon=alon,p=p,yyyymmddhh=yyyymmddhh)
;       print,filename+' '+svars(ivar),' ',min(data),max(data)
        if svars(ivar) eq 'H2O' then h2o3d=data
        if svars(ivar) eq 'T' then tp3d=data
        if svars(ivar) eq 'Z' then z3d=data/1000.
    endfor  ; loop over variables
;
; reform into 2d arrays as MLS
;
    nr=n_elements(alat)
    nc=n_elements(alon)
    nl=n_elements(p)
    mprof=nr*nc
    mh2o=fltarr(mprof,nl)
    mtemp=fltarr(mprof,nl)
    mheight=fltarr(mprof,nl)
    mlat=fltarr(mprof)
    mlon=fltarr(mprof)
    ncount=0L
    for i=0L,nc-1L do begin
        for j=0L,nr-1L do begin
            mh2o(ncount,*)=h2o3d(i,j,*)
            mtemp(ncount,*)=tp3d(i,j,*)
            mheight(ncount,*)=z3d(i,j,*)
            mlon(ncount)=alon(i)
            mlat(ncount)=alat(j)
            ncount=ncount+1L
        endfor
    endfor
;
; compute local time
;
    muttime=12.+0.*mlon
    mltime=0.*mlon
    mldoy=kday+0.*mlon
    mkltime,muttime,mlon,mltime,mldoy
;
; compute solar zenith angle for each NOGAPS profile
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
; eliminate bad UT times opposite hemisphere
;
    if shem eq 'north' then index=where(muttime gt 0. and mlat ge min(latlo)-4. and mlat le max(lathi)+4.,mprof)
    if shem eq 'south' then index=where(muttime gt 0. and mlat le max(latlo)+4. and mlat ge min(lathi)-4.,mprof)
    if index(0) eq -1L then goto,jumpday
    muttime=reform(muttime(index))
    mlat=reform(mlat(index))
    mlon=reform(mlon(index))
    mtemp=reform(mtemp(index,*))
    mh2o=reform(mh2o(index,*))
    mheight=reform(mheight(index,*))
    msza=reform(msza(index))
    mltime=reform(mltime(index))
    mldoy=reform(mldoy(index))
;
;   xmn=xorig(1)
;   xmx=xorig(1)+xlen
;   ymn=yorig(1)
;   ymx=yorig(1)+ylen
;   set_viewport,xmn,xmx,ymn,ymx
;   !type=2^2+2^3
;   if min(mlat) gt 0. then map_set,90,0,0,/ortho,/contin,/grid,title='NOGAPS',color=0,limit=[50.,0.,80.,360.],/noerase
;   if min(mlat) lt 0. then map_set,-90,0,0,/ortho,/contin,/grid,title='NOGAPS',color=0,limit=[-50.,0.,-80.,360.],/noerase
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
; indices 0-34: NOGAPS local time should be < 6 or > 18
; indices 35-69: NOGAPS local time should be between 6 and 18
; 
nbin2=nbin/2 	; 35
    for j=0,nbin-1L do begin
        if j lt nbin2 then begin
           mltime0=6.
           mltime1=18.
        endif
        if j ge nbin2 then begin
           mltime0=6.
           mltime1=18.
        endif
        if lathi(j) gt latlo(j) then begin
           y0=latlo(j)
           y1=lathi(j)
        endif
        if lathi(j) lt latlo(j) then begin
           y0=lathi(j)
           y1=latlo(j)
        endif
        if j lt nbin2 then good=where(mlat ge y0 and mlat lt y1 and (mltime le mltime0 or mltime ge mltime1),nn)
        if j ge nbin2 then good=where(mlat ge y0 and mlat lt y1 and mltime ge mltime0 and mltime le mltime1,nn)
        if good(0) ne -1L then begin
;print,'NOGAPS LT at Lat= ',j,y0,min(mltime(index),/nan),max(mltime(index),/nan),' within range? of ',mltime0,mltime1
           tp_profs=reform(mtemp(good,*))
           h2o_profs=reform(mh2o(good,*))
           z_profs=reform(mheight(good,*))
           zindex=where(z_profs ge 82. and z_profs le 87.)
           nogaps_tp(today,j)=mean(tp_profs(zindex))
           nogaps_h2o(today,j)=mean(h2o_profs(zindex))
        endif
    endfor
    jumpday:		; no NOGAPS data
    icount=icount+1L
    jumpcount:		; no CIPS data
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
              /bold,/color,bits_per_pixel=8,/helvetica,filename='save_nogaps4cips_3c_'+syr+'_LT.ps'
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
index=where(nogaps_tp eq -999.)
if index(0) ne -1L then nogaps_tp(index)=0./0.
index=where(nogaps_h2o eq -999.)
if index(0) ne -1L then nogaps_h2o(index)=0./0.

rlat=72
if shem eq 'south' then rlat=-1.*rlat
yindex=where(latlo eq rlat)
xyouts,.35,.925,syr+' at '+strcompress(long(rlat),/remove_all)+' Deg',/normal,charsize=2,color=0
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(0)),xrange=[-51.,71.],psym=8,yrange=[0.,40.],xtitle='DFS',color=0,title='ASC',ytitle='Albedo'
axis,/yax,/save,yrange=[120.,170.],color=90,ytitle='NOGAPS Temperature',yminor=1
oplot,dfs,nogaps_tp(*,yindex(0)),psym=8,color=90,symsize=0.75
oplot,dfs,nogaps_tp(*,yindex(0)),thick=2,color=90
;
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(0)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='NOGAPS Water Vapor',yminor=1
oplot,dfs,nogaps_h2o(*,yindex(0)),psym=8,color=90,symsize=0.75
oplot,dfs,nogaps_h2o(*,yindex(0)),thick=2,color=90

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,title='DES',ytitle='Albedo'        
axis,/yax,/save,yrange=[120.,170.],color=90,ytitle='NOGAPS Temperature',yminor=1
oplot,dfs,nogaps_tp(*,yindex(1)),psym=8,color=90,symsize=0.75
oplot,dfs,nogaps_tp(*,yindex(1)),thick=2,color=90
;
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'      ; 75 degrees latitude
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='NOGAPS Water Vapor',yminor=1
oplot,dfs,nogaps_h2o(*,yindex(1)),psym=8,color=90,symsize=0.75
oplot,dfs,nogaps_h2o(*,yindex(1)),thick=2,color=90

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim save_nogaps4cips_3c_'+syr+'_LT.ps -rotate -90 save_nogaps4cips_3c_'+syr+'_LT.jpg'
;  spawn,'rm -f save_nogaps4cips_3c_'+sdate+'_LT.ps'
endif
;
; SAVE NOGAPS T and H2O for corresponding CIPS summary file
;
index=where(finite(nogaps_tp) eq 0)
if index(0) ne -1L then nogaps_tp(index)=-999.
index=where(finite(nogaps_h2o) eq 0)
if index(0) ne -1L then nogaps_h2o(index)=-999.

comment=strarr(3)
comment(0)='NOGAPS T and H2O averaged between 82 and 87 km'
comment(1)='For CIPS latitude bins 0-34, NOGAPS local times are < 6 or > 18
comment(2)='For CIPS latitude bins 35-69, NOGAPS local times are between 6 and 18

ofile=odir+shem+'_'+syr+'_v04.20_r04_LT.sav'
print,'saved '+ofile
save,file=ofile,nogaps_tp,nogaps_h2o,latlo,lathi,doy,comment
end
