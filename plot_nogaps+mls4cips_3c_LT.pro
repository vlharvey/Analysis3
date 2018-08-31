;
; restore save files and plot MLS and NOGAPS T and H2O with CIPS albedo
; instead of separating ASC/DES by SZA, use LT
; VLH 11/05/2011
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
ndir='/Volumes/earth/harvey/NOGAPS_Alpha/Datfiles/'
cdir='/Volumes/Data/CIPS_data/Datfiles/cips_3c_'
odir='/Volumes/Data/CIPS_data/Datfiles_NOGAPS_DMP/cips_3c_nogaps_'
mdir='/Volumes/Data/CIPS_data/Datfiles_MLS_DMP/cips_3c_mls_'
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
doy_cips=doy
;
; restore NOGAPS
;
ofile=odir+shem+'_'+syr+'_v04.20_r04_LT.sav'
restore,ofile	;,nogaps_tp,nogaps_h2o,latlo,lathi,doy,comment
doy_nogaps=doy
;
; restore MLS
;
ofile=mdir+shem+'_'+syr+'_v04.20_r04_LT.sav'
print,'saved '+ofile
restore,ofile	;,mls_tp,mls_h2o,latlo,lathi,doy,comment
doy_mls=doy
bad=where(mls_tp lt 0.)
if bad(0) ne -1L then mls_tp(bad)=0./0.
bad=where(mls_h2o lt 0.)
if bad(0) ne -1L then mls_h2o(bad)=0./0.
;
; postscript file
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='nogaps+mls4cips_3c_'+syr+'_LT.ps'
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
oplot,dfs,nogaps_tp(*,yindex(0)),psym=8,color=90
;oplot,dfs,nogaps_tp(*,yindex(0)),thick=2,color=90
oplot,dfs,mls_tp(*,yindex(0)),psym=8,color=250,symsize=0.75
;oplot,dfs,mls_tp(*,yindex(0)),thick=2,color=250
;
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(0)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='NOGAPS Water Vapor',yminor=1
oplot,dfs,nogaps_h2o(*,yindex(0)),psym=8,color=90
;oplot,dfs,nogaps_h2o(*,yindex(0)),thick=2,color=90
oplot,dfs,mls_h2o(*,yindex(0)),psym=8,color=250,symsize=0.75
;oplot,dfs,mls_h2o(*,yindex(0)),thick=2,color=250

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,title='DES',ytitle='Albedo'        
axis,/yax,/save,yrange=[120.,170.],color=90,ytitle='NOGAPS Temperature',yminor=1
oplot,dfs,nogaps_tp(*,yindex(1)),psym=8,color=90
;oplot,dfs,nogaps_tp(*,yindex(1)),thick=2,color=90
oplot,dfs,mls_tp(*,yindex(1)),psym=8,color=250,symsize=0.75
;oplot,dfs,mls_tp(*,yindex(1)),thick=2,color=250
;
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
plot,dfs,alb(*,yindex(1)),psym=8,xrange=[-51.,71.],yrange=[0.,40.],xtitle='DFS',color=0,ytitle='Albedo'      ; 75 degrees latitude
axis,/yax,/save,yrange=[1.,9.],color=90,ytitle='NOGAPS Water Vapor',yminor=1
oplot,dfs,nogaps_h2o(*,yindex(1)),psym=8,color=90
;oplot,dfs,nogaps_h2o(*,yindex(1)),thick=2,color=90
oplot,dfs,mls_h2o(*,yindex(1)),psym=8,color=250,symsize=0.75
;oplot,dfs,mls_h2o(*,yindex(1)),thick=2,color=250
xyouts,min(xorig)+xlen+0.03,min(yorig)+ylen+0.02,'MLS',charsize=2,charthick=2,color=250,/normal

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim nogaps+mls4cips_3c_'+syr+'_LT.ps -rotate -90 nogaps+mls4cips_3c_'+syr+'_LT.jpg'
;  spawn,'rm -f nogaps+mls4cips_3c_'+sdate+'_LT.ps'
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
