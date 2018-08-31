;
; plot CIPS summary data as surfaces as J. Olivero
; color albedo surface by MLS Tp
; VLH 10/18/2011
;
@stddat
@kgmt
@ckday
@kdate
@mkltime
@smoothit
@fillit

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
xorig=[0.3]
yorig=[0.3]
xlen=0.65
ylen=0.65
cbaryoff=0.175
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
cdir='/atmos/harvey/CIPS_data/Datfiles/Level_3c_Summary/cips_3c_'
mdir='/Volumes/Data/CIPS_data/Datfiles_MLS_DMP/cips_3c_mls_'
;
; Ask interactive questions- get starting/ending date and p surface
;
shem='n'
lstyr=13
ialb=2
;read,' Enter hemisphere (n,s) ',shem
;read,' Enter PMC onset year (yy) ',lstyr
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
restore,mdir+shem+'_'+syr+'_v05.10_r01_LT.sav'
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
restore,cdir+shem+'_'+syr+'_v05.10_r01_'+salb+'_cld.sav'
print,'restored '+cdir+shem+'_'+syr+'_v05.10_r01_'+salb+'_cld.sav'
;
; loop over DOYs in the cips summary file
;
n=findgen(nrev)
n1=1+findgen(nrev)
doy=julday(strmid(strcompress(date,/r),4,2),strmid(strcompress(date,/r),6,2),strmid(strcompress(date,/r),0,4))-julday(1,1,strmid(strcompress(date,/r),0,4))+1L
index=where(doy(n)-doy(n1) ne 0)
days=[doy(index),doy(nrev-1)]
sdate=strcompress(date,/remove_all)
year=long(strmid(sdate,0,4))
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
; postscript file
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,filename='plot_cips_surface_'+syr+'_MLS_TP.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
index=where(alb eq -999.)
if index(0) ne -1L then alb(index)=0
index=where(rad eq -999.)
if index(0) ne -1L then rad(index)=0
index=where(iwc eq -999.)
if index(0) ne -1L then iwc(index)=0
index=where(mls_tp eq -999.)
if index(0) ne -1L then mls_tp(index)=0.
mls_tpsm=0.*mls_tp
fillit,mls_tp,mls_tpsm
mls_tp=mls_tpsm

result=size(rad)
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nbin2=nbin/2
latlosave=latlo
latlo(nbin2:nbin-1)=latlo(nbin2)+findgen(nbin2)
index=where(latlo mod 5 eq 0,nyticks)
tmin=120.	; MLS TP
tmax=180.
ival=1.
if shem eq 'south' then ival=-1.
shade_surf,smooth(alb,3),dfs,latlo,xtitle='DFS 20'+syr,ytitle='ASC     Latitude     DES',color=0,ztitle='CIPS Albedo (G)',charsize=3,ytickv=latlo(index),$
        yticks=nyticks-1,ytickname=strcompress(long(ival*latlosave(index)),/remove_all),ztickname=['10','20','30','40'],$
        zticks=3,ztickv=[10,20,30,40],zrange=[0,40],/save,shades=bytscl(smooth(mls_tp,3),tmin,tmax),pixels=result(4),xrange=[-51.,71.]
set_viewport,xmn,xmx-0.1,ymn-cbaryoff,ymn-cbaryoff+cbarydel
!type=2^2+2^3+2^6
plot,[tmin,tmax],[0,10],yrange=[0,10],xrange=[tmin,tmax],/noeras,color=0,charsize=1.5,xtitle='MLS Temperature (K)'
ybox=[0,10,10,0,0]
x2=tmin
col1=(findgen(21)/21.)*mcolor
nlvls=n_elements(col1)
dx=(tmax-tmin)/float(nlvls)
for j=0,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
;
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert plot_cips_surface_'+syr+'_MLS_TP.ps -rotate -90 plot_cips_surface_'+syr+'_MLS_TP.jpg'
;  spawn,'rm -f plot_cips_surface_'+syr+'_MLS_TP.ps'
endif
end
