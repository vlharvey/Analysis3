;
; plot CIPS summary data as surfaces as J. Olivero
; VLH 10/18/2011
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
xorig=[0.15]
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
cdir='/Volumes/Data/CIPS_data/Datfiles/cips_3c_'
mdir='/Volumes/Data/CIPS_data/Datfiles_MLS_DMP/cips_3c_mls_'
;
; Ask interactive questions- get starting/ending date and p surface
;
shem='s'
lstyr=10
ialb=1
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
restore,mdir+shem+'_'+syr+'_v04.20_r05_LT.sav'
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
restore,cdir+shem+'_'+syr+'_v04.20_r05_'+salb+'_all.sav'
print,'restored '+cdir+shem+'_'+syr+'_v04.20_r05_'+salb+'_all.sav'
if shem eq 'south' then latlo=-1.*latlo
if shem eq 'south' then lathi=-1.*lathi
;
; convert date to DOY
;
sdate=strcompress(date,/remove_all)
nn=n_elements(sdate)
doy=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(sdate(i),0,4))
    imn=long(strmid(sdate(i),4,2))
    idy=long(strmid(sdate(i),6,2))
    z = kgmt(imn,idy,iyr,iday)
    doy(i)=1.0*iday
endfor
year=long(strmid(sdate,0,4))
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
; indices 0-34: MLS local time should be < 6 or > 18
; indices 35-69: MLS local time should be between 6 and 18
; 
; postscript file
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,filename='plot_cips_surface_'+syr+'.ps'
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
if index(0) ne -1L then mls_tp(index)=0./0.
mls_tp=smooth(mls_tp,5,/nan)

result=size(rad)
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nbin2=nbin/2
latlo(nbin2:nbin-1)=latlo(nbin2)+findgen(nbin2)
index=where(lathi mod 5 eq 0,nyticks)
;surface,smooth(alb,5),dfs,latlo,xtitle='DFS 20'+syr,ytitle='PM     Latitude     AM',color=0,charsize=3,ytickv=latlo(index),$
;       yticks=nyticks-1,ytickname=strcompress(lathi(index),/remove_all),ztitle='Albedo',ztickname=['10','20','30','40','50','60'],$
;       zticks=5,ztickv=[10,20,30,40,50,60],/save
imin=0.		; IWC
imax=325.	; IWC
imin=120.	; MLS TP
imax=190.
shade_surf,smooth(alb,5),dfs,latlo,xtitle='DFS 20'+syr,ytitle='PM     Latitude     AM',color=0,ztitle='Albedo',charsize=3,ytickv=latlo(index),$
        yticks=nyticks-1,ytickname=strcompress(lathi(index),/remove_all),ztickname=['10','20','30','40','50','60'],$
        zticks=5,ztickv=[10,20,30,40,50,60],/save,shades=bytscl(mls_tp,imin,imax),pixels=result(4)	;,az=-60

!type=2^2+2^3+2^5+2^6
CONTOUR,smooth(alb,9),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[1.],color=0,thick=2
CONTOUR,smooth(alb,9),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[2.],color=50,thick=2
CONTOUR,smooth(alb,9),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[5.],color=100,thick=2
CONTOUR,smooth(alb,9),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[10.],color=140,thick=2
CONTOUR,smooth(alb,7),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[20.],color=220,thick=2
CONTOUR,smooth(alb,7),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[30.],color=230,thick=2
CONTOUR,smooth(alb,7),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[40.],color=240,thick=2
CONTOUR,smooth(alb,7),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[50.],color=250,thick=2
!type=2^2+2^3
CONTOUR,smooth(alb,7),dfs,latlo,/T3D,/NOERASE,MAX_VAL = max(alb)+10., ZVALUE = 1.0, /NOCLIP,LEVELS=[1.],color=0,ytickv=latlo(index),$
        yticks=nyticks-1,ytickname=strcompress(lathi(index),/remove_all),charsize=3

   set_viewport,xmx+0.02,xmx+0.06,(ymx+ymn)/1.5,ymx+0.02
   !type=2^2+2^3+2^5+2^6
   plot,[0,10],[1,50],xrange=[0,10],yrange=[1,50],/noeras,color=0,charsize=1,ytitle='Garys'
   ylab=['1','2','5','10','20','30','40','50']
   xbox=[0,10,10,0,0]
   y2=1.
   col1=[0,50,100,140,220,230,240,250]
   nlvls=n_elements(col1)
   dy=(50.-1.)/(float(nlvls)-1)
   for j=0,nlvls-1 do begin
       ybox=[y2,y2,y2+dy,y2+dy,y2]
       polyfill,xbox,ybox,color=col1(j)
       xyouts,0,y2+dy/4,ylab(j),/data,color=0,charsize=1.5
       if j lt 2 then xyouts,0,y2+dy/4,ylab(j),/data,color=mcolor,charsize=1.5
       y2=y2+dy
   endfor
   xyouts,22.,20.,'Garys',/data,color=0,orientation=90,charsize=2

   set_viewport,xmx+0.02,xmx+0.06,ymn,(xmn+ymx)/2.
   !type=2^2+2^3+2^5
   plot,[0,10],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='MLS Temperature'
   xbox=[0,10,10,0,0]
   y2=imin
   col1=(findgen(21)/21.)*mcolor
   nlvls=n_elements(col1)
   dy=(imax-imin)/(float(nlvls)-1)
   for j=0,nlvls-1 do begin
       ybox=[y2,y2,y2+dy,y2+dy,y2]
       polyfill,xbox,ybox,color=col1(j)
       y2=y2+dy
   endfor
;  xyouts,22.,200.,'IWC',/data,color=0,orientation=90,charsize=2

;
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim plot_cips_surface_'+syr+'.ps -rotate -90 plot_cips_surface_'+syr+'.jpg'
;  spawn,'rm -f plot_cips_surface_'+syr+'.ps'
endif
end
