;
; strip out a SZA bin and plot line
; compare CIPS level 2 v4.2 and v5.10
; VLH 1/10/2017

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
xorig=[0.15,0.15,0.15,0.65,0.65,0.65]
yorig=[0.7,0.4,0.1,0.7,0.4,0.1]
xlen=0.25
ylen=0.225
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
erase
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='line_cips_l2_v4.20_v5.10_medians_allyears.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
pth='/atmos/harvey/CIPS_data/Datfiles/Level_2/cips_sci_2_orbit_'
;
; ALB
;
restore,'cips_l2_medians_vs_sza_north_2007.sav
print,szabin
rsza=70.
read,'Enter desired SZA bin ',rsza
index=where(szabin eq rsza)
isza=index(0)
ssza=strcompress(long(rsza),/r)
npts=100.
index=where(num20_all lt npts)
if index(0) ne -1L then medalb20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then medalb51_all(index)=0./0.
nlvls=9
col1=indgen(nlvls)*mcolor/(nlvls-1.)
col1(-1)=col1(-1)-1
!type=2^2+2^3
level=2.5+2.5*findgen(20)
xyouts,.45,.95,'SZA= '+ssza,/normal,color=0,charsize=2,charthick=2
set_viewport,xorig(0),xorig(0)+xlen,yorig(0),yorig(0)+ylen
plot,dfs_all,reform(medalb20_all(*,isza))-reform(medalb51_all(*,isza)),xrange=[-20,40],yrange=[-10,10],ytitle='ALB v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8,title='North'
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
xyouts,xorig(0)+xlen+0.02,yorig(0)+ylen-0.02,'2007',/normal,color=0,charthick=2
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then medalb20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then medalb51_all(index)=0./0.
    oplot,dfs_all,reform(medalb20_all(*,isza))-reform(medalb51_all(*,isza)),psym=8,color=col1(iyear-2007)
    xyouts,xorig(0)+xlen+0.02,yorig(0)+ylen-0.02-0.03*(iyear-2007),syear,/normal,color=col1(iyear-2007),charthick=2
endfor
;
; IWC
;
restore,'cips_l2_medians_vs_sza_north_2007.sav
index=where(num20_all lt npts)
if index(0) ne -1L then mediwc20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then mediwc51_all(index)=0./0.
level=10+10*findgen(20)
set_viewport,xorig(1),xorig(1)+xlen,yorig(1),yorig(1)+ylen
plot,dfs_all,reform(mediwc20_all(*,isza))-reform(mediwc51_all(*,isza)),xrange=[-20,40],yrange=[-100,100],ytitle='IWC v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then mediwc20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then mediwc51_all(index)=0./0.
    oplot,dfs_all,reform(mediwc20_all(*,isza))-reform(mediwc51_all(*,isza)),psym=8,color=col1(iyear-2007)
endfor
;
; Rad
;
restore,'cips_l2_medians_vs_sza_north_2007.sav
index=where(num20_all lt npts)
if index(0) ne -1L then medrad20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then medrad51_all(index)=0./0.
level=20+2.5*findgen(20)
set_viewport,xorig(2),xorig(2)+xlen,yorig(2),yorig(2)+ylen
plot,dfs_all,reform(medrad20_all(*,isza))-reform(medrad51_all(*,isza)),xrange=[-20,40],yrange=[-40,40],ytitle='RAD v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8,xtitle='DFS'
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then medrad20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then medrad51_all(index)=0./0.
    oplot,dfs_all,reform(medrad20_all(*,isza))-reform(medrad51_all(*,isza)),psym=8,color=col1(iyear-2007)
endfor
;
; SH
; ALB
;
restore,'cips_l2_medians_vs_sza_south_2007.sav
print,szabin
rsza=60.
print,'Enter desired SZA bin ',rsza
index=where(szabin eq rsza)
isza=index(0)
ssza=strcompress(long(rsza),/r)
npts=100.
index=where(num20_all lt npts)
if index(0) ne -1L then medalb20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then medalb51_all(index)=0./0.
!type=2^2+2^3
level=2.5+2.5*findgen(20)
set_viewport,xorig(3),xorig(3)+xlen,yorig(3),yorig(3)+ylen
plot,dfs_all,reform(medalb20_all(*,isza))-reform(medalb51_all(*,isza)),xrange=[-20,40],yrange=[-10,10],ytitle='ALB v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8,title='South'
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
;xyouts,xorig(3)+xlen+0.02,yorig(3)+ylen-0.02,'2007',/normal,color=0,charthick=2
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    if iyear ne 2014 then begin
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_south_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then medalb20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then medalb51_all(index)=0./0.
    oplot,dfs_all,reform(medalb20_all(*,isza))-reform(medalb51_all(*,isza)),psym=8,color=col1(iyear-2007)
    endif
endfor
;
; IWC
;
restore,'cips_l2_medians_vs_sza_south_2007.sav
index=where(num20_all lt npts)
if index(0) ne -1L then mediwc20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then mediwc51_all(index)=0./0.
level=10+10*findgen(20)
set_viewport,xorig(4),xorig(4)+xlen,yorig(4),yorig(4)+ylen
plot,dfs_all,reform(mediwc20_all(*,isza))-reform(mediwc51_all(*,isza)),xrange=[-20,40],yrange=[-100,100],ytitle='IWC v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    if iyear ne 2014 then begin
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_south_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then mediwc20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then mediwc51_all(index)=0./0.
    oplot,dfs_all,reform(mediwc20_all(*,isza))-reform(mediwc51_all(*,isza)),psym=8,color=col1(iyear-2007)
    endif
endfor
;
; Rad
;
restore,'cips_l2_medians_vs_sza_south_2007.sav
index=where(num20_all lt npts)
if index(0) ne -1L then medrad20_all(index)=0./0.
index=where(num51_all lt npts)
if index(0) ne -1L then medrad51_all(index)=0./0.
level=20+2.5*findgen(20)
set_viewport,xorig(5),xorig(5)+xlen,yorig(5),yorig(5)+ylen
plot,dfs_all,reform(medrad20_all(*,isza))-reform(medrad51_all(*,isza)),xrange=[-20,40],yrange=[-40,40],ytitle='RAD v4 - v5',charsize=1.25,charthick=2,/noeras,color=col1(0),psym=8,xtitle='DFS'
oplot,dfs_all,0*dfs_all,linestyle=5,color=col1(0)
for iyear=2008,2015 do begin            ; 2015/2016 is the last SH v4.2 season
    if iyear ne 2014 then begin
    syear=strcompress(long(iyear),/r)
    restore,'cips_l2_medians_vs_sza_south_'+syear+'.sav
    npts=100.
    index=where(num20_all lt npts)
    if index(0) ne -1L then medrad20_all(index)=0./0.
    index=where(num51_all lt npts)
    if index(0) ne -1L then medrad51_all(index)=0./0.
    oplot,dfs_all,reform(medrad20_all(*,isza))-reform(medrad51_all(*,isza)),psym=8,color=col1(iyear-2007)
    endif
endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim line_cips_l2_v4.20_v5.10_medians_allyears.ps -rotate -90 line_cips_l2_v4.20_v5.10_medians_allyears.png'
    endif
end
