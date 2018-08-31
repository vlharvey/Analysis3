;
; compare CIPS level 2 v4.2 and v5.10
; VLH 9/13/2016
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
usersym,0.5*cos(a),0.5*sin(a),/fill
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.15,0.15,0.15]
yorig=[0.7,0.4,0.1]
xlen=0.7
ylen=0.225
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
smonth=['J','F','M','A','M','J','J','A','S','O','N','D']
pth='/atmos/harvey/CIPS_data/Datfiles/Level_2/cips_sci_2_orbit_'
;
; loop over years
;
for iyear=2007,2016 do begin
syear=strcompress(long(iyear),/r)
restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav

xorig=[.15,.55,.15,.55,.15,.55]
yorig=[.7,.7,.4,.4,.1,.1]
erase
      if setplot eq 'ps' then begin
         lc=0
         set_plot,'ps'
         xsize=nxdim/100.
         ysize=nydim/100.
         !p.font=0
         device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                /bold,/color,bits_per_pixel=8,/helvetica,filename='scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps'
         !p.charsize=1.25
         !p.thick=2
         !p.charthick=5
         !p.charthick=5
         !y.thick=2
         !x.thick=2
      endif

nlvls=20
col1=1+indgen(nlvls)*mcolor/nlvls
!type=2^2+2^3
level=2.5+2.5*findgen(20)
xlen=0.3

xyouts,.45,.95,strmid(sdate_all(0),0,4),charsize=2,charthick=2,color=0,/normal
nday=long(kday)
nbin=n_elements(szabin)
x2d=fltarr(nday,nbin)
y2d=fltarr(nday,nbin)
for j=0L,nbin-1L do x2d(*,j)=dfs_all
for i=0L,nday-1L do y2d(i,*)=szabin
set_viewport,xorig(0),xorig(0)+xlen,yorig(0),yorig(0)+ylen
index=where(medalb51_all gt 0. and medalb20_all gt 0.)
plot,medalb20_all(index),medalb51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 ALB',xtitle='4.2 ALB',xrange=[0,50],yrange=[0,50],charsize=1.5,charthick=2,/noeras
index=where(x2d le 0 and medalb51_all gt 0. and medalb20_all gt 0.)	; onset
oplot,medalb20_all(index),medalb51_all(index),color=250,psym=8,symsize=1
index=where(x2d ge 40. and medalb51_all gt 0. and medalb20_all gt 0.)   ; decline
oplot,medalb20_all(index),medalb51_all(index),color=90,psym=8,symsize=1
oplot,findgen(51),findgen(51),thick=2,color=0
xyouts,30,15,'PRE',/data,color=250,charsize=1.5,charthick=2
xyouts,30,5,'POST',/data,color=90,charsize=1.5,charthick=2

set_viewport,xorig(1),xorig(1)+xlen,yorig(1),yorig(1)+ylen
index=where(medalb51_all gt 0. and medalb20_all gt 0.)
plot,medalb20_all(index),medalb51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 ALB',xtitle='4.2 ALB',xrange=[0,50],yrange=[0,50],charsize=1.5,charthick=2,/noeras,/nodata
index=where(y2d le 70. and medalb51_all gt 0. and medalb20_all gt 0.)	; DES
oplot,medalb20_all(index),medalb51_all(index),color=250,psym=8,symsize=1
result=linfit(medalb20_all(index),medalb51_all(index),yfit=linevals)
x=findgen(51)
b=result(0)
slope=result(1)
y=slope*x+b
;oplot,x,y,thick=2,color=250

index=where(y2d gt 70. and medalb51_all gt 0. and medalb20_all gt 0.)   ; ASC
oplot,medalb20_all(index),medalb51_all(index),color=90,psym=8,symsize=1
result=linfit(medalb20_all(index),medalb51_all(index),yfit=linevals)
x=findgen(51)
b=result(0)
slope=result(1)
y=slope*x+b
;oplot,x,y,thick=2,color=90

oplot,findgen(51),findgen(51),thick=2,color=0
xyouts,35,15,'ASC',/data,color=90,charsize=1.5,charthick=2
xyouts,35,5,'DES',/data,color=250,charsize=1.5,charthick=2

level=10+10*findgen(50)
set_viewport,xorig(2),xorig(2)+xlen,yorig(2),yorig(2)+ylen
index=where(mediwc51_all gt 0. and mediwc20_all gt 0.)
plot,mediwc20_all(index),mediwc51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 IWC',xtitle='4.2 IWC',xrange=[0,350],yrange=[0,350],charsize=1,charthick=2,/noeras
index=where(x2d le 0 and mediwc51_all gt 0. and mediwc20_all gt 0.)     ; onset
oplot,mediwc20_all(index),mediwc51_all(index),color=250,psym=8,symsize=1
index=where(x2d ge 40. and mediwc51_all gt 0. and mediwc20_all gt 0.)   ; decline
oplot,mediwc20_all(index),mediwc51_all(index),color=90,psym=8,symsize=1
oplot,level,level,thick=2,color=0

set_viewport,xorig(3),xorig(3)+xlen,yorig(3),yorig(3)+ylen
index=where(mediwc51_all gt 0. and mediwc20_all gt 0.)
plot,mediwc20_all(index),mediwc51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 IWC',xtitle='4.2 IWC',xrange=[0,350],yrange=[0,350],charsize=1,charthick=2,/noeras
index=where(y2d le 70 and mediwc51_all gt 0. and mediwc20_all gt 0.)
oplot,mediwc20_all(index),mediwc51_all(index),color=250,psym=8,symsize=1
index=where(y2d gt 70. and mediwc51_all gt 0. and mediwc20_all gt 0.)
oplot,mediwc20_all(index),mediwc51_all(index),color=90,psym=8,symsize=1
oplot,level,level,thick=2,color=0

level=10+2.5*findgen(39)
set_viewport,xorig(4),xorig(4)+xlen,yorig(4),yorig(4)+ylen
index=where(medrad51_all gt 0. and medrad20_all gt 0.)
plot,medrad20_all(index),medrad51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 RAD',xtitle='4.2 RAD',xrange=[10,90],yrange=[10,90],charsize=1.5,charthick=2,/noeras
index=where(x2d le 0 and medrad51_all gt 0. and medrad20_all gt 0.)     ; onset
oplot,medrad20_all(index),medrad51_all(index),color=250,psym=8,symsize=1
index=where(x2d ge 40. and medrad51_all gt 0. and medrad20_all gt 0.)   ; decline
oplot,medrad20_all(index),medrad51_all(index),color=90,psym=8,symsize=1
oplot,level,level,thick=2,color=0

set_viewport,xorig(5),xorig(5)+xlen,yorig(5),yorig(5)+ylen
index=where(medrad51_all gt 0. and medrad20_all gt 0.)
plot,medrad20_all(index),medrad51_all(index),color=0,psym=8,symsize=1,ytitle='5.1 RAD',xtitle='4.2 RAD',xrange=[10,90],yrange=[10,90],charsize=1.5,charthick=2,/noeras
index=where(y2d le 70 and medrad51_all gt 0. and medrad20_all gt 0.)
oplot,medrad20_all(index),medrad51_all(index),color=250,psym=8,symsize=1
index=where(y2d gt 70. and medrad51_all gt 0. and medrad20_all gt 0.)
oplot,medrad20_all(index),medrad51_all(index),color=90,psym=8,symsize=1
oplot,level,level,thick=2,color=0

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps -rotate -90 scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.jpg'
;      spawn,'rm -f scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_'+syear+'.ps'
    endif
endfor
end
