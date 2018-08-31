;
; fit a line to each years ASC and DES and plot regressions for all years
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
for iyear=2007,2015 do begin
syear=strcompress(long(iyear),/r)
restore,'cips_l2_medians_vs_sza_north_'+syear+'.sav

if iyear eq 2007 then begin
dfs_tot=DFS_ALL			;	FLOAT     = Array[121]
freq20_tot=FREQ20		;	FLOAT     = Array[121]
freq51_tot=FREQ51		;	FLOAT     = Array[121]
MEDALB20_tot=MEDALB20_ALL	;	FLOAT     = Array[121, 60]
MEDALB51_tot=MEDALB51_ALL	;	FLOAT     = Array[121, 60]
MEDIWC20_tot=MEDIWC20_ALL	;	FLOAT     = Array[121, 60]
MEDIWC51_tot=MEDIWC51_ALL	;	FLOAT     = Array[121, 60]
MEDRAD20_tot=MEDRAD20_ALL	;	FLOAT     = Array[121, 60]
MEDRAD51_tot=MEDRAD51_ALL	;	FLOAT     = Array[121, 60]
num20_tot=num20_all
num51_tot=num51_all
sdate_tot=SDATE_ALL		;	STRING    = Array[121]
;SIGALB20_ALL    FLOAT     = Array[121, 60]
;SIGALB51_ALL    FLOAT     = Array[121, 60]
;SIGIWC20_ALL    FLOAT     = Array[121, 60]
;SIGIWC51_ALL    FLOAT     = Array[121, 60]
;SIGRAD20_ALL    FLOAT     = Array[121, 60]
;SIGRAD51_ALL    FLOAT     = Array[121, 60]
endif
if iyear gt 2007 then begin
dfs_tot=[dfs_tot,DFS_ALL]                 ;       FLOAT     = Array[121]
freq20_tot=[freq20_tot,FREQ20]               ;       FLOAT     = Array[121]
freq51_tot=[freq51_tot,FREQ51]               ;       FLOAT     = Array[121]
MEDALB20_tot=[MEDALB20_tot,MEDALB20_ALL]       ;       FLOAT     = Array[121, 60]
MEDALB51_tot=[MEDALB51_tot,MEDALB51_ALL]       ;       FLOAT     = Array[121, 60]
MEDIWC20_tot=[MEDIWC20_tot,MEDIWC20_ALL]       ;       FLOAT     = Array[121, 60]
MEDIWC51_tot=[MEDIWC51_tot,MEDIWC51_ALL]       ;       FLOAT     = Array[121, 60]
MEDRAD20_tot=[MEDRAD20_tot,MEDRAD20_ALL]       ;       FLOAT     = Array[121, 60]
MEDRAD51_tot=[MEDRAD51_tot,MEDRAD51_ALL]       ;       FLOAT     = Array[121, 60]
num20_tot=[num20_tot,num20_all]
num51_tot=[num51_tot,num51_all]
sdate_tot=[sdate_tot,SDATE_ALL]
endif

endfor
iyears=long(strmid(sdate_tot,0,4))

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
                /bold,/color,bits_per_pixel=8,/helvetica,filename='scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_allyears.ps'
         !p.charsize=1.2
         !p.thick=2
         !p.charthick=5
         !y.thick=2
         !x.thick=2
      endif

nlvls=9
col1=1+indgen(nlvls)*mcolor/nlvls
!type=2^2+2^3
level=2.5+2.5*findgen(20)
xlen=0.25

npts=100.
index=where(num20_tot lt npts)
if index(0) ne -1L then begin
   medalb20_tot(index)=0./0.
   mediwc20_tot(index)=0./0.
   medrad20_tot(index)=0./0.
endif
index=where(num51_tot lt npts)
if index(0) ne -1L then begin
   medalb51_tot(index)=0./0.
   mediwc51_tot(index)=0./0.
   medrad51_tot(index)=0./0.
endif

year1=2007
year2=2015

nday=n_elements(SDATE_TOT)
nbin=n_elements(szabin)
x2d=fltarr(nday,nbin)
xx2d=fltarr(nday,nbin)
y2d=fltarr(nday,nbin)
for j=0L,nbin-1L do x2d(*,j)=dfs_tot
for j=0L,nbin-1L do xx2d(*,j)=iyears
for i=0L,nday-1L do y2d(i,*)=szabin
set_viewport,xorig(0),xorig(0)+xlen,yorig(0),yorig(0)+ylen
med20=medalb20_tot
med51=medalb51_tot
level=findgen(51)
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 ALB',xtitle='4.2 ALB',xrange=[0,50],yrange=[0,50],charsize=1.2,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and y2d le 70. and med51 gt 0. and med20 gt 0.) ; DES
    oplot,med20(index),med51(index),color=250,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    x=level
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=250

    index=where(xx2d eq iyear and y2d gt 70. and med51 gt 0. and med20 gt 0.)   ; ASC
    oplot,med20(index),med51(index),color=90,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=90     ;col1(iyear-2007)
endfor
oplot,level,level,thick=2,color=0
xyouts,35,15,'ASC',/data,color=90,charsize=1.5,charthick=2
xyouts,35,5,'DES',/data,color=250,charsize=1.5,charthick=2


set_viewport,xorig(1),xorig(1)+xlen,yorig(1),yorig(1)+ylen
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 ALB',xtitle='4.2 ALB',xrange=[0,50],yrange=[0,50],charsize=1.2,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and med51 gt 0. and med20 gt 0.)	; DES
    oplot,med20(index),med51(index),color=col1(iyear-year1),psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=3,color=col1(iyear-year1)
    xyouts,xorig(1)+xlen+0.02,yorig(5) +0.02+ (iyear-year1)*(yorig(5)+yorig(1))/(year2-year1+1), strcompress(iyear,/r),/normal,color=col1(iyear-year1),charthick=2
endfor
oplot,level,level,thick=2,color=0

set_viewport,xorig(2),xorig(2)+xlen,yorig(2),yorig(2)+ylen
med20=mediwc20_tot
med51=mediwc51_tot
level=10*findgen(51)
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 IWC',xtitle='4.2 IWC',xrange=[0,400],yrange=[0,400],charsize=1.2,xticks=4,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and y2d le 70. and med51 gt 0. and med20 gt 0.) ; DES
    oplot,med20(index),med51(index),color=250,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    x=level
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=250

    index=where(xx2d eq iyear and y2d gt 70. and med51 gt 0. and med20 gt 0.)   ; ASC
    oplot,med20(index),med51(index),color=90,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=90     ;col1(iyear-2007)
endfor
oplot,level,level,thick=2,color=0


set_viewport,xorig(3),xorig(3)+xlen,yorig(3),yorig(3)+ylen
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 IWC',xtitle='4.2 IWC',xrange=[0,400],yrange=[0,400],charsize=1.2,xticks=4,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and med51 gt 0. and med20 gt 0.)   ; DES   
    oplot,med20(index),med51(index),color=col1(iyear-year1),psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=3,color=col1(iyear-year1)
endfor
oplot,level,level,thick=2,color=0

set_viewport,xorig(4),xorig(4)+xlen,yorig(4),yorig(4)+ylen
med20=medrad20_tot
med51=medrad51_tot
level=2.*findgen(51)
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 RAD',xtitle='4.2 RAD',xrange=[20,100],yrange=[20,100],charsize=1.2,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and y2d le 70. and med51 gt 0. and med20 gt 0.) ; DES
    oplot,med20(index),med51(index),color=250,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    x=level
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=250

    index=where(xx2d eq iyear and y2d gt 70. and med51 gt 0. and med20 gt 0.)   ; ASC
    oplot,med20(index),med51(index),color=90,psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=2,color=90     ;col1(iyear-2007)
endfor
oplot,level,level,thick=2,color=0


set_viewport,xorig(5),xorig(5)+xlen,yorig(5),yorig(5)+ylen
index=where(med51 gt 0. and med20 gt 0.)
plot,med20(index),med51(index),color=0,psym=8,symsize=1,ytitle='5.1 RAD',xtitle='4.2 RAD',xrange=[20,100],yrange=[20,100],charsize=1.2,charthick=2,/noeras,/nodata
for iyear=year1,year2 do begin
    index=where(xx2d eq iyear and med51 gt 0. and med20 gt 0.)
    oplot,med20(index),med51(index),color=col1(iyear-year1),psym=8,symsize=0.5
    result=linfit(med20(index),med51(index),yfit=linevals)
    b=result(0)
    slope=result(1)
    y=slope*x+b
    oplot,x,y,thick=3,color=col1(iyear-year1)
endfor
oplot,level,level,thick=2,color=0

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_allyears.ps -rotate -90 scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_allyears.jpg'
;      spawn,'rm -f scatter-6pan_cips_l4_v4.20_v5.10_rawfiles_north_allyears.ps'
    endif
end
