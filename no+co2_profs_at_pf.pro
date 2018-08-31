;
; SABER NO and CO2 profiles at Poker
;
loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
setplot='x'
read,'setplot=',setplot
mcolor=icolmax
icmm1=icolmax-1
icmm2=icolmax-2
nxdim=800 & nydim=800
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
cbaryoff=0.05
cbarydel=0.05
!NOERAS=-1
!p.font=1
!x.ticklen=-0.05
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
   !p.background=mcolor
endif
erase
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
restore,'saber_no+co2_1pf_2009.sav
;
; plot profiles
;
wnox=onox
wco2=oco2
waltitude=altitude
;
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
;plot,reform(wnox(22,*)),waltitude(22,*),color=0,charsize=2,charthick=2,thick=3,ytitle='Altitude (km)',xtitle='NO (ppmv)',yrange=[70,120],xrange=[0.01,500],/xlog,xstyle=8
;oplot,wnox(22,*),waltitude(22,*),color=100,thick=3
;oplot,wnox(26,*),waltitude(26,*),color=250,thick=3 
;axis,/xax,xrange=[100,400],/save,color=0,xtitle='CO!l2!n (ppmv)',charsize=2,charthick=2,xticks=4,xtickv=[100,200,300,400]
;oplot,wco2(22,*),waltitude(22,*),color=100,thick=3
;oplot,wco2(26,*),waltitude(26,*),color=250,thick=3

plot,reform(wco2(22,*)),waltitude(22,*),color=0,charsize=2,charthick=2,thick=3,ytitle='Altitude (km)',xtitle='CO!l2!n (ppmv)',yrange=[70,120],xrange=[100,400],xstyle=8
oplot,wco2(22,*),waltitude(22,*),color=100,thick=3             
oplot,wco2(26,*),waltitude(26,*),color=250,thick=3             
axis,/xax,xrange=[0.01,500],/xlog,/save,color=0,xtitle='NO (ppmv)',charsize=2,charthick=2
oplot,wnox(22,*),waltitude(22,*),color=100,thick=3
oplot,wnox(26,*),waltitude(26,*),color=250,thick=3

xyouts,.3,.7,strcompress(date_array(22),/r),/normal,charsize=3,charthick=2,color=100
xyouts,.3,.65,strcompress(date_array(26),/r),/normal,charsize=3,charthick=2,color=250

end
