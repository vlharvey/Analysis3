;
; read WACCM4 CO2x1SmidEmax_yBWCN set1 (and then set2) data and save yearly files
; NOTE: save Year150 from set1 and save Year151 from set2
;
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set1
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set2
;
loadct,39
device,decompose=0
mcolor=255
idir='/atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set1/'
restore,idir+'Vstar_CO2x1SmidEmax_yBWCN_vE.sav'
restore,idir+'Wstar_CO2x1SmidEmax_yBWCN_vE.sav'
nday=n_elements(dates)
nr=n_elements(lat)
nl=n_elements(ilev)
nprof=nl*nr
for ii=0L,nday-1L do begin
    WSTAR0=reform(WSTAR(*,*,ii))
    VSTAR0=reform(VSTAR(*,*,ii))
;
; check 
;
    erase
    contour,vstar0,LAT,ILEV,/ylog,/noeras,yrange=[1.,1.e-6],levels=1+findgen(20),thick=2,title=strcompress(dates(ii)),xtitle='Latitude',ytitle='Pressure (hPa)',charsize=2,charthick=2
    contour,vstar0,LAT,ILEV,/ylog,/overplot,levels=-20+findgen(20),thick=2,charsize=2,charthick=2,c_linestyle=5
    contour,wstar0,LAT,ILEV,/ylog,/overplot,levels=0.01+0.01*findgen(20),thick=2,charsize=2,charthick=2,c_linestyle=0,color=50
    contour,wstar0,LAT,ILEV,/ylog,/overplot,levels=-.2+0.01*findgen(20),thick=2,charsize=2,charthick=2,c_linestyle=5,color=250
;   velovect,vstar0,wstar0,lat,ilev,length=.2,/overplot

wait,1
endfor		; loop over days
end
