;
; save daily zonal means in yearly output files
; SmidEmax
;
loadct,39
device,decompose=0
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
;
; build MMDD dates
;
year1files=file_search(dir+'001????.nc3')
ndays=n_elements(year1files)
mmdd=strarr(ndays)
for ii=0L,ndays-1L do begin
    dum=strsplit(year1files(ii),'.',/extract)
    dum2=strsplit(dum(0),'_',/extract)
    mmdd(ii)=strmid(dum2(-1),3,4)
endfor
;
; loop over years
;
for iyear=1L,300L do begin
    syear=string(format='(i3.3)',iyear)
    ofile=dir+syear+'_ZMDAILY.sav'
    dum=file_search(ofile)
    if dum(0) ne '' then goto,skipyear
;
; loop over 365 days of the year
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
;
; read daily WACCM pressure data
; COGRD           FLOAT     = Array[144, 96, 66]
; LAT             FLOAT     = Array[96]
; LEV             FLOAT     = Array[66]
; LON             FLOAT     = Array[144]
; TGRD            FLOAT     = Array[144, 96, 66]
; UGRD            FLOAT     = Array[144, 96, 66]
; VGRD            FLOAT     = Array[144, 96, 66]
; ZGRD            FLOAT     = Array[144, 96, 66]
;
    filename=findfile(dir+syear+mmdd(iday)+'_vE.sav')
    if filename(0) eq '' then goto,jumpday
    restore,filename
;
    if iday eq 0L then begin
       nr=n_elements(lat)
       nlv=n_elements(lev)
       ubar=0./0.*fltarr(nr,nlv,ndays)
       vbar=0./0.*fltarr(nr,nlv,ndays)
       tbar=0./0.*fltarr(nr,nlv,ndays)
       zbar=0./0.*fltarr(nr,nlv,ndays)
       cobar=0./0.*fltarr(nr,nlv,ndays)
    endif
;
; retain all daily zonal mean fields in one yearly file
;
    ubar(*,*,iday)=mean(ugrd,dim=1,/Nan)
    vbar(*,*,iday)=mean(vgrd,dim=1,/Nan)
    tbar(*,*,iday)=mean(tgrd,dim=1,/Nan)
    zbar(*,*,iday)=mean(zgrd,dim=1,/Nan)
    cobar(*,*,iday)=mean(cogrd,dim=1,/Nan)

;!type=2^2+2^3
;sdate=mmdd(iday)
;contour,ubar(*,*,iday),lat,reform(zbar(*,*,iday))/1000.,xrange=[-90.,90.],yrange=[10.,130.],title='Ubar '+sdate,$
;        xtitle='Latitude',ytitle='Altitude (km)',charsize=2,charthick=2,xticks=6,/nodata
;contour,ubar(*,*,iday),lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=10+10*findgen(20)
;contour,ubar(*,*,iday),lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=-200+10*findgen(20),c_linestyle=5
;contour,ubar(*,*,iday),lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=[0],thick=3
;cobar=cobar*1.e6
;cobar0=reform(cobar(*,*,iday))
;dcobar0=0.*cobar0
;for k=0L,nlv-1L do dcobar0(*,k)=deriv(cobar0(*,k))/mean(cobar0(*,k),/Nan)     ;  normalized
;contour,cobar(*,*,iday),lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=[0.1,0.5,1+findgen(18)],c_color=1+(findgen(20)/20.)*255.,thick=5
;contour,dcobar0,lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=[-1.,-.9,-.8,-.7,-.6,-.5,-.4,-.3,-.2],c_color=[50,60,70,80,90,100,110,120,130],thick=5
;contour,dcobar0,lat,reform(zbar(*,*,iday))/1000.,/overplot,levels=0.2+0.1*findgen(9),c_color=200+5*findgen(9),thick=5
;stop

jumpday:
endfor	; loop over days
;
; save daily mean of all years
;
    print,'saving '+ofile
    save,filename=ofile,nr,nlv,ndays,lat,lev,mmdd,ubar,vbar,tbar,zbar,cobar
skipyear:
endfor	; loop over years
end
