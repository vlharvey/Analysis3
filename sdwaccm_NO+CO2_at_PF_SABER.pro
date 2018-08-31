;
; SABER CO2 and NO near Poker Flat
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
xorig=[0.1,0.55]
yorig=[0.2,0.2]
xlen=0.35
ylen=0.6
cbaryoff=0.01
cbarydel=0.03
!NOERAS=-1
!p.font=1
!x.ticklen=-0.05
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
   !p.background=mcolor
endif
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
;  !p.font=0
   !p.charthick=3
   device,font_size=8
   device,/landscape,bits=8,filename='zt_sdwaccm_nox_nh_spring+co2.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize
endif
erase
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
syear=['2009']	;,'2006','2008','2009','2010','2011','2012']
nyear=n_elements(syear)
;
; loop over years
;
for iyear=0L,nyear-1L do begin
;goto,quick
;
; restore WACCM output
;
lc=icolmax
if syear(iyear) eq '2009' then lc=0
spawn,'ls /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.'+syear(iyear)+'-0*.nc_MLS_etal.sav',ifiles
spawn,'ls /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.'+syear(iyear)+'-0*.nc_MLS_etal_v2.sav',ifiles2
nfile=n_elements(ifiles)
nfile=10
icount=0L
for ifile=0L,nfile-1L do begin
    restore,ifiles(ifile)
    restore,ifiles2(ifile)
    print,ifiles(ifile)
    index=where(latitude gt 0.,nprof)
    if index(0) ne -1L then begin
       if icount ne 0L then begin
          instrument_all=[instrument_all,INSTRUMENT(index)]
          date_all=[date_all,DATE(index)]
          fdoy_all=[fdoy_all,FDOY(index)]
          latitude_all=[latitude_all,LATITUDE(index)]
          longitude_all=[longitude_all,LONGITUDE(index)]
          ltime_all=[ltime_all,LTIME(index)]
          sctype_all=[sctype_all,SCTYPE(index)]
          co2mix=[co2mix,CO2(index,*)]
          nomix=[nomix,NO(index,*)]
          press=[press,P(index,*)]
          temp=[temp,T(index,*)]
          altitude=[altitude,Z(index,*)]
       endif
       if icount eq 0L then begin
          instrument_all=INSTRUMENT(index)
          date_all=DATE(index)
          fdoy_all=FDOY(index)
          latitude_all=LATITUDE(index)
          longitude_all=LONGITUDE(index)
          ltime_all=LTIME(index)
          sctype_all=SCTYPE(index)
          co2mix=CO2(index,*)              
          nomix=NO(index,*)
          press=P(index,*)
          temp=T(index,*)
          altitude=Z(index,*)
          icount=1L
       endif
;help,ifile,z(index,*),altitude
    endif
endfor
;
; extract MIPAS
;
index=where(instrument_all eq 4L)
if index(0) eq -1L then stop,'No SABER data'
date_all=DATE_all(index)
fdoy_all=FDOY_all(index)
latitude_all=LATITUDE_all(index)
longitude_all=LONGITUDE_all(index)
ltime_all=LTIME_all(index)
sctype_all=SCTYPE_all(index)
comix=COmix(index,*)
co2mix=CO2mix(index,*)
nomix=NOmix(index,*)
press=PRESS(index,*)
temp=TEMP(index,*)
altitude=altitude(index,*)
;save,file='saber_no+co2_2009.sav',nomix,co2mix,date_all,fdoy_all,latitude_all,longitude_all,ltime_all,temp,press,altitude
quick:
;restore,'saber_no+co2_2009.sav'
;
; rename
;
fdoy=fdoy_all
latitude=latitude_all
longitude=longitude_all
;
; NO and CO2 near Poker Flat
;
;index=where(fdoy gt 30. and latitude gt 65. and latitude lt 75. and longitude gt 210. and longitude lt 215.,nn)
index=where(fdoy gt 30. and latitude gt 75.,nn)
no_at_pf=1.e6*reform(nomix(index,*))
co2_at_pf=1.e6*reform(co2mix(index,*))
z_at_pf=reform(altitude(index,*))/1000.
fdoy_at_pf=reform(fdoy(index))

    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,no_at_pf(0,*),z_at_pf(0,*),yrange=[20.,130.],/xlog,color=0,xrange=[1.e-12,1.e3],thick=3,ytitle='Altitude (km)',xtitle='NO (ppmv)',charsize=1.5
    for i=0L,nn-1L do oplot,no_at_pf(i,*),z_at_pf(i,*),color=(float(i)/float(nn))*mcolor,thick=2

    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,co2_at_pf(0,*),z_at_pf(0,*),yrange=[20.,130.],color=0,xrange=[0,400.],thick=3,ytitle='Altitude (km)',xtitle='CO!l2!n (ppmv)',charsize=1.5
    for i=0,nn-1L do oplot,co2_at_pf(i,*),z_at_pf(i,*),color=(float(i)/float(nn))*mcolor,thick=2
stop
    jumpyear:
endfor  ; loop over years

if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim zt_sdwaccm_nox_nh_spring+co2.ps -rotate -90 zt_sdwaccm_nox_nh_spring+co2.jpg'
endif
end
