;
; zonal mean version for ALL variables
; SmidEmax 300 years
; i.e., Jan 1 is the average of ALL January 1sts.
; save zonal means and sigmas
;
; EKGWSPEC, OMEGA, QJOULE, QRS_AUR, QRS_EUV, TTGW, UTGWSPEC, Vstar, Wstar, Z3
;
loadct,39
mcolor=byte(!p.color)
icmm1=mcolor-1B
icmm2=mcolor-2B
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.1,0.4,0.7,0.1,0.4,0.7,0.1,0.4,0.7]
yorig=[0.7,0.7,0.7,0.4,0.4,0.4,0.1,0.1,0.1]
xlen=0.225
ylen=0.225
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir3d='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
dirzm='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)
svar=['EKGWSPEC', 'OMEGA', 'QJOULE', 'QRS_AUR', 'QRS_EUV', 'TTGW', 'UTGWSPEC', 'Vstar', 'Wstar', 'Z3','T']
nvar=n_elements(svar)
;
; build MMDD dates
;
year1files=file_search(dir3d+'001????.nc3')
ndays=n_elements(year1files)
mmdd=strarr(ndays)
for ii=0L,ndays-1L do begin
    dum=strsplit(year1files(ii),'.',/extract)
    dum2=strsplit(dum(0),'_',/extract)
    mmdd(ii)=strmid(dum2(-1),3,4)
endfor
;
; read all variables for all years and retain for sigma calculation
;
nyear=300L
;nyear=10		; testing
for ivar=0L,nvar-1L do begin
print,svar(ivar)
for iyear=1L,nyear do begin
    syear=string(format='(i3.3)',iyear)
    print,syear
    restore,dirzm+svar(ivar)+'_CO2x1SmidEmax_yBWCN_vE_Year'+syear+'.sav'
;
; get dimensions for this variable on the first year
;
    if iyear eq 1L then begin
       nr=n_elements(lat)
       nl=n_elements(lev)
       if nl eq 0L then nl=n_elements(ilev)
    endif
    if iyear eq 1L and svar(ivar) eq 'EKGWSPEC' then EKGWSPEC_YEARLY_ALL=fltarr(nr,n_elements(ilev),ndays,nyear)
    if svar(ivar) eq 'EKGWSPEC' then EKGWSPEC_YEARLY_ALL(*,*,*,iyear-1)=EKGWSPEC_YEARLY

    if iyear eq 1L and svar(ivar) eq 'OMEGA' then OMEGA_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'OMEGA' then OMEGA_YEARLY_ALL(*,*,*,iyear-1)=OMEGA_YEARLY

    if iyear eq 1L and svar(ivar) eq 'QJOULE' then QJOULE_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'QJOULE' then QJOULE_YEARLY_ALL(*,*,*,iyear-1)=QJOULE_YEARLY

    if iyear eq 1L and svar(ivar) eq 'QRS_AUR' then QRS_AUR_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'QRS_AUR' then QRS_AUR_YEARLY_ALL(*,*,*,iyear-1)=QRS_AUR_YEARLY

    if iyear eq 1L and svar(ivar) eq 'QRS_EUV' then QRS_EUV_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'QRS_EUV' then QRS_EUV_YEARLY_ALL(*,*,*,iyear-1)=QRS_EUV_YEARLY

    if iyear eq 1L and svar(ivar) eq 'TTGW' then TTGW_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'TTGW' then TTGW_YEARLY_ALL(*,*,*,iyear-1)=TTGW_YEARLY

    if iyear eq 1L and svar(ivar) eq 'UTGWSPEC' then UTGWSPEC_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'UTGWSPEC' then UTGWSPEC_YEARLY_ALL(*,*,*,iyear-1)=UTGWSPEC_YEARLY

    if iyear eq 1L and svar(ivar) eq 'Vstar' then Vstar_YEARLY_ALL=fltarr(nr,n_elements(ilev),ndays,nyear)
    if svar(ivar) eq 'Vstar' then Vstar_YEARLY_ALL(*,*,*,iyear-1)=Vstar_YEARLY

    if iyear eq 1L and svar(ivar) eq 'Wstar' then Wstar_YEARLY_ALL=fltarr(nr,n_elements(ilev),ndays,nyear)
    if svar(ivar) eq 'Wstar' then Wstar_YEARLY_ALL(*,*,*,iyear-1)=Wstar_YEARLY

    if iyear eq 1L and svar(ivar) eq 'Z3' then Z3_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'Z3' then Z3_YEARLY_ALL(*,*,*,iyear-1)=Z3_YEARLY

    if iyear eq 1L and svar(ivar) eq 'T' then T_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'T' then T_YEARLY_ALL(*,*,*,iyear-1)=T_YEARLY
endfor
endfor
;
; calculate daily means and sigmas
;
EKGWSPEC_YEARLY_MEAN=mean(EKGWSPEC_YEARLY_ALL,dim=4)
OMEGA_YEARLY_MEAN=mean(OMEGA_YEARLY_ALL,dim=4)
QJOULE_YEARLY_MEAN=mean(QJOULE_YEARLY_ALL,dim=4)
QRS_AUR_YEARLY_MEAN=mean(QRS_AUR_YEARLY_ALL,dim=4)
QRS_EUV_YEARLY_MEAN=mean(QRS_EUV_YEARLY_ALL,dim=4)
TTGW_YEARLY_MEAN=mean(TTGW_YEARLY_ALL,dim=4)
UTGWSPEC_YEARLY_MEAN=mean(UTGWSPEC_YEARLY_ALL,dim=4)
Vstar_YEARLY_MEAN=mean(Vstar_YEARLY_ALL,dim=4)
Wstar_YEARLY_MEAN=mean(Wstar_YEARLY_ALL,dim=4)
Z3_YEARLY_MEAN=mean(Z3_YEARLY_ALL,dim=4)
T_YEARLY_MEAN=mean(T_YEARLY_ALL,dim=4)

EKGWSPEC_YEARLY_SIGMA=stddev(EKGWSPEC_YEARLY_ALL,dim=4)
OMEGA_YEARLY_SIGMA=stddev(OMEGA_YEARLY_ALL,dim=4)
QJOULE_YEARLY_SIGMA=stddev(QJOULE_YEARLY_ALL,dim=4)
QRS_AUR_YEARLY_SIGMA=stddev(QRS_AUR_YEARLY_ALL,dim=4)
QRS_EUV_YEARLY_SIGMA=stddev(QRS_EUV_YEARLY_ALL,dim=4)
TTGW_YEARLY_SIGMA=stddev(TTGW_YEARLY_ALL,dim=4)
UTGWSPEC_YEARLY_SIGMA=stddev(UTGWSPEC_YEARLY_ALL,dim=4)
Vstar_YEARLY_SIGMA=stddev(Vstar_YEARLY_ALL,dim=4)
Wstar_YEARLY_SIGMA=stddev(Wstar_YEARLY_ALL,dim=4)
Z3_YEARLY_SIGMA=stddev(Z3_YEARLY_ALL,dim=4)
T_YEARLY_SIGMA=stddev(T_YEARLY_ALL,dim=4)
;
; loop over days of the year and save ZM daily means and sigmas
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
    ofile=dirzm+'ZM_CO2x1SmidEmax_yBWCN_'+mmdd(iday)+'.sav'
    dum=file_search(ofile)
;   if dum(0) ne '' then goto,skipday
;
; today
;
    EKGWSPEC_DAILY_MEAN=reform(EKGWSPEC_YEARLY_MEAN(*,*,iday))
    OMEGA_DAILY_MEAN=reform(OMEGA_YEARLY_MEAN(*,*,iday))
    QJOULE_DAILY_MEAN=reform(QJOULE_YEARLY_MEAN(*,*,iday))*86400.
    QRS_AUR_DAILY_MEAN=reform(QRS_AUR_YEARLY_MEAN(*,*,iday))*86400.
    QRS_EUV_DAILY_MEAN=reform(QRS_EUV_YEARLY_MEAN(*,*,iday))*86400.
    TTGW_DAILY_MEAN=reform(TTGW_YEARLY_MEAN(*,*,iday))*86400.
    UTGWSPEC_DAILY_MEAN=reform(UTGWSPEC_YEARLY_MEAN(*,*,iday))*86400.
    Vstar_DAILY_MEAN=reform(Vstar_YEARLY_MEAN(*,*,iday))
    Wstar_DAILY_MEAN=reform(Wstar_YEARLY_MEAN(*,*,iday))
    Z3_DAILY_MEAN=reform(Z3_YEARLY_MEAN(*,*,iday))
    T_DAILY_MEAN=reform(T_YEARLY_MEAN(*,*,iday))

    EKGWSPEC_DAILY_SIGMA=reform(EKGWSPEC_YEARLY_SIGMA(*,*,iday))
    OMEGA_DAILY_SIGMA=reform(OMEGA_YEARLY_SIGMA(*,*,iday))
    QJOULE_DAILY_SIGMA=reform(QJOULE_YEARLY_SIGMA(*,*,iday))*86400.
    QRS_AUR_DAILY_SIGMA=reform(QRS_AUR_YEARLY_SIGMA(*,*,iday))*86400.
    QRS_EUV_DAILY_SIGMA=reform(QRS_EUV_YEARLY_SIGMA(*,*,iday))*86400.
    TTGW_DAILY_SIGMA=reform(TTGW_YEARLY_SIGMA(*,*,iday))*86400.
    UTGWSPEC_DAILY_SIGMA=reform(UTGWSPEC_YEARLY_SIGMA(*,*,iday))*86400.
    Vstar_DAILY_SIGMA=reform(Vstar_YEARLY_SIGMA(*,*,iday))
    Wstar_DAILY_SIGMA=reform(Wstar_YEARLY_SIGMA(*,*,iday))
    Z3_DAILY_SIGMA=reform(Z3_YEARLY_SIGMA(*,*,iday))
    T_DAILY_SIGMA=reform(T_YEARLY_SIGMA(*,*,iday))
;
; save daily means and sigmas of all years
;
    print,'saving '+ofile
    save,filename=ofile,nr,lat,lev,ilev,EKGWSPEC_DAILY_MEAN,OMEGA_DAILY_MEAN,QJOULE_DAILY_MEAN,QRS_AUR_DAILY_MEAN,QRS_EUV_DAILY_MEAN,TTGW_DAILY_MEAN,$
         UTGWSPEC_DAILY_MEAN,Vstar_DAILY_MEAN,Wstar_DAILY_MEAN,Z3_DAILY_MEAN,T_DAILY_MEAN,EKGWSPEC_DAILY_SIGMA,OMEGA_DAILY_SIGMA,QJOULE_DAILY_SIGMA,$
         QRS_AUR_DAILY_SIGMA,QRS_EUV_DAILY_SIGMA,TTGW_DAILY_SIGMA,UTGWSPEC_DAILY_SIGMA,Vstar_DAILY_SIGMA,Wstar_DAILY_SIGMA,Z3_DAILY_SIGMA,T_DAILY_SIGMA
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_zm_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; plot
;
    zyz=Z3_DAILY_MEAN/1000.
    erase
    xyouts,.4,.975,'WACCM '+mmdd(iday),/normal,color=0,charthick=2,charsize=2
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    nlvls=21
    level=3+3*findgen(nlvls)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,reform(EKGWSPEC_DAILY_MEAN(*,0:n_elements(lev)-1)),lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],$
            ytitle='Altitude (km)',charsize=1.5,charthick=2,title='Kzz (m2/s)'
    contour,reform(EKGWSPEC_DAILY_SIGMA(*,0:n_elements(lev)-1)),lat,zyz,/noera,/foll,color=0,thick=2,levels=5*findgen(nlvls),/overplot
;
; convert omega to w
;
    rgas=287.058								; J/(kg-K) => m2/(s2 K)
    g=9.80665									; m/s2
    rho=0.*OMEGA_DAILY_MEAN
    for i=0L,nr-1L do rho(i,*)= (lev*100.)/(rgas*T_DAILY_MEAN(i,*))		; density => kg/m3 (lev in Pascals)
    wyz=-100.0*OMEGA_DAILY_MEAN/(rho*g)						; cm/s
    wsigyz=100.0*OMEGA_DAILY_SIGMA/(rho*g)					; sigma is positive

    level=-10+1.*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,wyz,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='W (cm/s)'
;   contour,wsigyz,lat,zyz,/noeras,/foll,color=0,thick=2,levels=findgen(20),/overplot

    level=5.+5.*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,QJOULE_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='QJOULE (K/day)'
    contour,QJOULE_DAILY_SIGMA,lat,zyz,/noeras,/foll,color=0,thick=2,levels=5*findgen(20),/overplot

    level=1.+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,QRS_AUR_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,ytitle='Altitude (km)',title='QRS_AUR (K/day)'
    contour,QRS_AUR_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=5*findgen(20),/overplot,thick=2
    
    level=1.+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(4)
    xmx=xorig(4)+xlen
    ymn=yorig(4)
    ymx=yorig(4)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,QRS_EUV_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='QRS_EUV (K/day)'
    contour,QRS_EUV_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=5*findgen(20),/overplot,thick=2

    level=2.+2.*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(5)
    xmx=xorig(5)+xlen
    ymn=yorig(5)
    ymx=yorig(5)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,TTGW_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='TTGW (K/day)'
    contour,TTGW_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=2*findgen(20),/overplot,thick=2

    level=-100+10*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(6)
    xmx=xorig(6)+xlen
    ymn=yorig(6)
    ymx=yorig(6)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,UTGWSPEC_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude',title='UTGWSPEC (m/s/day)',ytitle='Altitude (km)'
    contour,UTGWSPEC_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=20*findgen(20),/overplot,thick=2

    level=-20+2*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(7)
    xmx=xorig(7)+xlen
    ymn=yorig(7)
    ymx=yorig(7)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,reform(Vstar_DAILY_MEAN(*,0:n_elements(lev)-1)),lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],xtitle='Latitude',yrange=[30,125],charsize=1.5,charthick=2,title='Vstar (m/s)'
    contour,reform(Vstar_DAILY_SIGMA(*,0:n_elements(lev)-1)),lat,zyz,/noera,/foll,color=0,thick=2,levels=findgen(nlvls),/overplot

    level=-10+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(8)
    xmx=xorig(8)+xlen
    ymn=yorig(8)
    ymx=yorig(8)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,100*reform(Wstar_DAILY_MEAN(*,0:n_elements(lev)-1)),lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],xtitle='Latitude',yrange=[30,125],charsize=1.5,charthick=2,title='Wstar (cm/s)'
    contour,100*reform(Wstar_DAILY_SIGMA(*,0:n_elements(lev)-1)),lat,zyz,/noera,/foll,color=0,thick=2,levels=findgen(nlvls),/overplot
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim YZ_figures/yz_zm_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps -rotate -90 YZ_figures/yz_zm_avg+sig_waccm_smidemax_'+mmdd(iday)+'.jpg'
       spawn,'rm -f YZ_figures/yz_zm_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
    endif

skipday:
endfor	; loop over days of the year
end
