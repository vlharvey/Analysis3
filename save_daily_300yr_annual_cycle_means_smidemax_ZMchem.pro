;
; zonal mean version for chemicals
; SmidEmax 300 years
; i.e., Jan 1 is the average of ALL January 1sts.
; save zonal means and sigmas
;
; CLONO2, CO, NO, NOx, NOy, O3
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
xorig=[0.1,0.4,0.7,0.1,0.4,0.7]
yorig=[0.55,0.55,0.55,0.15,.15,.15]
xlen=0.225
ylen=0.3
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
svar=['CLONO2', 'CO', 'NO', 'NOx', 'NOy', 'O3', 'Z3']
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
    endif
    if iyear eq 1L and svar(ivar) eq 'CLONO2' then CLONO2_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'CLONO2' then CLONO2_YEARLY_ALL(*,*,*,iyear-1)=CLONO2_YEARLY

    if iyear eq 1L and svar(ivar) eq 'CO' then CO_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'CO' then CO_YEARLY_ALL(*,*,*,iyear-1)=CO_YEARLY

    if iyear eq 1L and svar(ivar) eq 'NO' then NO_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'NO' then NO_YEARLY_ALL(*,*,*,iyear-1)=NO_YEARLY

    if iyear eq 1L and svar(ivar) eq 'NOx' then NOx_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'NOx' then NOx_YEARLY_ALL(*,*,*,iyear-1)=NOx_YEARLY

    if iyear eq 1L and svar(ivar) eq 'NOy' then NOy_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'NOy' then NOy_YEARLY_ALL(*,*,*,iyear-1)=NOy_YEARLY

    if iyear eq 1L and svar(ivar) eq 'O3' then O3_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'O3' then O3_YEARLY_ALL(*,*,*,iyear-1)=O3_YEARLY

    if iyear eq 1L and svar(ivar) eq 'Z3' then Z3_YEARLY_ALL=fltarr(nr,n_elements(lev),ndays,nyear)
    if svar(ivar) eq 'Z3' then Z3_YEARLY_ALL(*,*,*,iyear-1)=Z3_YEARLY

endfor
endfor
;
; calculate daily means and sigmas
;
CLONO2_YEARLY_MEAN=mean(CLONO2_YEARLY_ALL,dim=4)
CO_YEARLY_MEAN=mean(CO_YEARLY_ALL,dim=4)
NO_YEARLY_MEAN=mean(NO_YEARLY_ALL,dim=4)
NOx_YEARLY_MEAN=mean(NOx_YEARLY_ALL,dim=4)
NOy_YEARLY_MEAN=mean(NOy_YEARLY_ALL,dim=4)
O3_YEARLY_MEAN=mean(O3_YEARLY_ALL,dim=4)
Z3_YEARLY_MEAN=mean(Z3_YEARLY_ALL,dim=4)

CLONO2_YEARLY_SIGMA=stddev(CLONO2_YEARLY_ALL,dim=4)
CO_YEARLY_SIGMA=stddev(CO_YEARLY_ALL,dim=4)
NO_YEARLY_SIGMA=stddev(NO_YEARLY_ALL,dim=4)
NOx_YEARLY_SIGMA=stddev(NOx_YEARLY_ALL,dim=4)
NOy_YEARLY_SIGMA=stddev(NOy_YEARLY_ALL,dim=4)
O3_YEARLY_SIGMA=stddev(O3_YEARLY_ALL,dim=4)
Z3_YEARLY_SIGMA=stddev(Z3_YEARLY_ALL,dim=4)
;
; loop over days of the year and save ZM daily means and sigmas
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
    ofile=dirzm+'ZMCHEM_CO2x1SmidEmax_yBWCN_'+mmdd(iday)+'.sav'
    dum=file_search(ofile)
;   if dum(0) ne '' then goto,skipday
;
; today
;
    CLONO2_DAILY_MEAN=reform(CLONO2_YEARLY_MEAN(*,*,iday))
    CO_DAILY_MEAN=reform(CO_YEARLY_MEAN(*,*,iday))
    NO_DAILY_MEAN=reform(NO_YEARLY_MEAN(*,*,iday))
    NOx_DAILY_MEAN=reform(NOx_YEARLY_MEAN(*,*,iday))
    NOy_DAILY_MEAN=reform(NOy_YEARLY_MEAN(*,*,iday))
    O3_DAILY_MEAN=reform(O3_YEARLY_MEAN(*,*,iday))
    Z3_DAILY_MEAN=reform(Z3_YEARLY_MEAN(*,*,iday))

    CLONO2_DAILY_SIGMA=reform(CLONO2_YEARLY_SIGMA(*,*,iday))
    CO_DAILY_SIGMA=reform(CO_YEARLY_SIGMA(*,*,iday))
    NO_DAILY_SIGMA=reform(NO_YEARLY_SIGMA(*,*,iday))
    NOx_DAILY_SIGMA=reform(NOx_YEARLY_SIGMA(*,*,iday))
    NOy_DAILY_SIGMA=reform(NOy_YEARLY_SIGMA(*,*,iday))
    O3_DAILY_SIGMA=reform(O3_YEARLY_SIGMA(*,*,iday))
    Z3_DAILY_SIGMA=reform(Z3_YEARLY_SIGMA(*,*,iday))
;
; save daily means and sigmas of all years
;
    print,'saving '+ofile
    save,filename=ofile,nr,lat,lev,CLONO2_DAILY_MEAN,CO_DAILY_MEAN,NO_DAILY_MEAN,NOx_DAILY_MEAN,NOy_DAILY_MEAN,O3_DAILY_MEAN,$
         Z3_DAILY_MEAN,CLONO2_DAILY_SIGMA,CO_DAILY_SIGMA,NO_DAILY_SIGMA,NOx_DAILY_SIGMA,NOy_DAILY_SIGMA,O3_DAILY_SIGMA,Z3_DAILY_SIGMA
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_zmchem_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
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
    level=0.05*findgen(nlvls)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    CLONO2_DAILY_MEAN=CLONO2_DAILY_MEAN*1.e9
    CLONO2_DAILY_SIGMA=CLONO2_DAILY_SIGMA*1.e9
    contour,reform(CLONO2_DAILY_MEAN(*,0:n_elements(lev)-1)),lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],$
            ytitle='Altitude (km)',charsize=1.5,charthick=2,title='CLONO2 (ppbv)'
    contour,reform(CLONO2_DAILY_SIGMA(*,0:n_elements(lev)-1)),lat,zyz,/noera,/foll,color=0,thick=2,levels=5*findgen(nlvls),/overplot

    level=2.*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    CO_DAILY_MEAN=CO_DAILY_MEAN*1.e6
    CO_DAILY_SIGMA=CO_DAILY_SIGMA*1.e6
    contour,CO_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='CO (ppmv)'
    contour,CO_DAILY_SIGMA,lat,zyz,/noeras,/foll,color=0,thick=2,levels=findgen(20),/overplot

    level=-10.+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    NO_DAILY_MEAN=alog(NO_DAILY_MEAN*1.e9)
    NO_DAILY_SIGMA=alog(NO_DAILY_SIGMA*1.e9)
    contour,NO_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='alog(NO) (ppbv)'
    contour,NO_DAILY_SIGMA,lat,zyz,/noeras,/foll,color=0,thick=2,levels=5*findgen(20),/overplot

    level=1.+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    NOx_DAILY_MEAN=alog(NOx_DAILY_MEAN*1.e9)
    NOx_DAILY_SIGMA=alog(NOx_DAILY_SIGMA*1.e9)
    contour,NOx_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,ytitle='Altitude (km)',title='alog(NOx) (ppbv)'
    contour,NOx_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=5*findgen(20),/overplot,thick=2
    
    level=1.+findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(4)
    xmx=xorig(4)+xlen
    ymn=yorig(4)
    ymx=yorig(4)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    NOy_DAILY_MEAN=alog(NOy_DAILY_MEAN*1.e9)
    NOy_DAILY_SIGMA=alog(NOy_DAILY_SIGMA*1.e9)
    contour,NOy_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='alog(NOy) (ppbv)'
    contour,NOy_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=5*findgen(20),/overplot,thick=2

    level=.5*findgen(nlvls)
    !type=2^2+2^3
    xmn=xorig(5)
    xmx=xorig(5)+xlen
    ymn=yorig(5)
    ymx=yorig(5)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    O3_DAILY_MEAN=O3_DAILY_MEAN*1.e6
    O3_DAILY_SIGMA=O3_DAILY_SIGMA*1.e6
    contour,O3_DAILY_MEAN,lat,zyz,/noera,/fill,color=0,c_color=col1,levels=level,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='O3 (ppmv)'
    contour,O3_DAILY_SIGMA,lat,zyz,/noera,/foll,color=0,levels=2*findgen(20),/overplot,thick=2
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim YZ_figures/yz_zmchem_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps -rotate -90 YZ_figures/yz_zmchem_avg+sig_waccm_smidemax_'+mmdd(iday)+'.jpg'
       spawn,'rm -f YZ_figures/yz_zmchem_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
    endif

skipday:
endfor	; loop over days of the year
end
