;
; save average and sigma annual cycle of zonal mean chemicals
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
xorig=[0.15,0.15]
yorig=[0.6,0.15]
xlen=0.7
ylen=0.35
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
dir2='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/ZMCHEM_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)

re=40000./2./!pi
earth_area=4.*!pi*re*re
hem_area=earth_area/2.0
rtd=double(180./!pi)
dtr=1./rtd
nrr=91L
yeq=findgen(nrr)
latcircle=fltarr(nrr)
hem_frac=fltarr(nrr)
for j=0,nrr-2 do begin
    hy=re*dtr
    dx=re*cos(yeq(j)*dtr)*360.*dtr
    latcircle(j)=dx*hy
endfor
for j=0,nrr-1 do begin
    if yeq(j) ge 0. then index=where(yeq ge yeq(j))
    if index(0) ne -1 then hem_frac(j)=100.*total(latcircle(index))/hem_area
    if yeq(j) eq 0. then hem_frac(j)=100.
endfor

;goto,quick
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
; loop over days of the year
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
;
; restore daily mean of all years
;
; CLONO2_DAILY_MEAN FLOAT     = Array[96, 66]
; CLONO2_DAILY_SIGMA FLOAT     = Array[96, 66]
; CO_DAILY_MEAN   FLOAT     = Array[96, 66]
; CO_DAILY_SIGMA  FLOAT     = Array[96, 66]
; LAT             FLOAT     = Array[96]
; LEV             FLOAT     = Array[66]
; NOX_DAILY_MEAN  FLOAT     = Array[96, 66]
; NOX_DAILY_SIGMA FLOAT     = Array[96, 66]
; NOY_DAILY_MEAN  FLOAT     = Array[96, 66]
; NOY_DAILY_SIGMA FLOAT     = Array[96, 66]
; NO_DAILY_MEAN   FLOAT     = Array[96, 66]
; NO_DAILY_SIGMA  FLOAT     = Array[96, 66]
; NR              LONG      =           96
; O3_DAILY_MEAN   FLOAT     = Array[96, 66]
; O3_DAILY_SIGMA  FLOAT     = Array[96, 66]
; Z3_DAILY_MEAN   FLOAT     = Array[96, 66]
; Z3_DAILY_SIGMA  FLOAT     = Array[96, 66]

    restore,dir2+mmdd(iday)+'.sav'
    nl=n_elements(lev)
;
; declare arrays on the first day
;
    if iday eq 0L then begin
       clono2nh=fltarr(ndays,nl)
       conh=fltarr(ndays,nl)
       noxnh=fltarr(ndays,nl)
       noynh=fltarr(ndays,nl)
       nonh=fltarr(ndays,nl)
       o3nh=fltarr(ndays,nl)
       z3nh=fltarr(ndays,nl)

       clono2sh=fltarr(ndays,nl)
       cosh0=fltarr(ndays,nl)
       noxsh=fltarr(ndays,nl)
       noysh=fltarr(ndays,nl)
       nosh=fltarr(ndays,nl)
       o3sh=fltarr(ndays,nl)	; fill and save
       z3sh=fltarr(ndays,nl)

       rlat=60.
       alat=lat
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       nhlat=index(0)
       rlat=-60.
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       shlat=index(0)
    endif
;
; extract daily means in the polar regions
;
    clono2nh(iday,*)=mean(CLONO2_DAILY_MEAN(nhlat:-1,*),dim=1)
    conh(iday,*)=mean(CO_DAILY_MEAN(nhlat:-1,*),dim=1)
    noxnh(iday,*)=mean(NOX_DAILY_MEAN(nhlat:-1,*),dim=1)
    noynh(iday,*)=mean(NOY_DAILY_MEAN(nhlat:-1,*),dim=1)
    nonh(iday,*)=mean(NO_DAILY_MEAN(nhlat:-1,*),dim=1)
    o3nh(iday,*)=mean(O3_DAILY_MEAN(nhlat:-1,*),dim=1)
    z3nh(iday,*)=mean(Z3_DAILY_MEAN(nhlat:-1,*),dim=1)/1000.

    clono2sh(iday,*)=mean(CLONO2_DAILY_MEAN(0:shlat,*),dim=1)
    cosh0(iday,*)=mean(CO_DAILY_MEAN(0:shlat,*),dim=1)
    noxsh(iday,*)=mean(NOX_DAILY_MEAN(0:shlat,*),dim=1)
    noysh(iday,*)=mean(NOY_DAILY_MEAN(0:shlat,*),dim=1)
    nosh(iday,*)=mean(NO_DAILY_MEAN(0:shlat,*),dim=1)  
    o3sh(iday,*)=mean(O3_DAILY_MEAN(0:shlat,*),dim=1)
    z3sh(iday,*)=mean(Z3_DAILY_MEAN(0:shlat,*),dim=1)/1000.
endfor
;
; save file
;
save,file='zt_ZMCHEM_NH_SH_smidemax.sav',mmdd,ndays,clono2nh,conh,noxnh,noynh,$
     nonh,o3nh,z3nh,clono2sh,cosh0,noxsh,noysh,nosh,o3sh,z3sh
quick:
;restore,'zt_ZMCHEM_NH_SH_smidemax.sav'
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='zt_ZMCHEM_NH_SH_smidemax.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; plot
;
    x2d=0.*z3nh
    for k=0L,nl-1L do x2d(*,k)=findgen(ndays)
    smon=strmid(mmdd,0,2)
    sday=strmid(mmdd,2,2)
    xindex=where(sday eq '15',nxticks)
    xlabs=smon(xindex)
    erase
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    nlvls=21
    tlevel=2*findgen(nlvls)
    tlevel(0)=0.1
    nlvls=n_elements(tlevel)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,conh*1.e6,findgen(ndays),z3nh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='NH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    contour,conh*1.e6,findgen(ndays),z3nh,/noera,/foll,color=mcolor,thick=2,levels=tlevel,/overplot

    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,cosh0*1.e6,findgen(ndays),z3sh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='SH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    index=where(tlevel gt 0.)
    contour,cosh0*1.e6,findgen(ndays),z3sh,/noera,/foll,color=0,thick=2,levels=tlevel(index),/overplot
;
; horizontal color bar
;
!type=2^2+2^3+2^6
x0=xmn
x1=xmx
y0=ymn-0.1
y1=ymn-0.05
level=tlevel
nlvls  = n_elements(level)
col1 = (1 + indgen(nlvls)) * 255. / nlvls    ; define colors
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,(x0+x1)/3.,y0-0.03,'60-90 CO (ppmv)',color=0,charsize=1.5,charthick=2,/normal
slab=strcompress(string(format='(i3)',long(level)),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[0:10] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
    xyouts,x1-dx/2.,5,slab(i),charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim zt_ZMCHEM_NH_SH_smidemax.ps -rotate -90 zt_ZMCHEM_NH_SH_smidemax.jpg
;      spawn,'rm -f zt_ZMCHEM_NH_SH_smidemax.ps'
    endif

end
