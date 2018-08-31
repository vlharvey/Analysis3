;
; save average and sigma annual cycle of Vstar and Wstar and other variables
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
dir2='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/ZM_CO2x1SmidEmax_yBWCN_'
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

goto,quick
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
    filenames=file_search(dir+'???'+mmdd(iday)+'.nc3')
    ofile=dir+mmdd(iday)+'.sav'
    dum=file_search(ofile)
;   if dum(0) ne '' then goto,skipday
;
; restore daily mean of all years
;
    ofile=dir+mmdd(iday)+'.sav'
    print,'reading '+ofile
;
; ALAT            FLOAT     = Array[96]
; ALON            FLOAT     = Array[144]
; COAVG           FLOAT     = Array[96, 144, 22]
; COSIG           FLOAT     = Array[96, 144, 22]
; IPVAVG          FLOAT     = Array[96, 144, 22]
; IPVSIG          FLOAT     = Array[96, 144, 22]
; MAVG            FLOAT     = Array[96, 144, 22]
; MSIG            FLOAT     = Array[96, 144, 22]
; NC              LONG      =          144
; NR              LONG      =           96
; NTH             LONG      =           22
; PAVG            FLOAT     = Array[96, 144, 22]
; PSIG            FLOAT     = Array[96, 144, 22]
; QDFAVG          FLOAT     = Array[96, 144, 22]
; QDFSIG          FLOAT     = Array[96, 144, 22]
; SFAVG           FLOAT     = Array[96, 144, 22]
; SFSIG           FLOAT     = Array[96, 144, 22]
; TH              FLOAT     = Array[22]
; UAVG            FLOAT     = Array[96, 144, 22]
; USIG            FLOAT     = Array[96, 144, 22]
; VAVG            FLOAT     = Array[96, 144, 22]
; VSIG            FLOAT     = Array[96, 144, 22]
; ZAVG            FLOAT     = Array[96, 144, 22]
;
    restore,ofile
;
; EKGWSPEC_DAILY_MEAN FLOAT     = Array[96, 67]
; EKGWSPEC_DAILY_SIGMA FLOAT     = Array[96, 67]
; ILEV            FLOAT     = Array[67]
; LAT             FLOAT     = Array[96]
; LEV             FLOAT     = Array[66]
; NR              LONG      =           96
; OMEGA_DAILY_MEAN FLOAT     = Array[96, 66]
; OMEGA_DAILY_SIGMA FLOAT     = Array[96, 66]
; QJOULE_DAILY_MEAN FLOAT     = Array[96, 66]
; QJOULE_DAILY_SIGMA FLOAT     = Array[96, 66]
; QRS_AUR_DAILY_MEAN FLOAT     = Array[96, 66]
; QRS_AUR_DAILY_SIGMA FLOAT     = Array[96, 66]
; QRS_EUV_DAILY_MEAN FLOAT     = Array[96, 66]
; QRS_EUV_DAILY_SIGMA FLOAT     = Array[96, 66]
; TTGW_DAILY_MEAN FLOAT     = Array[96, 66]
; TTGW_DAILY_SIGMA FLOAT     = Array[96, 66]
; T_DAILY_MEAN    FLOAT     = Array[96, 66]
; T_DAILY_SIGMA   FLOAT     = Array[96, 66]
; UTGWSPEC_DAILY_MEAN FLOAT     = Array[96, 66]
; UTGWSPEC_DAILY_SIGMA FLOAT     = Array[96, 66]
; VSTAR_DAILY_MEAN FLOAT     = Array[96, 67]
; VSTAR_DAILY_SIGMA FLOAT     = Array[96, 67]
; WSTAR_DAILY_MEAN FLOAT     = Array[96, 67]
; WSTAR_DAILY_SIGMA FLOAT     = Array[96, 67]
; Z3_DAILY_MEAN   FLOAT     = Array[96, 66]
; Z3_DAILY_SIGMA  FLOAT     = Array[96, 66]
;
    restore,dir2+mmdd(iday)+'.sav'
    nl=n_elements(lev)
    nl2=n_elements(ilev)
;
; declare arrays on the first day
;
    if iday eq 0L then begin
       tnh=fltarr(ndays,nth)
       unh=fltarr(ndays,nth)
       mnh=fltarr(ndays,nth)
       znh=fltarr(ndays,nth)

       ekgwspecnh=fltarr(ndays,nl2)
       omeganh=fltarr(ndays,nl)
       qjoulenh=fltarr(ndays,nl)
       qaurnh=fltarr(ndays,nl)
       ttgwnh=fltarr(ndays,nl)
       utgwspecnh=fltarr(ndays,nl)
       wstarnh=fltarr(ndays,nl2)
       vstarnh=fltarr(ndays,nl2)
       wstarznh=fltarr(ndays,nl)

       tsh=fltarr(ndays,nth)
       ush=fltarr(ndays,nth)
       msh=fltarr(ndays,nth)
       zsh=fltarr(ndays,nth)

       ekgwspecsh=fltarr(ndays,nl2)
       omegash=fltarr(ndays,nl)
       qjoulesh=fltarr(ndays,nl)
       qaursh=fltarr(ndays,nl)
       ttgwsh=fltarr(ndays,nl)
       utgwspecsh=fltarr(ndays,nl)	; fill and save
       wstarsh=fltarr(ndays,nl2)
       vstarsh=fltarr(ndays,nl2)
       wstarzsh=fltarr(ndays,nl)

       rlat=60.
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       nhlat=index(0)
       rlat=-60.
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       shlat=index(0)
    endif
;
; calculate temp
;
    tavg=0*pavg
    for k=0,nth-1 do tavg(*,*,k) = th(k)*( (pavg(*,*,k)/1000.)^(.286) )
;
; calculate zonal means
;
    ipvyz=mean(ipvavg,dim=2)
    for k=0L,nth-1L do ipvyz(*,k)=ipvyz(*,k)*((th(k)/2000.))^(-9./2.)
    pyz=mean(pavg,dim=2)
    uyz=mean(uavg,dim=2)
    vyz=mean(vavg,dim=2)
    qdfyz=mean(qdfavg,dim=2)
    coyz=mean(coavg,dim=2)
    zyz=mean(zavg,dim=2)
    sfyz=mean(sfavg,dim=2)
    myz=mean(mavg,dim=2)
    tyz=mean(tavg,dim=2)
;
; extract daily means in the polar regions
;
    tnh(iday,*)=mean(tyz(nhlat:-1,*),dim=1)			; polar cap average
    unh(iday,*)=reform(uyz(nhlat,*))				; at 60N
    mnh(iday,*)=mean(myz(nhlat:-1,*),dim=1)
    znh(iday,*)=mean(zyz(nhlat:-1,*),dim=1)
    tsh(iday,*)=mean(tyz(0:shlat,*),dim=1)
    ush(iday,*)=reform(uyz(shlat,*))
    msh(iday,*)=mean(myz(0:shlat,*),dim=1)
    zsh(iday,*)=mean(zyz(0:shlat,*),dim=1)

    ekgwspecnh(iday,*)=mean(EKGWSPEC_DAILY_MEAN(nhlat:-1,*),dim=1)
    omeganh(iday,*)=mean(OMEGA_DAILY_MEAN(nhlat:-1,*),dim=1)
    qjoulenh(iday,*)=mean(QJOULE_DAILY_MEAN(nhlat:-1,*),dim=1)
    qaurnh(iday,*)=mean(QRS_AUR_DAILY_MEAN(nhlat:-1,*),dim=1)
    ttgwnh(iday,*)=reform(TTGW_DAILY_MEAN(nhlat,*))
    utgwspecnh(iday,*)=reform(UTGWSPEC_DAILY_MEAN(nhlat,*))		; at 60N
    vstarnh(iday,*)=reform(VSTAR_DAILY_MEAN(nhlat,*))		; at 60N
    wstarnh(iday,*)=mean(WSTAR_DAILY_MEAN(nhlat:-1,*),dim=1)
    wstarznh(iday,*)=mean(Z3_DAILY_MEAN(nhlat:-1,*),dim=1)/1000.

    ekgwspecsh(iday,*)=mean(EKGWSPEC_DAILY_MEAN(0:shlat,*),dim=1)
    omegash(iday,*)=mean(OMEGA_DAILY_MEAN(0:shlat,*),dim=1)
    qjoulesh(iday,*)=mean(QJOULE_DAILY_MEAN(0:shlat,*),dim=1)
    qaursh(iday,*)=mean(QRS_AUR_DAILY_MEAN(0:shlat,*),dim=1)
    ttgwsh(iday,*)=reform(TTGW_DAILY_MEAN(shlat,*))  
    utgwspecsh(iday,*)=reform(UTGWSPEC_DAILY_MEAN(shlat,*))
    vstarsh(iday,*)=reform(VSTAR_DAILY_MEAN(shlat,*))
    wstarsh(iday,*)=mean(WSTAR_DAILY_MEAN(0:shlat,*),dim=1)
    wstarzsh(iday,*)=mean(Z3_DAILY_MEAN(0:shlat,*),dim=1)/1000.
endfor
;
; save file
;
save,file='zt_RCetal_NH_SH_smidemax.sav',th,mmdd,tnh,unh,mnh,znh,tsh,ush,msh,zsh,ndays,nth,ekgwspecnh,omeganh,qjoulenh,qaurnh,$
     ttgwnh,utgwspecnh,wstarnh,vstarnh,wstarznh,ekgwspecsh,omegash,qjoulesh,qaursh,ttgwsh,utgwspecsh,wstarsh,vstarsh,wstarzsh
quick:
;restore,'zt_RCetal_NH_SH_smidemax.sav'
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='zt_RCetal_NH_SH_smidemax.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; strip off top level of RC
; convert to mm/s
;
wstarnh=1000.*reform(wstarnh(*,0:nl-1))
wstarsh=1000.*reform(wstarsh(*,0:nl-1))
;
; plot
;
    x2d=0.*tnh
    for k=0L,nth-1L do x2d(*,k)=findgen(ndays)
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
    tlevel=-50+10*findgen(nlvls)
    nlvls=n_elements(tlevel)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,wstarnh,findgen(ndays),wstarznh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='NH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    index=where(tlevel gt 0.)
    contour,wstarnh,findgen(ndays),wstarznh,/noera,/foll,color=0,thick=2,levels=tlevel(index),/overplot
    index=where(tlevel lt 0.)
    contour,wstarnh,findgen(ndays),wstarznh,/noera,/foll,color=mcolor,thick=2,levels=tlevel(index),/overplot,c_linestyle=5
;   contour,mnh,findgen(ndays),znh,/noera,/foll,color=0,thick=2,levels=0.1*findgen(10),/overplot
;   contour,mnh,findgen(ndays),znh,/noera,/foll,color=mcolor,thick=2,levels=-1+0.1*findgen(10),/overplot

    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,wstarsh,findgen(ndays),wstarzsh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='SH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    index=where(tlevel gt 0.)
    contour,wstarsh,findgen(ndays),wstarzsh,/noera,/foll,color=0,thick=2,levels=tlevel(index),/overplot
    index=where(tlevel lt 0.)
    contour,wstarsh,findgen(ndays),wstarzsh,/noera,/foll,color=mcolor,thick=2,levels=tlevel(index),/overplot,c_linestyle=5
;   contour,msh,findgen(ndays),zsh,/noera,/foll,color=0,thick=2,levels=0.1*findgen(10),/overplot
;   contour,msh,findgen(ndays),zsh,/noera,/foll,color=mcolor,thick=2,levels=-1+0.1*findgen(10),/overplot
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
xyouts,(x0+x1)/3.,y0-0.03,'60-90 Temperature (K)',color=0,charsize=1.5,charthick=2,/normal
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
       spawn,'convert -trim zt_RCetal_NH_SH_smidemax.ps -rotate -90 zt_RCetal_NH_SH_smidemax.jpg
;      spawn,'rm -f zt_RCetal_NH_SH_smidemax.ps'
    endif

end
