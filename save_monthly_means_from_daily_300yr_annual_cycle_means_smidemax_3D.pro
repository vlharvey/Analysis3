;
; save monthly means from previously saved 3d annual cycle averages
; SmidEmax 300 years
; input: Jan 1 is the average of ALL January 1sts.
; save 3D monthly means
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
xorig=[0.15]
yorig=[0.25]
xlen=0.8
ylen=0.7
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonthlab=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)
;
; loop over months
;
for imonth=1,12 do begin
    smonth=string(format='(i2.2)',imonth)
    print,'MONTH '+smonth
    ifiles=file_search(dir+smonth+'??.sav',count=nfile)
;
; loop over files
;
    FOR n=0l,nfile-1l DO BEGIN
        result=strsplit(ifiles(n),'.',/extract)
        sdate=result(-2)
        print,sdate
;
; restore daily mean/sigma of all years
;
        restore,ifiles(n)	;,nc,nr,nth,alon,alat,th,ipvavg,pavg,uavg,vavg,qdfavg,coavg,zavg,sfavg,mavg,$
;                   ipvsig,psig,usig,vsig,qdfsig,cosig,zsig,sfsig,msig
; sum
;
        if n eq 0L then begin
           COAVG_ALL=COAVG	;            FLOAT     = Array[96, 144, 22]
           COSIG_ALL=COSIG	;            FLOAT     = Array[96, 144, 22]
           IPVAVG_ALL=IPVAVG	;            FLOAT     = Array[96, 144, 22]
           IPVSIG_ALL=IPVSIG	;            FLOAT     = Array[96, 144, 22]
           MAVG_ALL=MAVG	;            FLOAT     = Array[96, 144, 22]
           MSIG_ALL=MSIG	;            FLOAT     = Array[96, 144, 22]
           PAVG_ALL=PAVG	;            FLOAT     = Array[96, 144, 22]
           PSIG_ALL=PSIG	;            FLOAT     = Array[96, 144, 22]
           QDFAVG_ALL=QDFAVG	;            FLOAT     = Array[96, 144, 22]
           QDFSIG_ALL=QDFSIG	;            FLOAT     = Array[96, 144, 22]
           SFAVG_ALL=SFAVG	;            FLOAT     = Array[96, 144, 22]
           SFSIG_ALL=SFSIG	;            FLOAT     = Array[96, 144, 22]
           UAVG_ALL=UAVG	;            FLOAT     = Array[96, 144, 22]
           USIG_ALL=USIG	;            FLOAT     = Array[96, 144, 22]
           VAVG_ALL=VAVG	;            FLOAT     = Array[96, 144, 22]
           VSIG_ALL=VSIG	;            FLOAT     = Array[96, 144, 22]
           ZAVG_ALL=ZAVG	;            FLOAT     = Array[96, 144, 22]
           ZSIG_ALL=ZSIG	;            FLOAT     = Array[96, 144, 22]
        endif
        if n gt 0L then begin
           COAVG_ALL=COAVG_ALL+COAVG
           COSIG_ALL=COSIG_ALL+COSIG
           IPVAVG_ALL=IPVAVG_ALL+IPVAVG
           IPVSIG_ALL=IPVSIG_ALL+IPVSIG
           MAVG_ALL=MAVG_ALL+MAVG
           MSIG_ALL=MSIG_ALL+MSIG
           PAVG_ALL=PAVG_ALL+PAVG
           PSIG_ALL=PSIG_ALL+PSIG
           QDFAVG_ALL=QDFAVG_ALL+QDFAVG
           QDFSIG_ALL=QDFSIG_ALL+QDFSIG
           SFAVG_ALL=SFAVG_ALL+SFAVG
           SFSIG_ALL=SFSIG_ALL+SFSIG
           UAVG_ALL=UAVG_ALL+UAVG
           USIG_ALL=USIG_ALL+USIG
           VAVG_ALL=VAVG_ALL+VAVG
           VSIG_ALL=VSIG_ALL+VSIG
           ZAVG_ALL=ZAVG_ALL+ZAVG
           ZSIG_ALL=ZSIG_ALL+ZSIG
        endif
    endfor  ; loop over days this month
;
; average
;
    COAVG=COAVG_ALL/float(nfile)
    COSIG=COSIG_ALL/float(nfile)
    IPVAVG=IPVAVG_ALL/float(nfile)
    IPVSIG=IPVSIG_ALL/float(nfile)
    MAVG=MAVG_ALL/float(nfile)
    MSIG=MSIG_ALL/float(nfile)
    PAVG=PAVG_ALL/float(nfile)
    PSIG=PSIG_ALL/float(nfile)
    QDFAVG=QDFAVG_ALL/float(nfile)
    QDFSIG=QDFSIG_ALL/float(nfile)
    SFAVG=SFAVG_ALL/float(nfile)
    SFSIG=SFSIG_ALL/float(nfile)
    UAVG=UAVG_ALL/float(nfile)
    USIG=USIG_ALL/float(nfile)
    VAVG=VAVG_ALL/float(nfile)
    VSIG=VSIG_ALL/float(nfile)
    ZAVG=ZAVG_ALL/float(nfile)
    ZSIG=ZSIG_ALL/float(nfile)
;
; save multi-year monthly means
;
    save,file=dir+smonth+'.sav',nc,nr,nth,alon,alat,th,ipvavg,pavg,uavg,vavg,qdfavg,coavg,zavg,sfavg,mavg,$
         ipvsig,psig,usig,vsig,qdfsig,cosig,zsig,sfsig,msig
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_ubarave_waccm_smidemax_'+smonth+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; calculate zonal means
;
    uyz=mean(uavg,dim=2)
    coyz=mean(coavg,dim=2)
    zyz=mean(zavg,dim=2)
    myz=mean(mavg,dim=2)

    usigyz=mean(usig,dim=2)
    cosigyz=mean(cosig,dim=2)
    msigyz=mean(msig,dim=2)
;
; plot
;
    x2d=0.*zyz
    for k=0L,nth-1L do x2d(*,k)=alat
    erase
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    nlvls=21
    ulevel=-100+10.*findgen(nlvls)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,uyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=ulevel,xrange=[-90,90],yrange=[30,120],charsize=1.5,charthick=2,xtitle='Latitude',title=smonth,ytitle='Altitude (km)'
    index=where(ulevel gt 0.)
    contour,uyz,alat,zyz,/noera,/foll,color=0,levels=ulevel(index),/overplot
    index=where(ulevel lt 0.)
    contour,uyz,alat,zyz,/noera,/foll,color=mcolor,c_linestyle=5,levels=ulevel(index),/overplot

;   contour,usigyz,alat,zyz,/noeras,/foll,color=0,thick=2,levels=5*findgen(20),/overplot
    qlevel=-1.+.1*findgen(nlvls)
    index=where(qlevel gt 0.)
    contour,myz,alat,zyz,/noera,/foll,color=0,levels=qlevel(index),/overplot,thick=3
    index=where(qlevel lt 0.)
    contour,myz,alat,zyz,/noera,/foll,color=mcolor,levels=qlevel(index),/overplot,thick=3

    imin=min(ulevel)
    imax=max(ulevel)
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(0)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='SmidEmax FR-WACCM Ubar (m/s)',/noeras,charsize=1.5,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dxx=(imax-imin)/float(nlvls)
    for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
    endfor
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim YZ_figures/yz_ubarave_waccm_smidemax_'+smonth+'.ps -rotate -90 YZ_figures/yz_ubarave_waccm_smidemax_'+smonth+'.jpg'
       spawn,'rm -f YZ_figures/yz_ubarave_waccm_smidemax_'+smonth+'.ps'
    endif

endfor	; loop over months
end
