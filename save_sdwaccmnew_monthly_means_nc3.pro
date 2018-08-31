;
; save SD-WACCM multi-year monthly means (12 files)
; 
@rd_sdwaccm4_nc3_new

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,1.5*cos(a),1.5*sin(a),/fill
nxdim=700
nydim=700
xorig=[0.15]
yorig=[0.25]
xlen=0.8
ylen=0.7
cbaryoff=0.1
cbarydel=0.01
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif

PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
for imonth=1,12 do begin
    smonth=string(format='(i2.2)',imonth)
    ofile=dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.'+smonth+'.nc3.sav'
    print,'MONTH '+smonth
goto,quick
    ifiles=file_search(dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.2???'+smonth+'??.nc3',count=nfile)
;
; loop over files
;
    FOR n=0l,nfile-1l DO BEGIN
        result=strsplit(ifiles(n),'.',/extract)
        sdate=result(-2)
        print,sdate
;
; read nc3 data
;
        rd_sdwaccm4_nc3_new,ifiles(n),nc,nr,nth,alon,alat,th,$
           pvgrd,pgrd,zgrd,ugrd,vgrd,qgrd,qdfgrd,markgrd,sfgrd,h2ogrd,n2ogrd,o3grd,co2grd,nogrd,no2grd,iflg
;
; normalize marker
;
        index=where(markgrd lt 0.)
        if index(0) ne -1L then markgrd(index)=-1.
;
; on first day of this month
;
        if n eq 0L then begin
           pvgrd_all=pvgrd
           pgrd_all=pgrd
           zgrd_all=zgrd
           ugrd_all=ugrd
           vgrd_all=vgrd
           qgrd_all=qgrd
           qdfgrd_all=qdfgrd
           markgrd_all=markgrd
           sfgrd_all=sfgrd
           h2ogrd_all=h2ogrd
           n2ogrd_all=n2ogrd
           o3grd_all=o3grd
           co2grd_all=co2grd
           nogrd_all=nogrd
           no2grd_all=no2grd
        endif
        if n gt 0L then begin
           pvgrd_all=pvgrd_all+pvgrd
           pgrd_all=pgrd_all+pgrd
           zgrd_all=zgrd_all+zgrd
           ugrd_all=ugrd_all+ugrd
           vgrd_all=vgrd_all+vgrd
           qgrd_all=qgrd_all+qgrd
           qdfgrd_all=qdfgrd_all+qdfgrd
           markgrd_all=markgrd_all+markgrd
           sfgrd_all=sfgrd_all+sfgrd
           h2ogrd_all=h2ogrd_all+h2ogrd
           n2ogrd_all=n2ogrd_all+n2ogrd
           o3grd_all=o3grd_all+o3grd
           co2grd_all=co2grd_all+co2grd
           nogrd_all=nogrd_all+nogrd
           no2grd_all=no2grd_all+no2grd
        endif
    ENDFOR
;
; average
;
    pvgrd_all=pvgrd_all/float(nfile)
    pgrd_all=pgrd_all/float(nfile)
    zgrd_all=zgrd_all/float(nfile)
    ugrd_all=ugrd_all/float(nfile)
    vgrd_all=vgrd_all/float(nfile)
    qgrd_all=qgrd_all/float(nfile)
    qdfgrd_all=qdfgrd_all/float(nfile)
    markgrd_all=markgrd_all/float(nfile)
    sfgrd_all=sfgrd_all/float(nfile)
    h2ogrd_all=h2ogrd_all/float(nfile)
    n2ogrd_all=n2ogrd_all/float(nfile)
    o3grd_all=o3grd_all/float(nfile)
    co2grd_all=co2grd_all/float(nfile)
    nogrd_all=nogrd_all/float(nfile)
    no2grd_all=no2grd_all/float(nfile)
;
; save
;
    save,file=ofile,nc,nr,nth,alon,alat,th,pvgrd_all,pgrd_all,zgrd_all,ugrd_all,vgrd_all,qgrd_all,qdfgrd_all,$
        markgrd_all,sfgrd_all,h2ogrd_all,n2ogrd_all,o3grd_all,co2grd_all,nogrd_all,no2grd_all
quick:
    restore,ofile
;
; save postscript version
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='ZM_Plots/yz_sdwaccm_U+Mark_new_'+smonth+'.ps'
       device,/color
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
       !p.thick=2.0                   ;Plotted lines twice as thick
    endif

    uzm=mean(ugrd_all,dim=2)
    zzm=mean(zgrd_all,dim=2)/1000.
    markzm=mean(markgrd_all,dim=2)
 
    erase
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    level=-100.+10.*findgen(21)
    nlvls=n_elements(level)
    col1=1+(indgen(nlvls)/float(nlvls))*mcolor
    contour,uzm,alat,zzm,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[-90.,90.],charsize=2,charthick=2,$
        yrange=[30.,120.],title='SDWACCM '+smonth,ytitle='Altitude (km)',xtitle='Latitude',color=0
    index=where(level gt 0.)
    contour,uzm,alat,zzm,levels=level(index),/follow,color=0,/overplot,/noeras
    index=where(level lt 0.)
    contour,uzm,alat,zzm,levels=level(index),/follow,color=mcolor,/overplot,/noeras
;   contour,uzm,alat,zzm,levels=[0],/follow,color=0,/overplot,/noeras,thick=3

    contour,markzm,alat,zzm,levels=0.1+0.1*findgen(10),/follow,color=0,/noeras,/overplot,thick=4
    contour,markzm,alat,zzm,levels=-1+0.1*findgen(10),/follow,color=mcolor,/noeras,/overplot,thick=4
;   contour,tzm,alat,zzm,levels=100+10*findgen(6),/follow,color=0.1*mcolor,/noeras,/overplot,thick=4

    imin=min(level)
    imax=max(level)
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(0)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='Zonal Wind Speed (m/s)',/noeras,charsize=1.5,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dxx=(imax-imin)/float(nlvls)
    for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
    endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim ZM_Plots/yz_sdwaccm_U+Mark_new_'+smonth+'.ps -rotate -90 ZM_Plots/yz_sdwaccm_U+Mark_new_'+smonth+'.png'
    endif

jumpstep:
ENDFOR		; LOOP OVER MONTHS
end
