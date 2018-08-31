;
; yz plots of SD-WACCM winds and the vortex marker
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
ifiles=file_search(dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.*.nc3',count=nfile)
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
; temperature
;
    tpgrd=0.*pgrd
    for k=0L,nth-1L do tpgrd(*,*,k)=th(k)*(pgrd(*,*,k)/1000.)^.286
;
; zonal means
;
    uzm=mean(ugrd,dim=2)
    markzm=mean(markgrd,dim=2)
    zzm=mean(zgrd,dim=2)/1000.
    tzm=mean(tpgrd,dim=2)
;
; min/max mark around a latitude circle
;
    markmin=0.*markzm
    markmax=0.*markzm
    for k=0L,nth-1L do begin
    for j=0L,nr-1L do begin
        markmin(j,k)=min(markgrd(j,*,k))
        markmax(j,k)=max(markgrd(j,*,k))
    endfor
    endfor
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
         device,/landscape,bits=8,filename='ZM_Plots/yz_sdwaccm_U+Mark_new_'+sdate+'.ps'
         device,/color
         device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                xsize=xsize,ysize=ysize
         !p.thick=2.0                   ;Plotted lines twice as thick
      endif

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
        yrange=[0.,120.],title='SDWACCM '+sdate,ytitle='Altitude (km)',xtitle='Latitude',color=0
;   index=where(level gt 0.)
;   contour,uzm,alat,zzm,levels=level(index),/follow,color=0,/overplot,/noeras
;   index=where(level lt 0.)
;   contour,uzm,alat,zzm,levels=level(index),/follow,color=mcolor,/overplot,/noeras
    contour,uzm,alat,zzm,levels=[0],/follow,color=0,/overplot,/noeras,thick=3

;   contour,markzm,alat,zzm,levels=0.1+0.1*findgen(10),/follow,color=0,/noeras,/overplot,thick=4
;   contour,markzm,alat,zzm,levels=-1+0.1*findgen(10),/follow,color=mcolor,/noeras,/overplot,thick=4
    contour,markmax,alat,zzm,levels=0.1+0.1*findgen(10),/follow,color=0,/noeras,/overplot,thick=6
    contour,markmin,alat,zzm,levels=-1+0.1*findgen(10),/follow,color=mcolor,/noeras,/overplot,thick=4
    contour,tzm,alat,zzm,levels=100+10*findgen(6),/follow,color=0.1*mcolor,/noeras,/overplot,thick=4
;
; superimpose MLS zonal winds
;
    dum=findfile('/atmos/aura6/data/MLS_data/Datfiles_Grid/MLS_grid5_ALL_U_V_v4.2_'+sdate+'.sav')
    if dum(0) eq '' then goto,skipmls
    restore,dum
    udum=mean(u,dim=1,/Nan)	; average over longitude
    udum=mean(udum,dim=3,/Nan)	; average over 2 nodes
    zdum=mean(gph,dim=1,/Nan)/1000. ; average over longitude
    zdum=mean(zdum,dim=3,/Nan)      ; average over 2 nodes
    contour,udum,lat,zdum,levels=[0],/follow,color=0,/overplot,/noeras,thick=3
    index=where(level gt 0.)
    contour,udum,lat,zdum,levels=level(index),/follow,color=0,/overplot,/noeras
    index=where(level lt 0.)
    contour,udum,lat,zdum,levels=level(index),/follow,color=mcolor,/overplot,/noeras

skipmls:
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
       spawn,'convert -trim ZM_Plots/yz_sdwaccm_U+Mark_new_'+sdate+'.ps -rotate -90 ZM_Plots/yz_sdwaccm_U+Mark_new_'+sdate+'.png'
    endif

jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
