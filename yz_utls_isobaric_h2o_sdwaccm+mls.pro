;
; yz plots of SD-WACCM water and MLS water 
; 
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[88]
; LON             DOUBLE    = Array[144]
; H2O             FLOAT     = Array[144, 96, 88]
; N2O             FLOAT     = Array[144, 96, 88]
; O3              FLOAT     = Array[144, 96, 88]
; P               FLOAT     = Array[144, 96, 88]
; QSUM            FLOAT     = Array[144, 96, 88]
; T               FLOAT     = Array[144, 96, 88]
; U               FLOAT     = Array[144, 96, 88]
; V               FLOAT     = Array[144, 96, 88]
; Z               FLOAT     = Array[144, 96, 88]
; 
@compvort

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,1.5*cos(a),1.5*sin(a),/fill
nxdim=700
nydim=700
xorig=[0.25,0.25]
yorig=[.6,0.15]
xlen=0.5
ylen=0.3
cbaryoff=0.04
cbarydel=0.01
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif

nlvls=30L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
ifiles=file_search(dirw+'sdwaccm2012-2014_1_2_2.cam.h1.utls.201308*.sav',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    sdate=result(4)
    print,sdate
;
; look for nc3 file and skip if it already exists
;
    result=file_search(dirw+'sdwaccm2012-2014_1_2_2.cam.h1.'+sdate+'_utls.nc')
;   if result(0) ne '' then goto,jumpstep
;
; restore daily file
;
    restore,ifiles(n)
;
; rename variables
;
    alon=lon
    alat=lat
    nc=n_elements(alon)
    nr=n_elements(alat)
    nl=n_elements(lev)
    tl=t
    zl=z
    vl=v
    ul=u
    ql=qsum
;
; get level
;
;   if n eq 0L then begin
;      rlev=100
;      print,lev
;      read,'Enter desired pressure level ',rlev
;      index=where(min(abs(rlev-lev)) eq abs(rlev-lev))
;      ilev=index(0)
;      slev=strcompress(long(lev(ilev)),/r)+'hPa'
;   endif
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
         device,/landscape,bits=8,filename='yz_sdwaccm+mls_h2o_'+sdate+'_'+slev+'.ps'
         device,/color
         device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                xsize=xsize,ysize=ysize
         !p.thick=2.0                   ;Plotted lines twice as thick
      endif

    erase
    xyouts,.45,.95,sdate,/normal,color=0,charsize=2,charthick=2
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[88]
; LON             DOUBLE    = Array[144]
; H2O             FLOAT     = Array[144, 96, 88]
; N2O             FLOAT     = Array[144, 96, 88]
; O3              FLOAT     = Array[144, 96, 88]
; P               FLOAT     = Array[144, 96, 88]
; QSUM            FLOAT     = Array[144, 96, 88]
; T               FLOAT     = Array[144, 96, 88]
; U               FLOAT     = Array[144, 96, 88]
; V               FLOAT     = Array[144, 96, 88]
; Z               FLOAT     = Array[144, 96, 88]

    pp=mean(h2o,dim=1)*1.e6
    level=[1,2,3,4,5,6,7,8,9,10,20,50,100,200,500,1000,5000,10000,20000]	;min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
nlvls=n_elements(level)
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
    contour,pp,alat,lev,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[-90.,90.],$
        yrange=[300.,10.],/ylog,title='SDWACCM',ytitle='Pressure (hPa)',xtitle='Latitude',color=0
    contour,pp,alat,lev,levels=level,/follow,color=0,/overplot,/noeras
    pp2=mean(zl,dim=1)/1000.
;   contour,pp2,alat,lev,/ylog,levels=14+0.2*findgen(21),/follow,color=mcolor,/noeras,/overplot,thick=3
imin=min(level)
imax=max(level)
ymnb=yorig(0) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='SDWACCM H!l2!nO (ppmv)',/noeras
ybox=[0,10,10,0,0]
x1=imin
dxx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+dxx,x1+dxx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+dxx
endfor
;
; MLS
;
dum=findfile('/atmos/aura6/data/MLS_data/Datfiles_Grid/MLS_grid5_ALL_v3.3_'+sdate+'.sav')
if dum(0) eq '' then goto,skipmls
restore,dum
h2olev=mean(H2O_GRID,dim=1)*1.e6
gplev=mean(GP_GRID,dim=1)/1000.
!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,h2olev,lat,pmls2,/ylog,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[-90.,90.],$
        yrange=[300,10.],title='MLS',ytitle='Pressure (hPa)',xtitle='Latitude',color=0
contour,h2olev,lat,pmls2,levels=level,/follow,color=0,/overplot,/noeras
;contour,gplev,lon,pmls2,/ylog,levels=14+0.2*findgen(21),/follow,color=mcolor,/noeras,/overplot,thick=3
imin=min(level)
imax=max(level)
ymnb=yorig(1) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='MLS H!l2!nO (ppmv)',/noeras
ybox=[0,10,10,0,0]
x1=imin
dxx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+dxx,x1+dxx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+dxx
endfor

skipmls:
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim yz_sdwaccm+mls_h2o_'+sdate+'_'+slev+'.ps -rotate -90 yz_sdwaccm+mls_h2o_'+sdate+'_'+slev+'.jpg'
    endif

jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
