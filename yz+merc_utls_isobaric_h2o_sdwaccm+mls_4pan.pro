;
; merc and yz plots of SD-WACCM water and MLS water 
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
xorig=[0.15,0.55,0.15,0.55]
yorig=[0.6,0.6,0.15,0.15]
xlen=0.3
ylen=0.3
cbaryoff=0.06
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
ifiles=file_search(dirw+'sdwaccm2012-2014_1_2_2.cam.h1.utls.201306*.sav',count=nfile)
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
; calculate theta
;
thl=0.*tl
for L=0L,NL-1L do $
    THL(*,*,L)=TL(*,*,L)*(1000./lev(L))^.286
;
; get level
;
    if n eq 0L then begin
       rlev=100
       print,lev
       read,'Enter desired pressure level ',rlev
       index=where(min(abs(rlev-lev)) eq abs(rlev-lev))
       ilev=index(0)
       slev=strcompress(long(lev(ilev)),/r)+'hPa'
    endif
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
        yrange=[300.,50.],/ylog,title='SDWACCM',ytitle='Pressure (hPa)',xtitle='Latitude',color=0
    contour,pp,alat,lev,levels=level,/follow,color=0,/overplot,/noeras
    pp2=mean(tl,dim=1)
    contour,pp2,alat,lev,/ylog,levels=[180,190,200],/follow,color=0,/noeras,/overplot,thick=4
    pp2=mean(thl,dim=1)
    contour,pp2,alat,lev,/ylog,levels=[360,370,380],/follow,color=mcolor,/noeras,/overplot,thick=3
;
; MLS
;
dum=findfile('/atmos/aura6/data/MLS_data/Datfiles_Grid/MLS_grid5_ALL_v3.3_'+sdate+'.sav')
if dum(0) eq '' then goto,skipmls
restore,dum
;
; calculate theta
;
th_grid=0.*TP_GRID
for L=0L,N_elements(pmls2)-1L do $
    TH_grid(*,*,L)=TP_GRID(*,*,L)*(1000./pmls2(L))^.286
tlev=mean(tp_grid,dim=1)
thlev=mean(th_grid,dim=1)
h2olev=mean(H2O_GRID,dim=1)*1.e6
gplev=mean(GP_GRID,dim=1)/1000.
!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,h2olev,lat,pmls2,/ylog,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[-90.,90.],$
        yrange=[300,50.],title='MLS',ytitle='Pressure (hPa)',xtitle='Latitude',color=0
contour,h2olev,lat,pmls2,levels=level,/follow,color=0,/overplot,/noeras
contour,tlev,lat,pmls2,/ylog,levels=[180,190,200],/follow,color=0,/noeras,/overplot,thick=4
contour,thlev,lat,pmls2,/ylog,levels=[360,370,380],/follow,color=mcolor,/noeras,/overplot,thick=3
imin=min(level)
imax=max(level)
ymnb=yorig(1) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='H!l2!nO (ppmv)',/noeras,xtickname=strcompress(level,/r),xticks=nlvls-1
ybox=[0,10,10,0,0]
x1=imin
dxx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+dxx,x1+dxx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+dxx
endfor

    !type=2^2+2^3
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    pp=reform(h2o(*,*,ilev))*1.e6
;   level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
index=where(level le 20)
level=level(index)
nlvls=n_elements(level)
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
    contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
        yrange=[-90.,90.],title='SDWACCM at '+strcompress(lev(ilev),/r)+' hPa',color=0
    contour,pp,alon,alat,levels=level,/follow,color=0,/overplot
;   pp2=reform(thl(*,*,ilev))
;   contour,pp2,alon,alat,levels=[360,370,380,390,400],/follow,color=0.9*mcolor,/noeras,/overplot,thick=3
    pp2=reform(zl(*,*,ilev))/1000.
;   contour,pp2,alon,alat,levels=min(pp2)+((max(pp2)-min(pp2))/float(nlvls))*findgen(nlvls),/follow,color=.9*mcolor,/noeras,/overplot,thick=3
    contour,pp2,alon,alat,levels=15+0.2*findgen(21),/follow,color=mcolor,/noeras,/overplot,thick=3

index=where(min(abs(rlev-pmls2)) eq abs(rlev-pmls2))
h2olev=reform(H2O_GRID(*,*,index(0)))*1.e6
gplev=reform(GP_GRID(*,*,index(0)))/1000.
thlev=reform(th_grid(*,*,index(0)))
!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,h2olev,lon,lat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
        yrange=[-90.,90.],title='MLS at '+strcompress(pmls2(index(0)),/r)+' hPa',color=0
contour,h2olev,lon,lat,levels=level,/follow,color=0,/overplot
;contour,thlev,lon,lat,levels=[360,370,380,390,400],/follow,color=0.9*mcolor,/noeras,/overplot,thick=3
contour,gplev,lon,lat,levels=15+0.2*findgen(21),/follow,color=mcolor,/noeras,/overplot,thick=3
imin=min(level)
imax=max(level)
ymnb=yorig(2) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xorig(2),xorig(3)+xlen,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='H!l2!nO (ppmv)',/noeras
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
