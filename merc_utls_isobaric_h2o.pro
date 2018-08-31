;
; mercator plots of SD-WACCM water
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
xorig=[0.15]
yorig=[.2]
xlen=0.8
ylen=0.6
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
    if n eq 0L then begin
       rlev=100
       print,lev
;      read,'Enter desired pressure level ',rlev
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
         device,/landscape,bits=8,filename='merc_sdwaccm_h2o_'+sdate+'_'+slev+'.ps'
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
    pp=reform(h2o(*,*,ilev))*1.e6
    level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
    contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
        yrange=[-90.,90.],title=sdate+'  '+strcompress(lev(ilev),/r)+' hPa',color=0
    pp2=reform(thl(*,*,ilev))
    contour,pp2,alon,alat,levels=reverse(thlev),/follow,color=mcolor,/noeras,/overplot,thick=3
    pp2=reform(zl(*,*,ilev))/1000.
    contour,pp2,alon,alat,levels=min(pp2)+((max(pp2)-min(pp2))/float(nlvls))*findgen(nlvls),/follow,color=.9*mcolor,/noeras,/overplot,thick=3
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
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim merc_sdwaccm_h2o_'+sdate+'_'+slev+'.ps -rotate -90 merc_sdwaccm_h2o_'+sdate+'_'+slev+'.jpg'
    endif

jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
