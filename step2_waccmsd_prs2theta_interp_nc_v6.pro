;
; read WACCM IDL save files, compute PV and QDF, interpolate to theta surfaces, and save as netcdf
; restore,'/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_v6/c_cesm2_fswd_2005_cntrl.cam.h1.20050101.sav
;
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[88]
; LON             DOUBLE    = Array[144]
; COGRD           FLOAT     = Array[144, 96, 88]
; NO2GRD          FLOAT     = Array[144, 96, 88]
; NOGRD           FLOAT     = Array[144, 96, 88]
; O3GRD           FLOAT     = Array[144, 96, 88]
; PGRD            FLOAT     = Array[144, 96, 88]
; TGRD            FLOAT     = Array[144, 96, 88]
; UGRD            FLOAT     = Array[144, 96, 88]
; VGRD            FLOAT     = Array[144, 96, 88]
; ZGRD            FLOAT     = Array[144, 96, 88]
;
; note, don't carry chemistry along. Isobaric chemistry data is fine, don't need isentroic.
;
@compvort

loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
!p.background=icolmax
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.10]
yorig=[0.25]
xlen=0.8
ylen=0.6
cbaryoff=0.1
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

thlev=[100000.,80000.,60000.,40000.,30000., 20000., 18000., 16000., 14000., 12000., 10000.,$
         9000., 8000., 7000., 6000., 5000., 4500., 4000., 3500., 3000., 2800., 2600., 2400.,$
         2200., 2000., 1900., 1800., 1700., 1600., 1500., 1400., 1300., 1200., 1100., 1000.,$
          900.,800.,700.,650.,600.,550.,500.,475.,450.,425.,400.,390.,380.,370.,360.,350.]
nth=n_elements(thlev)
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_v6/'
ifiles=file_search(dirw+'c_cesm2_fswd_2005_cntrl.cam.h1.20??????.sav',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    sdate=result(-2)
    print,sdate
;
; look for nc3 file and skip if it already exists
;
    result=file_search(dirw+'c_cesm2_fswd_2005_cntrl.cam.h1.'+sdate+'.nc3')
    if result(0) ne '' then goto,jumpstep
    result=file_search(dirw+'c_cesm2_fswd_2005_cntrl.cam.h1.'+sdate+'.nc')
    if result(0) ne '' then goto,jumpstep
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
    tl=tgrd
    zl=zgrd
    vl=vgrd
    ul=ugrd
    pl=pgrd
;   ql=0.*ul
;
; calculate theta, absolute vorticity, PV, QDF
;
thl=0.*tl
for L=0L,NL-1L do $
    THL(*,*,L)=TL(*,*,L)*(1000./pl(*,*,L))^.286
eta=0.*tl
compvort,ul,vl,eta,alon,alat,nc,nr
;
; initialize isentropic arrays
;
UGRD=fltarr(nr,nc,nth)
VGRD=fltarr(nr,nc,nth)
IPVGRD=fltarr(nr,nc,nth)
PGRD=fltarr(nr,nc,nth)
;QGRD=fltarr(nr,nc,nth)
ZGRD=fltarr(nr,nc,nth)
QDFGRD=fltarr(nr,nc,nth)
;
; LOOP OVER LATITUDES
;
pv=0.*tl
qdf=0.*tl
for LAT=0L,NR-1L do begin
    JP1=LAT-1
    JM1=LAT+1
    JP2=LAT-2
    JM2=LAT+2
    IF LAT EQ 0 THEN begin
       JP1=0
       JP2=0
    ENDIF
    IF LAT EQ NR-1L THEN begin
       JM1=NR-1L
       JM2=NR-1L
    ENDIF
    IF LAT EQ NR-2 then JM2=NR-1
    IF LAT EQ 1 then JP2=0
    DY1=RADEA*(ALAT(JP1)-ALAT(JM1))*DTR
    DY2=RADEA*(ALAT(JP2)-ALAT(JM2))*DTR
    DX1=RADEA*COS(ALAT(LAT)*DTR)*PI2/(.5*NC)
    DX2=RADEA*COS(ALAT(LAT)*DTR)*PI2/(.25*NC)
;
; LOOP OVER LONGITUDES
;
    for I=0,NC-1L do begin
        IP1=I+1
        IM1=I-1
        IP2=I+2
        IM2=I-2
        IF I EQ 0 THEN begin
           IM1=NC-1
           IM2=NC-2
        ENDIF
        IF I EQ NC-1 THEN begin
           IP1=0
           IP2=1
        ENDIF
        IF I EQ 1 then IM2=NC-1
        IF I EQ NC-2 then IP2=0
;
; COMPUTE ISENTROPIC POTENTIAL VORTICITY ON PRESSURE SURFACE
;
        for K=0,NL-1L do begin
            LM1=K-1
            LP1=K+1
            IF K EQ 0 then LM1=0
            IF K EQ NL-1L then LP1=NL-1L
            DTHDP=(THL(I,LAT,LP1)-THL(I,LAT,LM1))/(lev(LP1)-lev(LM1))
            DUDP=(ul(I,LAT,LP1)-ul(I,LAT,LM1))/(lev(LP1)-lev(LM1))
            DVDP=(vl(I,LAT,LP1)-vl(I,LAT,LM1))/(lev(LP1)-lev(LM1))
            DTHDX=(4./3.)*(THL(IP1,LAT,K)-THL(IM1,LAT,K))/DX1 - $
                  (1./3.)*(THL(IP2,LAT,K)-THL(IM2,LAT,K))/DX2
            IF LAT LE 1 OR LAT GE NR-2 THEN begin
               DTHDY=(THL(I,JP1,K)-THL(I,JM1,K))/DY1
            endif
            IF LAT gt 1 and LAT lt NR-2 THEN begin
               DTHDY=(4./3.)*(THL(I,JP1,K)-THL(I,JM1,K))/DY1 - $
                     (1./3.)*(THL(I,JP2,K)-THL(I,JM2,K))/DY2
            ENDIF
            IF DTHDP GE 0. THEN begin
               PV(I,LAT,K)=1.E12
            endif
            IF DTHDP lt 0. THEN begin
               PV(I,LAT,K)=eta(I,LAT,K)-(DUDP*DTHDY-DVDP*DTHDX)/DTHDP
               PV(I,LAT,K)=-9.8*DTHDP*PV(I,LAT,K)*10000.	; divide by 100 for Pascals and multiply by 1.e6 (x1.e4) for PVU
            ENDIF

; normalized by RADEA. The signed sqrt of Q is taken.
            arg1 = (ul(IP1,LAT,K)-ul(IM1,LAT,K))/DX1 $
                  - vl(I,LAT,K)*TAN(ALAT(LAT)*DTR)/RADEA
            arg2 = (vl(IP1,LAT,K)-vl(IM1,LAT,K))/DX1 $
                  + ul(I,LAT,K)*TAN(ALAT(LAT)*DTR)/RADEA
            DVDY = (vl(I,JP1,K)-vl(I,JM1,K))/DY1
            DUDY = (ul(I,JP1,K)-ul(I,JM1,K))/DY1
            if abs(arg1) gt 1.e12 or abs(arg2) gt 1.e12 then QDF(I,LAT,K) = 1.e12
            if abs(arg1) lt 1.e12 and abs(arg2) lt 1.e12 then begin
               qtemp=(0.5*(arg1*arg1+DVDY*DVDY)+arg2*DUDY)*RADEA*RADEA
               if qtemp ge 0.0 then QDF(I,LAT,K) = sqrt(qtemp)
               if qtemp lt 0.0 then QDF(I,LAT,K) = -sqrt(-qtemp)
            endif
; end calculation of QDF deformation diagnostic
        endfor

; LINEARLY INTERPOLATE IN THETA TO ISENTROPIC LEVEL
      for m=0,nth-1L do begin
      UGRD(LAT,I,M)=0.
      VGRD(LAT,I,M)=0.
      IPVGRD(LAT,I,M)=1.E12
      PGRD(LAT,I,M)=0.
      QDFGRD(LAT,I,M)=0.
;     QGRD(LAT,I,M)=0.
      ZGRD(LAT,I,M)=0.

      for K=1,NL-1L do begin
      LM1 = K-1
      LP1 = K+1
      IF K EQ NL-1L then LP1 = NL-1L

      IF THLEV(M) LE THL(I,LAT,LM1) AND THLEV(M) GT THL(I,LAT,K) THEN begin
      PLM1=lev(LM1)
      PL=lev(K)
      SCALE=(THLEV(M)-THL(I,LAT,K))/(THL(I,LAT,LM1)-THL(I,LAT,K))
      UGRD(LAT,I,m)=UL(I,LAT,K)+SCALE*(UL(I,LAT,LM1)-UL(I,LAT,K))
      VGRD(LAT,I,m)=VL(I,LAT,K)+SCALE*(VL(I,LAT,LM1)-VL(I,LAT,K))
      IPVGRD(LAT,I,m)=PV(I,LAT,K)+SCALE*(PV(I,LAT,LM1)-PV(I,LAT,K))
      PGRD(LAT,I,m)=PL^.286 + SCALE*(PLM1^.286-PL^.286)
      PGRD(LAT,I,m)=PGRD(LAT,I,m)^(1./.286)
      QDFGRD(LAT,I,M)=QDF(I,LAT,K)+SCALE*(QDF(I,LAT,LM1)-QDF(I,LAT,K))
;     QGRD(LAT,I,M)=QL(I,LAT,K)+SCALE*(QL(I,LAT,LM1)-QL(I,LAT,K))
      ZGRD(LAT,I,M)=ZL(I,LAT,K)+SCALE*(ZL(I,LAT,LM1)-ZL(I,LAT,K))
      ENDIF

      endfor
      endfor
    endfor
endfor

; average QDF for polar rows
for M = 0, NTH-1 do begin
    xlst=0.0
    frst=0.0
    for I = 0, nc-1 do begin
       xlst = xlst + QDFGRD(nr-2,i,m)/float(nc)
       frst = frst + QDFGRD(1,i,m)/float(nc)
    endfor
    for I = 0, nc-1 do begin
       QDFGRD(nr-1,i,m) = xlst
       QDFGRD(0,i,m) = frst
    endfor
endfor
;
; check
; 
;rlev=2000.
;print,thlev
;read,'Enter theta surface ',rlev
;index=where(thlev eq rlev)
;ilev=index(0)
;slev=string(rlev)
;pp=transpose(ipvgrd(*,*,ilev))
;level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
;if max(pp) eq min(pp) then level=findgen(10)
;!type=2^2+2^3
;erase
;contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
;        yrange=[-90.,90.],title=sdate+'  '+slev+' K'
;contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
;contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
;stop

if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='ZM_Plots/yz_sdwaccm_v6_ubar_'+sdate+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
zpp=mean(zgrd,dim=2)/1000.
upp=mean(ugrd,dim=2)
imin=-100.
imax=100.
nlvls=20L
level=imin+((imax-imin)/float(nlvls))*findgen(nlvls+1)
col1=1+(indgen(nlvls+1)/float(nlvls+1))*mcolor
contour,upp,alat,zpp,levels=level,/cell,c_color=col1,/noeras,ytitle='Altitude (km)',xtitle='Latitude',xrange=[-90,90],title=sdate,charsize=2,charthick=2,color=0
index=where(level gt 0.)
contour,upp,alat,zpp,levels=level(index),/foll,color=0,/noeras,/overplot
index=where(level lt 0.)
contour,upp,alat,zpp,levels=level(index),/foll,color=mcolor,c_linestyle=5,/noeras,/overplot
contour,upp,alat,zpp,levels=[0.],/foll,color=0,thick=5,/noeras,/overplot

imin=min(level)
imax=max(level)
ymnb=yorig(0) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='SD-WACCM v6 Ubar (m/s)',charsize=2,charthick=2
ybox=[0,10,10,0,0]
x1=imin
dx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+dx,x1+dx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+dx
endfor

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim ZM_Plots/yz_sdwaccm_v6_ubar_'+sdate+'.ps -rotate -90 ZM_Plots/yz_sdwaccm_v6_ubar_'+sdate+'.png'
   spawn,'rm -f ZM_Plots/yz_sdwaccm_v6_ubar_'+sdate+'.ps'
endif
;
; write daily theta file
;
ofile=dirw+'c_cesm2_fswd_2005_cntrl.cam.h1.'+sdate+'.nc'
print,'writing ',ofile
nocid = ncdf_create(ofile,/CLOBBER)
latdimid=ncdf_dimdef(nocid, 'number_of_latitudes' , nr)
londimid=ncdf_dimdef(nocid, 'number_of_longitudes', nc)
levdimid=ncdf_dimdef(nocid, 'number_of_levels'    , nth)
lonsid = ncdf_vardef(nocid, 'longitude',  londimid)
latsid = ncdf_vardef(nocid, 'latitude' ,  latdimid)
levsid = ncdf_vardef(nocid, 'theta'    ,  levdimid)
vid  = ncdf_vardef(nocid, 'IPV' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'P'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'U'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'V'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'QDF' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'GPH' , [latdimid,londimid,levdimid])

ncdf_attput, nocid, 'longitude', 'longname', 'longitude' & ncdf_attput, nocid, 'longitude', 'units', 'deg E'
ncdf_attput, nocid, 'latitude', 'longname', 'latitude' & ncdf_attput, nocid, 'latitude', 'units', 'deg'
ncdf_attput, nocid, 'theta', 'longname', 'potential temperature' & ncdf_attput, nocid, 'theta', 'units', 'K'
ncdf_attput, nocid, 'IPV', 'longname', 'Isentropic Potential Vorticity' & ncdf_attput, nocid, 'IPV', 'units', 'K m^2 /s /kg'
ncdf_attput, nocid, 'P', 'longname', 'Pressure' & ncdf_attput, nocid, 'P', 'units', 'hPa'
ncdf_attput, nocid, 'U', 'longname', 'Zonal Wind' & ncdf_attput, nocid, 'U', 'units', 'm/s'
ncdf_attput, nocid, 'V', 'longname', 'Meridional Wind' & ncdf_attput, nocid, 'V', 'units', 'm/s'
ncdf_attput, nocid, 'QDF', 'longname', 'Strain/Rotation Parameter' & ncdf_attput, nocid, 'QDF', 'units', 's-1'
ncdf_attput, nocid, 'GPH', 'longname', 'Geopotential Height' & ncdf_attput, nocid, 'GPH', 'units', 'm'

ncdf_control,nocid,/ENDEF
ncdf_varput, nocid, 'longitude', alon  , COUNT=[nc]
ncdf_varput, nocid, 'latitude' , alat  , COUNT=[nr]
ncdf_varput, nocid, 'theta'    , thlev , COUNT=[nth]
ncdf_varput, nocid, 'IPV' , ipvgrd, COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'P'   , pgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'U'   , ugrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'V'   , vgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'QDF' , qdfgrd, COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'GPH' , zgrd  , COUNT=[nr,nc,nth]
ncdf_close,nocid
jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
