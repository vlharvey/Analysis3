;
; read WACCM4 IDL save files, compute PV and QDF, interpolate to theta surfaces, and save as netcdf
; Input data:  IDL>restore,'/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2.cam2.h3.dyns.20421231_3D_dyn.sav
; ILEV            DOUBLE    = Array[67]
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[66]
; LON             DOUBLE    = Array[144]
; OMEGA3D         FLOAT     = Array[144, 96, 66]
; P3D             FLOAT     = Array[144, 96, 66]
; PS2D            FLOAT     = Array[144, 96]
; QRLTOT3D        FLOAT     = Array[144, 96, 66]
; QRSTOT3D        FLOAT     = Array[144, 96, 66]
; T3D             FLOAT     = Array[144, 96, 66]
; TH3D            FLOAT     = Array[144, 96, 67]
; TS2D            FLOAT     = Array[144, 96]
; TTGW3D          FLOAT     = Array[144, 96, 66]
; U3D             FLOAT     = Array[144, 96, 66]
; UV3D3D          FLOAT     = Array[144, 96, 67]
; UW3D3D          FLOAT     = Array[144, 96, 67]
; V3D             FLOAT     = Array[144, 96, 66]
; VTH3D3D         FLOAT     = Array[144, 96, 67]
; Z3D             FLOAT     = Array[144, 96, 66]
; 
@compvort

;loadct,39
;device,decompose=0
;mcolor=byte(!p.color)
;nlvls=30L
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
thlev=[30000., 20000., 18000., 16000., 14000., 12000., 10000., 9000., 8000., 7000., 6000.,$ 
        5000., 4500., 4000., 3500., 3000., 2800., 2600., 2400., 2200., 2000., 1900., 1800.,$
        1700., 1600., 1500., 1400., 1300., 1200., 1100., 1000., 900., 800., 700., 650., 600.,$
         550., 500., 475., 450., 425., 400., 375., 350., 325., 300.]
nth=n_elements(thlev)
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
ifiles=file_search(dirw+'mee00fpl_FW2.cam2.h3.dyns.*_3D_dyn.sav',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    result2=strsplit(result(4),'_',/extract)
    sdate=result2(0)
    print,sdate
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
    tl=t3d
    zl=z3d
    vl=v3d
    ul=u3d
    prl=p3d
    ql=(qrltot3d+qrstot3d)*86400.	; K/day
    ttgw=ttgw3d
    t3d=0 & z3d=0 & v3d=0 & u3d=0 & p3d=0 & qrltot3d=0 & qrstot3d=0 & ttgw3d=0 & omega3d=0 & th3d=0 & uv3d3d=0 & uw3d3d=0 & vth3d3d=0
;
; calculate theta, absolute vorticity, PV, QDF
;
thl=0.*tl
for L=0L,NL-1L do $
    THL(*,*,L)=TL(*,*,L)*(1000./PRL(*,*,L))^.286
eta=0.*tl
compvort,ul,vl,eta,alon,alat,nc,nr
;
; initialize surface arrays (arrays are top down)
;
tsfc=ts2d
psfc=ps2d
thsfc=reform(thl(*,*,nl-1L))
usfc=reform(ul(*,*,nl-1L))
vsfc=reform(vl(*,*,nl-1L))
zsfc=reform(zl(*,*,nl-1L))
qsfc=reform(ql(*,*,nl-1L))
ttgwsfc=reform(ttgw(*,*,nl-1L))
qdfsfc=0.*tsfc
pvsfc=0.*tsfc
;
; initialize isentropic arrays
;
UGRD=fltarr(nr,nc,nth)
VGRD=fltarr(nr,nc,nth)
IPVGRD=fltarr(nr,nc,nth)
PGRD=fltarr(nr,nc,nth)
QGRD=fltarr(nr,nc,nth)
ZGRD=fltarr(nr,nc,nth)
QDFGRD=fltarr(nr,nc,nth)
TTGWGRD=fltarr(nr,nc,nth)
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
            DTHDP=(THL(I,LAT,LP1)-THL(I,LAT,LM1))/(PRL(I,LAT,LP1)-PRL(I,LAT,LM1))
            DUDP=(ul(I,LAT,LP1)-ul(I,LAT,LM1))/(PRL(I,LAT,LP1)-PRL(I,LAT,LM1))
            DVDP=(vl(I,LAT,LP1)-vl(I,LAT,LM1))/(PRL(I,LAT,LP1)-PRL(I,LAT,LM1))
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
               PV(I,LAT,K)=-9.8*DTHDP*PV(I,LAT,K)*10000.	; multiply by 1.e6 for PVU
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
        qdfsfc(i,lat)=qdf(i,lat,nl-1)
        pvsfc(i,lat)=pv(i,lat,nl-1)

; LINEARLY INTERPOLATE IN THETA TO ISENTROPIC LEVEL
      for m=0,nth-1L do begin
      UGRD(LAT,I,M)=0.
      VGRD(LAT,I,M)=0.
      IPVGRD(LAT,I,M)=1.E12
      PGRD(LAT,I,M)=0.
      QDFGRD(LAT,I,M)=0.
      QGRD(LAT,I,M)=0.
      ZGRD(LAT,I,M)=0.
      TTGWGRD(LAT,I,M)=0.

      for K=1,NL-1L do begin
      LM1 = K-1
      LP1 = K+1
      IF K EQ NL-1L then LP1 = NL-1L

      IF THLEV(M) LE THL(I,LAT,LM1) AND THLEV(M) GT THL(I,LAT,K) THEN begin
      PLM1=PRL(I,LAT,LM1)
      PL=PRL(I,LAT,K)
      SCALE=(THLEV(M)-THL(I,LAT,K))/(THL(I,LAT,LM1)-THL(I,LAT,K))
      UGRD(LAT,I,m)=UL(I,LAT,K)+SCALE*(UL(I,LAT,LM1)-UL(I,LAT,K))
      VGRD(LAT,I,m)=VL(I,LAT,K)+SCALE*(VL(I,LAT,LM1)-VL(I,LAT,K))
      IPVGRD(LAT,I,m)=PV(I,LAT,K)+SCALE*(PV(I,LAT,LM1)-PV(I,LAT,K))
      PGRD(LAT,I,m)=PL^.286 + SCALE*(PLM1^.286-PL^.286)
      PGRD(LAT,I,m)=PGRD(LAT,I,m)^(1./.286)
      QDFGRD(LAT,I,M)=QDF(I,LAT,K)+SCALE*(QDF(I,LAT,LM1)-QDF(I,LAT,K))
      QGRD(LAT,I,M)=QL(I,LAT,K)+SCALE*(QL(I,LAT,LM1)-QL(I,LAT,K))
      ZGRD(LAT,I,M)=ZL(I,LAT,K)+SCALE*(ZL(I,LAT,LM1)-ZL(I,LAT,K))
      TTGWGRD(LAT,I,M)=TTGW(I,LAT,K)+SCALE*(TTGW(I,LAT,LM1)-TTGW(I,LAT,K))
      ENDIF

      endfor
; if desired theta surface intersects the ground then
; isentropic quantities are equal to the sfc values (at psfc)
      if thlev(m) le thsfc(i,lat) then begin
      PGRD(LAT,I,M)=psfc(i,lat)
      UGRD(LAT,I,M)=usfc(i,lat)
      VGRD(LAT,I,M)=vsfc(i,lat)
      IPVGRD(LAT,I,M)=pvsfc(i,lat)
      QDFGRD(LAT,I,M)=qdfsfc(i,lat)
      QGRD(LAT,I,M)=qsfc(i,lat)
      ZGRD(LAT,I,M)=zsfc(i,lat)
      TTGWGRD(LAT,I,M)=ttgwsfc(i,lat)
      endif

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
;;
;; check
;; 
;rlev=2000.
;;print,thlev
;;read,'Enter theta surface ',rlev
;index=where(thlev eq rlev)
;ilev=index(0)
;slev=string(rlev)
;pp=transpose(qdfgrd(*,*,ilev))
;level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
;!type=2^2+2^3
;erase
;contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
;        yrange=[-90.,90.],title=sdate+'  '+slev+' K'
;contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
;contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
;
; write daily theta file
;
ofile=dirw+'mee00fpl_FW2.cam2.h3.dyns.'+sdate+'_3D_dyn.nc'
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
vid  = ncdf_vardef(nocid, 'Q'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'GPH' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'TTGW', [latdimid,londimid,levdimid])
ncdf_attput, nocid, 'longitude', 'longname', 'longitude' & ncdf_attput, nocid, 'longitude', 'units', 'deg E'
ncdf_attput, nocid, 'latitude', 'longname', 'latitude' & ncdf_attput, nocid, 'latitude', 'units', 'deg'
ncdf_attput, nocid, 'theta', 'longname', 'potential temperature' & ncdf_attput, nocid, 'theta', 'units', 'K'
ncdf_attput, nocid, 'IPV', 'longname', 'Isentropic Potential Vorticity' & ncdf_attput, nocid, 'IPV', 'units', 'K m^2 /s /kg'
ncdf_attput, nocid, 'P', 'longname', 'Pressure' & ncdf_attput, nocid, 'P', 'units', 'hPa'
ncdf_attput, nocid, 'U', 'longname', 'Zonal Wind' & ncdf_attput, nocid, 'U', 'units', 'm/s'
ncdf_attput, nocid, 'V', 'longname', 'Meridional Wind' & ncdf_attput, nocid, 'V', 'units', 'm/s'
ncdf_attput, nocid, 'QDF', 'longname', 'Strain/Rotation Parameter' & ncdf_attput, nocid, 'QDF', 'units', 's-1'
ncdf_attput, nocid, 'Q', 'longname', 'Net Diabatic Heating Rate' & ncdf_attput, nocid, 'Q', 'units', 'K/day'
ncdf_attput, nocid, 'GPH', 'longname', 'Geopotential Height' & ncdf_attput, nocid, 'GPH', 'units', 'm'
ncdf_attput, nocid, 'TTGW', 'longname', 'Temperature Tendency due to Gravity Waves' & ncdf_attput, nocid, 'TTGW', 'units', 'K/day'
ncdf_control,nocid,/ENDEF
ncdf_varput, nocid, 'longitude', alon  , COUNT=[nc]
ncdf_varput, nocid, 'latitude' , alat  , COUNT=[nr]
ncdf_varput, nocid, 'theta'    , thlev , COUNT=[nth]
ncdf_varput, nocid, 'IPV' , ipvgrd, COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'P'   , pgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'U'   , ugrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'V'   , vgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'QDF' , qdfgrd, COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'Q'   , qgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'GPH' , zgrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'TTGW', ttgwgrd  , COUNT=[nr,nc,nth]
ncdf_close,nocid
ENDFOR		; LOOP OVER TIMESTEPS
end
