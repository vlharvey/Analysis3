;
; read WACCM IDL save files, compute PV and QDF, interpolate to theta surfaces, and save as netcdf
; restore,'/Volumes/cloud/data/WACCM_data/Datfiles_SD/sdwaccm2012-2014_1_2_2.cam.h1.utls.20140101.sav
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
device,decompose=0
mcolor=byte(!p.color)
nlvls=30L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;thlev=[30000., 20000., 18000., 16000., 14000., 12000., 10000., 9000., 8000., 7000., 6000.,$ 
;        5000., 4500., 4000., 3500., 3000., 2800., 2600., 2400., 2200., 2000., 1900., 1800.,$
;        1700., 1600., 1500., 1400., 1300., 1200., 1100., 1000., 900., 800., 700., 650., 600.,$
;         550., 500., 475., 450., 425., 400.]
thlev=[450.,440.,430.,420.,410.,400.,390.,380.,370.,360.,350.,340.,330.]
nth=n_elements(thlev)
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
ifiles=file_search(dirw+'sdwaccm2012-2014_1_2_2.cam.h1.utls.2013*.sav',count=nfile)
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
    tl=t
    zl=z
    vl=v
    ul=u
    ql=qsum
;    h2o,n2o,o3
;
; calculate theta, absolute vorticity, PV, QDF
;
thl=0.*tl
for L=0L,NL-1L do $
    THL(*,*,L)=TL(*,*,L)*(1000./lev(L))^.286
eta=0.*tl
compvort,ul,vl,eta,alon,alat,nc,nr
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
h2oGRD=fltarr(nr,nc,nth)
n2oGRD=fltarr(nr,nc,nth)
o3GRD=fltarr(nr,nc,nth)
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
               PV(I,LAT,K)=-9.8*DTHDP*PV(I,LAT,K)/100.	; multiply by 1.e6 for PVU
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
      QGRD(LAT,I,M)=0.
      ZGRD(LAT,I,M)=0.
      h2oGRD(LAT,I,M)=0.
      n2oGRD(LAT,I,M)=0.
      o3GRD(LAT,I,M)=0.

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
      QGRD(LAT,I,M)=QL(I,LAT,K)+SCALE*(QL(I,LAT,LM1)-QL(I,LAT,K))
      ZGRD(LAT,I,M)=ZL(I,LAT,K)+SCALE*(ZL(I,LAT,LM1)-ZL(I,LAT,K))

      h2oGRD(LAT,I,M)=h2o(I,LAT,K)+SCALE*(h2o(I,LAT,LM1)-h2o(I,LAT,K))
      n2oGRD(LAT,I,M)=n2o(I,LAT,K)+SCALE*(n2o(I,LAT,LM1)-n2o(I,LAT,K))
      o3GRD(LAT,I,M)=o3(I,LAT,K)+SCALE*(o3(I,LAT,LM1)-o3(I,LAT,K))
      ENDIF
;
; add "surface" logic - assign points to lowest level if thlev is less than minimum at that gridpoint
;
      if thlev(m) lt min(THL(I,LAT,*)) then begin
         UGRD(LAT,I,m)=UL(I,LAT,nl-1)
         VGRD(LAT,I,m)=VL(I,LAT,nl-1)
         IPVGRD(LAT,I,m)=PV(I,LAT,nl-1)
         PGRD(LAT,I,m)=lev(nl-1)
         QDFGRD(LAT,I,M)=QDF(I,LAT,nl-1)
         QGRD(LAT,I,M)=QL(I,LAT,nl-1)
         ZGRD(LAT,I,M)=ZL(I,LAT,nl-1)

         h2oGRD(LAT,I,M)=h2o(I,LAT,nl-1)
         n2oGRD(LAT,I,M)=n2o(I,LAT,nl-1)
         o3GRD(LAT,I,M)=o3(I,LAT,nl-1)
;        print,'assigning lowest level to ',thlev(m)
      endif

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
;;
;; check
;; 
rlev=450.
;;print,thlev
;read,'Enter theta surface ',rlev
index=where(thlev eq rlev)
ilev=index(0)
slev=string(rlev)
pp=transpose(zgrd(*,*,ilev))
pp=transpose(ipvgrd(*,*,ilev))
level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
if max(pp) eq min(pp) then level=findgen(10)
!type=2^2+2^3
erase
contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
        yrange=[-90.,90.],title=sdate+'  '+slev+' K'
contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
pp=transpose(pgrd(*,*,ilev))
level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
if max(pp) eq min(pp) then level=findgen(10)
contour,pp,alon,alat,levels=level,/follow,c_color=mcolor,/noeras,/overplot
;
; write daily theta file
;
ofile=dirw+'sdwaccm2012-2014_1_2_2.cam.h1.'+sdate+'_utls.nc'
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
vid  = ncdf_vardef(nocid, 'H2O' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'N2O' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'O3' , [latdimid,londimid,levdimid])
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
ncdf_attput, nocid, 'H2O', 'longname', 'Water Vapor' & ncdf_attput, nocid, 'H2O', 'units', 'ppmv'
ncdf_attput, nocid, 'N2O', 'longname', 'Nitrous Oxide' & ncdf_attput, nocid, 'N2O', 'units', 'ppmv'
ncdf_attput, nocid, 'O3', 'longname', 'Ozone' & ncdf_attput, nocid, 'O3', 'units', 'ppmv'
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
ncdf_varput, nocid, 'H2O' , h2ogrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'N2O' , n2ogrd  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'O3' , o3grd  , COUNT=[nr,nc,nth]
ncdf_close,nocid
jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
