;mkavg_jan_ice_column_mls.pro -- Make avg Jan ice mass column using Mark's 0D formula.

;Note that Mark's formula gives the ice mass density (e.g., mass/volume). I want
;  the column, so I must integrate in height. 
;The gridded MLS data are on pressure levels, so I need to calculate dz.
;To do this I can convert the geopotential height to geometric height, and do the
;   integration at each point explicitly (i.e., calculate delta-z for each point, since
;   I'll have the geometric height for each point).
;   z =  h * (R_earth) / ((R_earth) - h), z=geometric ht, h=geopotential ht.
;The earth's radius is given by (http://en.wikipedia.org/wiki/Earth_radius):
; R(lat)=sqrt(num/den) where:
;        num = [a^2 cos(lat)]^2 + [b^2 sin(lat)]^2
;	    den = [a cos(lat)]^2 + (b sin(lat))^2
;	    a = 6,378.1370 km    b = 6,356.7523 km 

ptht='C:\data\mls\level3\temperature\'
pthh='C:\data\mls\level3\h2o\'
pthz='C:\data\mls\level3\gph\'

ndays=31
days=strcompress(indgen(ndays)+1,/remove_all)
x=where(days lt 10)
days(x)='00'+days(x)
x=where(days gt 9 and days lt 100)
days(x)='0'+days(x)

pret='MLS_3.3_T_grid5_'
preh='MLS_3.3_H2O_grid5_'
prez='MLS_3.3_GPH_grid5_'

for year=2008,2012 do begin
   yyyy=strcompress(year,/remove_all)
   for iDAY=0,ndays-1 do begin
      file=pret+yyyy+'d'+days(iDAY)+'.nc'   
      ncid = ncdf_open(pthT+file)
      ncdf_varget,ncid,ncdf_varid(ncid,'mls_t_grid'),mls_t_grid

      IF IDAY EQ 0 THEN BEGIN
         ncdf_varget,ncid,ncdf_varid(ncid,'lon'),lon
         ncdf_varget,ncid,ncdf_varid(ncid,'lat'),lat
         ncdf_varget,ncid,ncdf_varid(ncid,'lev'),lev
         NLON=N_ELEMENTS(LON) & NLAT=N_ELEMENTS(LAT) & NLEV=N_ELEMENTS(LEV)
         ICE=FLTARR(NLON,NLAT,NDAYS)-99	;WILL HOLD FINAL INTEGRATED ICE COLUMN MASS
      ENDIF

      ncdf_close,ncid

      file=preH+yyyy+'d'+days(iDAY)+'.nc'   
      ncid = ncdf_open(pthH+file)
      ncdf_varget,ncid,ncdf_varid(ncid,'mls_h2o_grid'),mls_h2o_grid
      ncdf_close,ncid    

      file=prez+yyyy+'d'+days(iDAY)+'.nc'   
      ncid = ncdf_open(pthZ+file)
      ncdf_varget,ncid,ncdf_varid(ncid,'mls_gph_grid'),GPH
      ncdf_close,ncid 
      GPH=GPH/1000.	;CONVERT FROM METERS TO KM   

      ;CALCULATE GEOMETRIC ALTITUDE (z=h*(R_earth)/((R_earth)- h) 
      ;ALT=GPH*Re/(Re-GPH)
	 ; Re(lat)=sqrt(num/den) where:
	 ;        num = [a^2 cos(lat)]^2 + [b^2 sin(lat)]^2
	 ;	    den = [a cos(lat)]^2 + (b sin(lat))^2
	 ;	    a = 6,378.1370 km    b = 6,356.7523 km 
	 a = 6378.1370 & b = 6356.7523
	 num = [a^2 * cos(lat*!PI/180.)]^2 + [b^2 * sin(lat*!PI/180.)]^2
	 den = [a * cos(lat*!PI/180.)]^2 + (b * sin(lat*!PI/180.))^2
	 Re = SQRT(num/den)
	 ALT = GPH
	 ALT(*)= -99.
	 FOR ILAT=0,N_ELEMENTS(GPH(0,*,0))-1 DO BEGIN
		ALT(*,ILAT,*) = GPH(*,ILAT,*) * RE(ILAT) / (RE(ILAT) - GPH(*,ILAT,*))
	 ENDFOR
	 BAD=WHERE(FINITE(GPH) EQ 0,NBAD)
	 IF NBAD GT 0 THEN ALT(BAD)=-99.
	 ;stop

      ;CALCULATE M_ICE AT EACH PRESSURE LEVEL USING MARK'S FORMULA
      ;M_ICE = ice mass density = [(P_H2O - P_SAT)*100 / (T * 8.314)] * 18*1.e6
      ; P_H2O IS PARTIAL PRESSURE OF H2O (MIXING RATIO * TOTAL PRESSURE), hPa
      ; P_SAT IS SATURATION VAPOR PRESSURE (SEE BELOW; POSSIBLY FROM RAPP & THOMAS, 2006)
      ; 100 IS TO CONVERT FROM hPa TO Pa (1 hPa = 100 Pa)
      ; T IS TEMPERATURE IN K 
      ; 8.314 IS THE GAS CONSTANT (m^3 Pa K^-1 mol^-1) 
      ;    or equivalently (J K^-1 mol^-1) since 1 Pa m^3 = 1 J.
      ;    Note that 1 Pa = 1 kg m^-1 s^-2 and 1J = 1 km m^2 s^-2
      ;    So 1 Pa m^3 = 1 kg m^2 s^-2 = 1J
      ; 18 is molecular weight of water (g/mol)
      ; 1.E6 IS conversion for g to microg
      ; So units of M_ice = ug/m^3
      ; CIPS gives IWC in ug/m^2, so we have to integrate z in terms of meters.
   
      ;P_SAT is given by Rapp and Thomas, 2006 as:
      ;  0.01*exp(9.550426-(5723.265/T)+3.53068*ln(T)-0.00728332*T)
      ;  The 0.01 converts from Pa to hPa.
      
      T=MLS_T_GRID
      H2O=MLS_H2O_GRID
      
      ;SATURATION VAPOR PRESSURE:
      PSAT=0.01*EXP(9.550426-(5723.265/T)+3.53068*ALOG(T)-0.00728332*T)
      ;H2O PARTIAL PRESSURE:
      PP=FLTARR(NLON,NLAT,NLEV)
      FOR I=0,NLEV-1 DO BEGIN
        PP(*,*,I)=H2O(*,*,I)*LEV(I)
      ENDFOR
      
      MICE=1.E6*18.*100*(PP-PSAT)/(T*8.314)
      ;ONLY INTEGRATE OVER A SMALL RANGE SINCE SOME LOW ALTITUDES ALSO HAVE
      ;  POSITIVE ICE AND MLS DATA ONLY VALID BELOW 90 KM OR SO.
      ;MUST MULTIPLY BY EACH DZ LAYER SEPARATELY.
      BAD=WHERE(MICE LT 0 OR FINITE(MICE) EQ 0)
      MICE(BAD)=0
      FOR I=0,NLON-1 DO BEGIN
         FOR J=0,NLAT-1 DO BEGIN
            SUM=0
            FOR K=0,NLEV-1 DO BEGIN
               IF ALT(I,J,K) GT 70 AND ALT(I,J,K) LT 90 THEN BEGIN
                  ALTHI=ALT(I,J,K)+(ALT(I,J,K-1)-ALT(I,J,K))/2.0
                  ALTLO=ALT(I,J,K)-(ALT(I,J,K)-ALT(I,J,K+1))/2.0
                  DZ=(ALTHI-ALTLO)*1000.	;INTEGRATE IN METERS, NOT KM
                  SUM=SUM+MICE(I,J,K)*DZ
               ENDIF
            ENDFOR
            ICE(I,J,IDAY)=SUM   
         ENDFOR
      ENDFOR
      ;if iday eq 10 then stop	;check to make sure the sum is correct
   ENDFOR
   
   ;AVERAGE THE ARRAY
   ICEAVG=FLTARR(NLON,NLAT)-99
   FOR I=0,NLON-1 DO BEGIN
      FOR J=0,NLAT-1 DO BEGIN
         GOOD=WHERE(ICE(I,J,*) GT 0,NGOOD)
         IF NGOOD GT 2 THEN ICEAVG(I,J)=MEDIAN(ICE(I,J,GOOD))
      ENDFOR
   ENDFOR
   FILE='avg_mls_iwc_jan_'+yyyy+'.dat'
   comment='ICE IS IWC IN MICROGRAMS PER SQUARE METER''
   save,lon,lat,ice,iceavg,comment,file=file
ENDFOR

end

