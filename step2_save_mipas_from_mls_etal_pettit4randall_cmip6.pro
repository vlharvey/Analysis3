;
; MIPAS
;
; /atmos/Pettit/RAISE_MEE_Simulations/CMIP6_Satellite/CMIP6_CCMI-34.cam2.satellite.*MLS_etal_v2.sav
;
; CO, NO, NO2, T, Z3
;
;dir='/atmos/Pettit/RAISE_MEE_Simulations/CMIP6_Satellite/'
dir='/atmos/Pettit/RAISE_MEE_Simulations/CMIP6/CMIP6_Satellite/'
;spawn,'ls '+dir+'CMIP6_CCMI-34.cam2.satellite.2004-*.nc',ifiles
spawn,'ls '+dir+'NEW_CMIP6_CCMI34.cam2.satellite.200?-*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)+'_MLS_etal_v2.sav'
    dum=findfile(ncfile0)
    if dum(0) eq '' then goto,jumpfile		; no MLS data that year
    print,ncfile0
    ofile=ifiles(ifile)+'_MIPAS.sav'
    dum=findfile(ofile)
;   if dum(0) ne '' then goto,jumpfile		; MIPAS file already exists
    restore,ncfile0
    restore,ifiles(ifile)+'_MLS_etal.sav'
;
; strip out data at MIPAS locations
;
    comment='instrument: 9=MIPAS'
    atmipas=where(instrument eq 9L,nprof)
    if atmipas(0) ne -1L then begin
       latitude=reform(latitude(atmipas))
       longitude=reform(longitude(atmipas))
       sctype=reform(sctype(atmipas))
       fdoy=reform(fdoy(atmipas))
       date=reform(date(atmipas))
       ltime=reform(ltime(atmipas))
       instrument=reform(instrument(atmipas))
       CH4=reform(ch4(atmipas,*))
       CO=reform(co(atmipas,*))
       CO2=reform(co2(atmipas,*))
       NO=reform(no(atmipas,*))
       NO2=reform(no2(atmipas,*))
       N2O=reform(n2o(atmipas,*))
       H2O=reform(h2o(atmipas,*))
       O3=reform(o3(atmipas,*))
       O2=reform(o2(atmipas,*))
       T=reform(t(atmipas,*))
       Z=reform(z(atmipas,*))
       P=reform(p(atmipas,*))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,O3,NO,NO2,CO,T,Z,CH4,CO2,N2O,H2O,O2,INSTRUMENT,COMMENT
    endif
jumpfile:
endfor			; loop over 10-day files
end
