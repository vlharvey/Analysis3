;
; SABER
;
; /atmos/Pettit/RAISE_MEE_Simulations/*_Satellite/*MLS_etal.sav
;
; CO, NO, NO2, T, Z3
;
dir='/atmos/Pettit/RAISE_MEE_Simulations/*_Satellite/'
spawn,'ls '+dir+'*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)+'_MLS_etal.sav'
    print,ncfile0
    dum=findfile(ncfile0)
    if dum(0) eq '' then goto,jumpfile		; no MLS file
    ofile=ifiles(ifile)+'_SABER.sav'
    dum=findfile(ofile)
    if dum(0) ne '' then goto,jumpfile		; SABER already exists
    restore,ncfile0
;
; strip out data at SABER locations
;
    comment='instrument: 4=SABER'
    atsaber=where(instrument eq 4L,nprof)
    if atsaber(0) ne -1L then begin
       latitude=reform(latitude(atsaber))
       longitude=reform(longitude(atsaber))
       sctype=reform(sctype(atsaber))
       fdoy=reform(fdoy(atsaber))
       date=reform(date(atsaber))
       ltime=reform(ltime(atsaber))
       instrument=reform(instrument(atsaber))
       CO=reform(co(atsaber,*))
       NO=reform(no(atsaber,*))
       NO2=reform(no2(atsaber,*))
       O3=reform(o3(atsaber,*))
       T=reform(t(atsaber,*))
       Z=reform(z(atsaber,*))
       P=reform(p(atsaber,*))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,O3,NO,NO2,CO,T,Z,INSTRUMENT,COMMENT
    endif
jumpfile:
endfor			; loop over 10-day files
end
