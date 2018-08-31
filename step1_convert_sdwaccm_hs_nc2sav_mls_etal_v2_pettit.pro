;
; MLS, MIPAS, HIRDLS, SABER, LIMS, UARS MLS, GOMOS
; read SDWACCM hs netcdf data files from Doug Kinnison
; output .sav files at instrument locations
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.*
;
; CO, NO, NO2, O3, T, Z3 -> CH4, N2O, H2O, CO2, O2, HF
;
@mkltime

dir='/atmos/Pettit/RAISE_MEE_Simulations/*_Satellite/'
spawn,'ls '+dir+'*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ofile=ncfile0+'_MLS_etal_v2.sav
    dum=findfile(ofile)
    if dum(0) ne '' then goto,jumpfile

    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'ncol' then ncol=dim
        if name eq 'lev' then nl=dim
        print,'read ',name,' dimension ',dim
    endfor
    varid = ncdf_varid(ncid,'P0')
    ncdf_varget,ncid,varid,P0
    varid = ncdf_varid(ncid,'hyai')
    ncdf_varget,ncid,varid,hyai
    varid = ncdf_varid(ncid,'hybi')
    ncdf_varget,ncid,varid,hybi
    varid = ncdf_varid(ncid,'hyam')
    ncdf_varget,ncid,varid,hyam
    varid = ncdf_varid(ncid,'hybm')
    ncdf_varget,ncid,varid,hybm
    varid = ncdf_varid(ncid,'date')		; current date (YYYYMMDD)
    ncdf_varget,ncid,varid,date_all
    varid = ncdf_varid(ncid,'lat')
    ncdf_varget,ncid,varid,lat
    varid = ncdf_varid(ncid,'lon')
    ncdf_varget,ncid,varid,lon
    varid = ncdf_varid(ncid,'lev')
    ncdf_varget,ncid,varid,lev
    varid = ncdf_varid(ncid,'datesec')	; current seconds of current date
    ncdf_varget,ncid,varid,datesec
;   varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa)
;   ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
;   varid = ncdf_varid(ncid,'occ_type')	; (short) 1 = sunrise, -1 = sunset, 0 = N/A
;   ncdf_varget,ncid,varid,occ_type
    occ_type=0*instr_num

;   varid = ncdf_varid(ncid,'doy')		; (int) year, day of year yyyyddd
;   ncdf_varget,ncid,varid,yyyydoy
    sdate_all=strcompress(date_all,/r)
    year=long(strmid(sdate_all,0,4))
    mm=long(strmid(sdate_all,4,2))
    dd=long(strmid(sdate_all,6,2))
    doy=julday(mm,dd,year)-julday(1,1,year)+1L          ; need this to calculate ltime

;   varid = ncdf_varid(ncid,'local_time')       ; (float) local solar time
;   ncdf_varget,ncid,varid,ltime_all
    ltime_all=0.*datesec
    uttime=24.*datesec/86400.
    lday=doy+0*datesec
    MKLTIME,UTTIME,LON,LTIME_all,lday

    varid = ncdf_varid(ncid,'CH4')
    ncdf_varget,ncid,varid,ch4data
;   varid = ncdf_varid(ncid,'O2')
;   ncdf_varget,ncid,varid,o2data
o2data=0.*ch4data
    varid = ncdf_varid(ncid,'N2O')
    ncdf_varget,ncid,varid,n2odata
    varid = ncdf_varid(ncid,'H2O')
    ncdf_varget,ncid,varid,h2odata
    varid = ncdf_varid(ncid,'CO2')
    ncdf_varget,ncid,varid,co2data
;
; day of year
;  
;   sdoy=strcompress(yyyydoy,/remove_all)
;   doy=long(strmid(sdoy,4,3))
;
; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa
;
    pdata=fltarr(nl,ncol)
;   FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
    FOR i=0,n_elements(instr_num)-1L DO pdata(*,i)=lev
;
; strip out data at measurement locations
;
    comment='instrument: 1=Aura-MLS, 3=HIRDLS, 4=SABER, 9=MIPAS, 19=UARS-MLS, 21=LIMS, 37=GOMOS'
    atsosst=where(instr_num eq 1L or instr_num eq 3L or instr_num eq 4L or instr_num eq 9L or instr_num eq 19L or instr_num eq 21L or instr_num eq 37L,nprof)
    if atsosst(0) ne -1L then begin
       latitude=reform(lat(atsosst))
       longitude=reform(lon(atsosst))
       sctype=reform(occ_type(atsosst))
       fdoy=reform(doy(atsosst))+reform(datesec(atsosst))/86400.
       date=reform(date_all(atsosst))
       ltime=reform(ltime_all(atsosst))
       instrument=reform(instr_num(atsosst))
       CH4=transpose(reform(ch4data(*,atsosst)))
       N2O=transpose(reform(n2odata(*,atsosst)))
       H2O=transpose(reform(h2odata(*,atsosst)))
       O2=transpose(reform(o2data(*,atsosst)))
       CO2=transpose(reform(co2data(*,atsosst)))
       P=transpose(reform(pdata(*,atsosst)))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,O2,N2O,H2O,CH4,CO2,INSTRUMENT,COMMENT
    endif
ncdf_close,ncid
jumpfile:
endfor			; loop over 10-day files
end
