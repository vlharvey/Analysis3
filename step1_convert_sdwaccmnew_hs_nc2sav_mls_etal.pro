;
; MLS et al
; read SDWACCM hs netcdf data files from Doug Kinnison
; output .sav files at instrument locations
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD_New/f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.hs.????-*.nc
;
; CO, NO, NO2, T, Z3
;
@mkltime

dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
spawn,'ls '+dir+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.hs.2???-*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=73L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ofile=ncfile0+'_MLS_etal.sav
    dum=findfile(ofile)
;   if dum(0) ne '' then goto,jumpfile

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
    varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa)
    ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
;
; this variable is missing from Datfiles_SD_New
;
;   varid = ncdf_varid(ncid,'doy')              ; (int) year, day of year yyyyddd
;   ncdf_varget,ncid,varid,yyyydoy
    sdate_all=strcompress(date_all,/r)
    year=long(strmid(sdate_all,0,4))
    mm=long(strmid(sdate_all,4,2))
    dd=long(strmid(sdate_all,6,2))
    doy=julday(mm,dd,year)-julday(1,1,year)+1L		; need this to calculate ltime
;
; this variable is missing from the Datfiles_SD_New run
;
;   varid = ncdf_varid(ncid,'occ_type')	; (short) 1 = sunrise, -1 = sunset, 0 = N/A
;   ncdf_varget,ncid,varid,occ_type
    occ_type=0*instr_num
;
; this variable is missing from the Datfiles_SD_New run
; calculate it here from LOCAL DAY from UT time and longitude
;
;   varid = ncdf_varid(ncid,'local_time')	; (float) local solar time
;   ncdf_varget,ncid,varid,ltime_all
    ltime_all=0.*datesec
    uttime=24.*datesec/86400.
    lday=doy+0*datesec
    MKLTIME,UTTIME,LON,LTIME_all,lday

    varid = ncdf_varid(ncid,'CO')
    ncdf_varget,ncid,varid,codata
    varid = ncdf_varid(ncid,'O3')
    ncdf_varget,ncid,varid,o3data
    varid = ncdf_varid(ncid,'NO')
    ncdf_varget,ncid,varid,nodata
    varid = ncdf_varid(ncid,'NO2')
    ncdf_varget,ncid,varid,no2data
    varid = ncdf_varid(ncid,'T')
    ncdf_varget,ncid,varid,tdata
    varid = ncdf_varid(ncid,'Z3')
    ncdf_varget,ncid,varid,gpdata
;
; add methane and water, etc
;
    varid = ncdf_varid(ncid,'H2O')
    ncdf_varget,ncid,varid,h2odata
    varid = ncdf_varid(ncid,'CO2')
    ncdf_varget,ncid,varid,co2data
    varid = ncdf_varid(ncid,'CH4')
    ncdf_varget,ncid,varid,ch4data
    varid = ncdf_varid(ncid,'CLO')
    ncdf_varget,ncid,varid,clodata
    varid = ncdf_varid(ncid,'CLONO2')
    ncdf_varget,ncid,varid,clono2data
    varid = ncdf_varid(ncid,'H')
    ncdf_varget,ncid,varid,hdata
    varid = ncdf_varid(ncid,'HNO3_GAS')
    ncdf_varget,ncid,varid,hno3data
    varid = ncdf_varid(ncid,'HO2')
    ncdf_varget,ncid,varid,ho2data
    varid = ncdf_varid(ncid,'N2O')
    ncdf_varget,ncid,varid,n2odata
    varid = ncdf_varid(ncid,'OH')
    ncdf_varget,ncid,varid,ohdata
    varid = ncdf_varid(ncid,'U')
    ncdf_varget,ncid,varid,udata
    varid = ncdf_varid(ncid,'V')
    ncdf_varget,ncid,varid,vdata
;
; day of year
; yyyydoy is missing. doy calculated above
;  
;   sdoy=strcompress(yyyydoy,/remove_all)
;   doy=long(strmid(sdoy,4,3))
;
; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa
;
    pdata=fltarr(nl,ncol)
    FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
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
       CO=transpose(reform(codata(*,atsosst)))
       CO2=transpose(reform(co2data(*,atsosst)))
       CH4=transpose(reform(ch4data(*,atsosst)))
       H2O=transpose(reform(h2odata(*,atsosst)))
       CLO=transpose(reform(clodata(*,atsosst)))
       CLONO2=transpose(reform(clono2data(*,atsosst)))
       H=transpose(reform(hdata(*,atsosst)))
       HNO3=transpose(reform(hno3data(*,atsosst)))
       HO2=transpose(reform(ho2data(*,atsosst)))
       OH=transpose(reform(ohdata(*,atsosst)))

       N2O=transpose(reform(n2odata(*,atsosst)))
       NO=transpose(reform(nodata(*,atsosst)))
       NO2=transpose(reform(no2data(*,atsosst)))
       O3=transpose(reform(o3data(*,atsosst)))
       T=transpose(reform(tdata(*,atsosst)))
       U=transpose(reform(udata(*,atsosst)))
       V=transpose(reform(vdata(*,atsosst)))
       Z=transpose(reform(gpdata(*,atsosst)))
       P=transpose(reform(pdata(*,atsosst)))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,OH,N2O,HNO3,HO2,H,CLO,CLONO2,CO2,CH4,H2O,O3,NO,NO2,CO,T,U,V,Z,INSTRUMENT,COMMENT
    endif
ncdf_close,ncid
jumpfile:
endfor			; loop over 10-day files
end
