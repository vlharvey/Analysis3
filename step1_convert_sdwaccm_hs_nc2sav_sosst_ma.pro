;
; read SDWACCM hs netcdf data files from Doug Kinnison
; output .sav files at instrument locations
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_ma.002.cam.hs.*
;
; CO, NO, NO2, T, Z3
;
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
spawn,'ls '+dir+'f_1975-2010_2deg_refc1sd_wa4_ma.002.cam.hs.????-*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ofile=ncfile0+'_SOSST.sav
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
    varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa)
    ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
    varid = ncdf_varid(ncid,'occ_type')	; (short) 1 = sunrise, -1 = sunset, 0 = N/A
    ncdf_varget,ncid,varid,occ_type
    varid = ncdf_varid(ncid,'local_time')	; (float) local solar time
    ncdf_varget,ncid,varid,ltime_all
    varid = ncdf_varid(ncid,'doy')		; (int) year, day of year yyyyddd
    ncdf_varget,ncid,varid,yyyydoy
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
; day of year
;  
    sdoy=strcompress(yyyydoy,/remove_all)
    doy=long(strmid(sdoy,4,3))
;
; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa
;
    pdata=fltarr(nl,ncol)
    FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
;
; strip out data at SOSST locations
;
    comment='instrument: 2=ACE-FTS, 5=SOFIE, 10=POAM2, 11=POAM3, 12=SAGE2, 13=SAGE3, 14=HALOE, 18=SAGE1'
    atsosst=where(instr_num eq 2L or instr_num eq 5L or instr_num eq 10L or instr_num eq 11L or instr_num eq 12L or instr_num eq 13L or instr_num eq 14L or instr_num eq 18L,nprof)
    if atsosst(0) ne -1L then begin
       latitude=reform(lat(atsosst))
       longitude=reform(lon(atsosst))
       sctype=reform(occ_type(atsosst))
       fdoy=reform(doy(atsosst))+reform(datesec(atsosst))/86400.
       date=reform(date_all(atsosst))
       ltime=reform(ltime_all(atsosst))
       instrument=reform(instr_num(atsosst))
       CO=transpose(reform(codata(*,atsosst)))
       NO=transpose(reform(nodata(*,atsosst)))
       NO2=transpose(reform(no2data(*,atsosst)))
       O3=transpose(reform(o3data(*,atsosst)))
       T=transpose(reform(tdata(*,atsosst)))
       Z=transpose(reform(gpdata(*,atsosst)))
       P=transpose(reform(pdata(*,atsosst)))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,O3,NO,NO2,CO,T,Z,INSTRUMENT,COMMENT
    endif
ncdf_close,ncid
jumpfile:
endfor			; loop over 10-day files
end
