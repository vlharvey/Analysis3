;
; MLS, MIPAS, HIRDLS, SABER, LIMS, UARS MLS, GOMOS
; read SDWACCM hs netcdf data files from Josh Pettit
; output .sav files at instrument locations
;
; /atmos/Pettit/RAISE_MEE_Simulations/PECK_JMP_Satellite/PECK_CCMI-34_JMP.cam2.satellite.2004-01-01-00000.nc
;
; CO, NO, NO2, T, Z3
;
dir='/atmos/Pettit/RAISE_MEE_Simulations/PECK_JMP_Satellite/'
spawn,'ls '+dir+'PECK_CCMI-34_JMP.cam2.satellite.2004-*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ofile=ncfile0+'_MLS_etal.sav
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
;   varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa) - MISSING
;   ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
    varid = ncdf_varid(ncid,'occ_type') ; (short) 1 = sunrise, -1 = sunset, 0 = N/A
    ncdf_varget,ncid,varid,occ_type
;occ_type=0*fix(instr_num)               ; this variable is missing
    varid = ncdf_varid(ncid,'local_time')       ; (float) local solar time
    ncdf_varget,ncid,varid,ltime_all    ; this variable is missing
    varid = ncdf_varid(ncid,'doy')              ; (int) year, day of year yyyyddd
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
;   varid = ncdf_varid(ncid,'Z3')	; MISSING - calculate from alog(lev) (since PS also missing)
;   ncdf_varget,ncid,varid,gpdata
;
; calculate day of year
;
   d=strcompress(date_all,/r)
   yy=strmid(d,0,4)
   mm=strmid(d,4,2)
   dd=strmid(d,6,2)
   jday=julday(mm,dd,yy)
   doy=jday-julday(12,31,yy-1)
;
; calculate local time
;
;  time=(datesec/86400.)*24.
;  ldoy=doy
;  mkltime,time,lon,ltime_all,ldoy              ; ldoy will increment depending on if the LOCAL time is yesterday or tomorrow. DOY will remain UT and this is what's saved
;
; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa      APPROXIMATE with LEV - PS missing
; Calculate APPROXIMATE GPH - need to output Z3 in any publication quality analysis
;
    pdata=fltarr(nl,ncol)
;   FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
    FOR j=0,ncol-1 DO pdata(*,j)=lev
    hscale=7000.
    gpdata=0.*pdata
    for j=0,ncol-1L do gpdata(*,j) = hscale * 1e-3 * alog(1e3/lev)
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
