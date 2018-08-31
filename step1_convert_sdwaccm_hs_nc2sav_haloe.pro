;
; read SDWACCM hs netcdf data files from Doug Kinnison
; output .sav files at instrument locations
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.hs.*
;
; CO, NO, NO2, T, Z3
;
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
spawn,'ls '+dir+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.hs.*.nc_HALOE.sav',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=strmid(ifiles(ifile),0,109)	;ifiles(ifile)
    print,ncfile0
    ofile=ifiles(ifile)	;ncfile0+'_HALOE.sav
;   dum=findfile(ofile)
;   if dum(0) ne '' then goto,jumpfile

    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)

    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'ncol' then ncol=dim
        if name eq 'lev' then nl=dim
;       print,'read ',name,' dimension ',dim
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
    varid = ncdf_varid(ncid,'obs_date')		; observation date (YYYYMMDD)
    ncdf_varget,ncid,varid,date_all
    varid = ncdf_varid(ncid,'instr_lat')	; instrument latitude
    ncdf_varget,ncid,varid,lat
    varid = ncdf_varid(ncid,'instr_lon')	; instrument longitude
    ncdf_varget,ncid,varid,lon
    varid = ncdf_varid(ncid,'lev')
    ncdf_varget,ncid,varid,lev
    varid = ncdf_varid(ncid,'obs_time')		; obs time of day (seconds)
    ncdf_varget,ncid,varid,datesec
    varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa)
    ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
;    varid = ncdf_varid(ncid,'orbit_num')
;    ncdf_varget,ncid,varid,orbit_num
    varid = ncdf_varid(ncid,'prof_num')
    ncdf_varget,ncid,varid,prof_num_all
;
; check for HALOE
;
    atsosst=where(instr_num eq 14L,nprof)
    if atsosst(0) eq -1L then goto,jumpclose

;    varid = ncdf_varid(ncid,'occ_type')	; (short) 1 = sunrise, -1 = sunset, 0 = N/A
;    ncdf_varget,ncid,varid,occ_type
occ_type=0*fix(instr_num)              ; this variable is missing
;    varid = ncdf_varid(ncid,'local_time')	; (float) local solar time
;    ncdf_varget,ncid,varid,ltime_all	; this variable is missing
;    varid = ncdf_varid(ncid,'doy')		; (int) year, day of year yyyyddd
;    ncdf_varget,ncid,varid,yyyydoy	; this variable is missing

    varid = ncdf_varid(ncid,'CH4')
    ncdf_varget,ncid,varid,ch4data
    varid = ncdf_varid(ncid,'H2O')
    ncdf_varget,ncid,varid,h2odata
    varid = ncdf_varid(ncid,'HCL_GAS')
    ncdf_varget,ncid,varid,hcldata
    varid = ncdf_varid(ncid,'HF')
    ncdf_varget,ncid,varid,hfdata

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
   time=(datesec/86400.)*24.
   ldoy=doy
   mkltime,time,lon,ltime_all,ldoy              ; ldoy will increment depending on if the LOCAL time is yesterday or tomorrow. DOY will remain UT and this is what's saved
;
; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa
;
    pdata=fltarr(nl,ncol)
    FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
;
; strip out data at HALOE locations
;
    comment='instrument: 14=HALOE'
    atsosst=where(instr_num eq 14L,nprof)
    if atsosst(0) ne -1L then begin
       latitude=reform(lat(atsosst))
       longitude=reform(lon(atsosst))
       sctype=reform(occ_type(atsosst))
       fdoy=reform(doy(atsosst))+reform(datesec(atsosst))/86400.
       date=reform(date_all(atsosst))
       uttime=(reform(datesec(atsosst))/86400.)*24.
       ltime=reform(ltime_all(atsosst))
       instrument=reform(instr_num(atsosst))
       prof_num=reform(prof_num_all(atsosst))
       CH4=transpose(reform(ch4data(*,atsosst)))
       H2O=transpose(reform(h2odata(*,atsosst)))
       HCL=transpose(reform(hcldata(*,atsosst)))
       HF=transpose(reform(hfdata(*,atsosst)))
       NO=transpose(reform(nodata(*,atsosst)))
       NO2=transpose(reform(no2data(*,atsosst)))
       O3=transpose(reform(o3data(*,atsosst)))
       T=transpose(reform(tdata(*,atsosst)))
       Z=transpose(reform(gpdata(*,atsosst)))
       P=transpose(reform(pdata(*,atsosst)))
       print,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,LEV,FDOY,LTIME,DATE,SCTYPE,P,O3,NO,NO2,CH4,H2O,HCL,HF,T,Z,INSTRUMENT,COMMENT,PROF_NUM
    endif
jumpclose:
ncdf_close,ncid
jumpfile:
endfor			; loop over 10-day files
end
