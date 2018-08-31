;
; read SDWACCM and SDWACCM-D hs netcdf data files produced by Josh Pettit
; and check that they are different
;
; /atmos/Pettit/Cora_WACCM/Cora_WACCM.cam.hs.2003-09-01-00000.nc
; /atmos/Pettit/Cora_WACCM-D/Cora_WACCM-D.cam.hs.2003-09-01-00000.nc
;
dir='/atmos/Pettit/Cora_WACCM/'
spawn,'ls '+dir+'Cora_WACCM_Non-D.cam.hs.????-*.nc',ifiles
ifile=10			; compare first file
ncfile0=ifiles(ifile)
print,ncfile0
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
    varid = ncdf_varid(ncid,'date')             ; current date (YYYYMMDD)
    ncdf_varget,ncid,varid,date_all
    varid = ncdf_varid(ncid,'lat')
    ncdf_varget,ncid,varid,lat
    varid = ncdf_varid(ncid,'lon')
    ncdf_varget,ncid,varid,lon
    varid = ncdf_varid(ncid,'lev')
    ncdf_varget,ncid,varid,lev
    varid = ncdf_varid(ncid,'datesec')  ; current seconds of current date
    ncdf_varget,ncid,varid,datesec
    varid = ncdf_varid(ncid,'PS')               ; surface pressure (Pa)
    ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')        ; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
;   varid = ncdf_varid(ncid,'occ_type') ; (short) 1 = sunrise, -1 = sunset, 0 = N/A
;   ncdf_varget,ncid,varid,occ_type
occ_type=0*fix(instr_num)               ; this variable is missing
;   varid = ncdf_varid(ncid,'local_time')       ; (float) local solar time
;   ncdf_varget,ncid,varid,ltime_all    ; this variable is missing
;   varid = ncdf_varid(ncid,'doy')              ; (int) year, day of year yyyyddd
;   ncdf_varget,ncid,varid,yyyydoy
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
ncdf_close,ncid
;
; SD-WACCM-D
;
dir='/atmos/Pettit/Cora_WACCM-D/'
spawn,'ls '+dir+'Cora_WACCM.cam.hs.????-*.nc',ifiles
ncfile0=ifiles(ifile)
print,ncfile0
ncid=ncdf_open(ncfile0)
varid = ncdf_varid(ncid,'NO')
ncdf_varget,ncid,varid,nodata_d
ncdf_close,ncid
;
; SD-WACCM MEE
;
dir='/atmos/Pettit/Cora_WACCM_MEE/'
spawn,'ls '+dir+'Cora_WACCM_Non-D_MEE.cam.hs.????-*.nc',ifiles
ncfile0=ifiles(ifile)
print,ncfile0
ncid=ncdf_open(ncfile0)
varid = ncdf_varid(ncid,'NO')
ncdf_varget,ncid,varid,nodata_mee
ncdf_close,ncid
;
; SD-WACCM-D with MEE
;
dir='/atmos/Pettit/Cora_WACCM-D_MEE/'
spawn,'ls '+dir+'Cora_WACCM-D.cam.hs.????-*.nc',ifiles
ncfile0=ifiles(ifile)
print,ncfile0
ncid=ncdf_open(ncfile0)
varid = ncdf_varid(ncid,'NO')
ncdf_varget,ncid,varid,nodata_dmee
ncdf_close,ncid
;
; check
;
print,'base vs. D'
help,where(nodata ne nodata_d)

print,'base vs MEE'
help,where(nodata ne nodata_mee)

print,'D vs MEE'
help,where(nodata_d ne nodata_mee)

print,'base vs DMEE'
help,where(nodata ne nodata_dmee)

print,'D vs DMEE'
help,where(nodata_d ne nodata_dmee)

print,'MEE vs DMEE'
help,where(nodata_mee ne nodata_dmee)


end
