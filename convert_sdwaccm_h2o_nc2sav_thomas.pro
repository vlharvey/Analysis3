;
; read SDWACCM h5 netcdf data files and output water and Z3 in IDL save format for G. Thomas
; July 1 2008 and surrounding 20 days
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD_New/f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.2008-06-27-00000.nc
;
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
spawn,'ls '+dir+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.2008-07*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ofile=ncfile0+'_h2o.sav
    dum=findfile(ofile)
;   if dum(0) ne '' then goto,jumpfile

    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'lat' then nr=dim
        if name eq 'lon' then nc=dim
        if name eq 'lev' then nl=dim
        if name eq 'time' then nt=dim
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
    ncdf_varget,ncid,varid,date
    varid = ncdf_varid(ncid,'lat')
    ncdf_varget,ncid,varid,lat
    varid = ncdf_varid(ncid,'lon')
    ncdf_varget,ncid,varid,lon
    varid = ncdf_varid(ncid,'lev')
    ncdf_varget,ncid,varid,lev
    varid = ncdf_varid(ncid,'H2O')
    ncdf_varget,ncid,varid,H2O
    varid = ncdf_varid(ncid,'Z3')
    ncdf_varget,ncid,varid,GPH

    print,ofile
    SAVE,FILE=OFILE,LAT,LON,LEV,DATE,H2O,GPH
    ncdf_close,ncid
jumpfile:
endfor			; loop over 10-day files
end
