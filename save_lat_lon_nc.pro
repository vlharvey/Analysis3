;
; save CIPS L3 level_3_lat_lon_north.nc and level_3_lat_lon_south.nc
;
restore,'read_cips_file.sav
;
; save netcdf file for NH
;
ifile='/Volumes/Data/CIPS_data/Datfiles/cips_sci_3a_2007-176_v04.20_r04.nc'
ofile='/Volumes/Data/CIPS_data/Datfiles/level_3_lat_lon_north.nc'
data = read_cips_file(ifile)
LATITUDE=data.latitude
LONGITUDE=data.longitude
result=size(LONGITUDE)
dim1_LONGITUDE=result(1)
dim1_LATITUDE=result(2)
ncid = ncdf_create(ofile,/CLOBBER)
xid  = ncdf_dimdef(ncid,'dim1_LATITUDE' ,dim1_LATITUDE)
yid  = ncdf_dimdef(ncid,'dim1_LONGITUDE',dim1_LONGITUDE)
latid= ncdf_vardef(ncid,'LATITUDE' ,[xid,yid])
lonid= ncdf_vardef(ncid,'LONGITUDE',[xid,yid])
ncdf_control,ncid,/ENDEF
ncdf_varput,ncid,latid,LATITUDE ,COUNT=[dim1_LONGITUDE,dim1_LATITUDE]
ncdf_varput,ncid,lonid,LONGITUDE,COUNT=[dim1_LONGITUDE,dim1_LATITUDE]
ncdf_close,ncid
;
; save netcdf file for SH
;
ifile='/Volumes/Data/CIPS_data/Datfiles/cips_sci_3a_2009-360_v04.20_r04.nc'
ofile='/Volumes/Data/CIPS_data/Datfiles/level_3_lat_lon_south.nc'
data = read_cips_file(ifile)
LATITUDE=data.latitude
LONGITUDE=data.longitude
result=size(LONGITUDE)
dim1_LONGITUDE=result(1)
dim1_LATITUDE=result(2)
ncid = ncdf_create(ofile,/CLOBBER)
xid  = ncdf_dimdef(ncid,'dim1_LATITUDE' ,dim1_LATITUDE)
yid  = ncdf_dimdef(ncid,'dim1_LONGITUDE',dim1_LONGITUDE)
latid= ncdf_vardef(ncid,'LATITUDE' ,[xid,yid])
lonid= ncdf_vardef(ncid,'LONGITUDE',[xid,yid])
ncdf_control,ncid,/ENDEF
ncdf_varput,ncid,latid,LATITUDE ,COUNT=[dim1_LONGITUDE,dim1_LATITUDE]
ncdf_varput,ncid,lonid,LONGITUDE,COUNT=[dim1_LONGITUDE,dim1_LATITUDE]
ncdf_close,ncid
end
