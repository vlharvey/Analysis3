pro write_waccm4_nc3,ofile,nc,nr,nth,alon,alat,thlev,$
          ipv,prs,gph,u,v,q,qdf,mark,ttgw,sf
;
; Create netCDF file and erase existing netCDF file if it exists
;
;print,'writing ',ofile
nocid = ncdf_create(ofile,/CLOBBER)
latdimid=ncdf_dimdef(nocid, 'number_of_latitudes' , nr)
londimid=ncdf_dimdef(nocid, 'number_of_longitudes', nc)
levdimid=ncdf_dimdef(nocid, 'number_of_levels'    , nth)
lonsid = ncdf_vardef(nocid, 'longitude',  londimid)
latsid = ncdf_vardef(nocid, 'latitude' ,  latdimid)
levsid = ncdf_vardef(nocid, 'theta'    ,  levdimid)
vid  = ncdf_vardef(nocid, 'IPV' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'P'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'U'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'V'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'QDF' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'Q'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'GPH' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'TTGW', [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'SF'  , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'MARK', [latdimid,londimid,levdimid])
ncdf_attput, nocid, 'longitude', 'longname', 'longitude' & ncdf_attput, nocid, 'longitude', 'units', 'deg E'
ncdf_attput, nocid, 'latitude', 'longname', 'latitude' & ncdf_attput, nocid, 'latitude', 'units', 'deg'
ncdf_attput, nocid, 'theta', 'longname', 'potential temperature' & ncdf_attput, nocid, 'theta', 'units', 'K'
ncdf_attput, nocid, 'IPV', 'longname', 'Isentropic Potential Vorticity' & ncdf_attput, nocid, 'IPV', 'units', 'K m^2 /s /kg'
ncdf_attput, nocid, 'P', 'longname', 'Pressure' & ncdf_attput, nocid, 'P', 'units', 'hPa'
ncdf_attput, nocid, 'U', 'longname', 'Zonal Wind' & ncdf_attput, nocid, 'U', 'units', 'm/s'
ncdf_attput, nocid, 'V', 'longname', 'Meridional Wind' & ncdf_attput, nocid, 'V', 'units', 'm/s'
ncdf_attput, nocid, 'QDF', 'longname', 'Strain/Rotation Parameter' & ncdf_attput, nocid, 'QDF', 'units', 's-1'
ncdf_attput, nocid, 'Q', 'longname', 'Net Diabatic Heating Rate' & ncdf_attput, nocid, 'Q', 'units', 'K/day'
ncdf_attput, nocid, 'GPH', 'longname', 'Geopotential Height' & ncdf_attput, nocid, 'GPH', 'units', 'm'
ncdf_attput, nocid, 'TTGW', 'longname', 'Temperature Tendency due to Gravity Waves' & ncdf_attput, nocid, 'TTGW', 'units', 'K/day'
ncdf_attput, nocid, 'SF', 'longname', 'Streamfunction' & ncdf_attput, nocid, 'SF', 'units', 'm2/s'
ncdf_attput, nocid, 'MARK', 'longname', 'Vortex Marker' & ncdf_attput, nocid, 'MARK', 'units', 'negative for anticyclones: positive for polar vortices'
ncdf_control,nocid,/ENDEF
ncdf_varput, nocid, 'longitude', alon  , COUNT=[nc]
ncdf_varput, nocid, 'latitude' , alat  , COUNT=[nr]
ncdf_varput, nocid, 'theta'    , thlev , COUNT=[nth]
ncdf_varput, nocid, 'IPV' , ipv     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'P'   , prs     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'GPH' , gph     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'U'   , u       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'V'   , v       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'Q'   , q       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'QDF' , qdf     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'MARK'  , mark  , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'TTGW', ttgw    , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'SF'  , sf      , COUNT=[nr,nc,nth]
ncdf_close,nocid
return
end
