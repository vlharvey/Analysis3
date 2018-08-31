;
; read CIPS RAA data
;
; /atmos/harvey/CIPS_data/Datfiles/RAA/cips_raa_2a_orbit_?????_YYYY-DOY_v01.00_r02_alb.nc (2A=Scene data: alb, ang, cat)
; /atmos/harvey/CIPS_data/Datfiles/RAA/cips_raa_2b_orbit_?????_YYYY-DOY_v01.00_r02_alb.nc (2B=Orbit data: alb, cat)
;
dir='/atmos/harvey/CIPS_data/Datfiles/RAA/'
spawn,'ls '+dir+'cips_raa_2b*.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
    ncfile0=ifiles(ifile)
    print,ncfile0

    ncid=ncdf_open(ncfile0)
    result=ncdf_inquire(ncid)

    nvars=result.nvars        ;# variables in the file
;
; Read in the data
;
    for ivar=0,nvars-1 do begin
        result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
;
; Puts data into array called "data" and variable name into "result.name":
;
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data

; Invoke some magic to check for string data
; masquerading as byte data, but don't convert
; byte data blindly, i.e., quality_flags is a 2-dimensional
; array of byte data.
; (This is done to account for a bug in the ncdf write routine.)

        if ( ( size( data, /n_dimensions )  EQ 1 ) && $
             ( size( data, /type ) EQ 1 ) ) then $
               data = string( data )
;
; Extract each variable from the "data" structure and name it
; the corresponding "name" from "result.name":
;
        if Execute(result.name + ' = data') eq 0 then $
           Print, ' "Execute" command failed -- are you in Virtual Machine mode?'

; check
        print,result.name
        help,data
        s=size(data)
        if s(0) gt 0L then begin
           good=where(finite(data))
           if good(0) ne -1L then print,min(data(good)),' ',max(data(good))
        endif
        print,' '

    endfor
    ncdf_close,ncid
stop
endfor
;save,file='test_all.sav',AIM_ORBIT_NUMBER,BBOX,CENTER_LON,data_product,jd_time,KM_PER_PIXEL,LATITUDE,LONGITUDE,nlayers,notes,orbit_end_time,ORBIT_START_TIME,ORBIT_START_TIME_UT,ORBIT_TRACK_EPOCH,ORBIT_TRACK_X_AXIS,ORBIT_TRACK_Y_AXIS,ORBIT_TRACK_Z_AXIS,PRODUCT_CREATION_TIME,RAYLEIGH_ALBEDO,RAYLEIGH_ALBEDO_ANOMALY,RAYLEIGH_ALBEDO_ANOMALY_UNC,revision,ut_date,ut_time,version,xdim,ydim,zenith_angle

;save,file='test_all-minus-time.sav',AIM_ORBIT_NUMBER,BBOX,CENTER_LON,data_product,jd_time,KM_PER_PIXEL,LATITUDE,LONGITUDE,nlayers,notes,orbit_end_time,ORBIT_START_TIME,ORBIT_START_TIME_UT,ORBIT_TRACK_EPOCH,ORBIT_TRACK_X_AXIS,ORBIT_TRACK_Y_AXIS,ORBIT_TRACK_Z_AXIS,PRODUCT_CREATION_TIME,RAYLEIGH_ALBEDO,RAYLEIGH_ALBEDO_ANOMALY,RAYLEIGH_ALBEDO_ANOMALY_UNC,revision,ut_date,version,xdim,ydim,zenith_angle

end
