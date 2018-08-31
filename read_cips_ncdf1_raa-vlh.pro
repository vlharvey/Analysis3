;+
; Laboratory for Atmospheric and Space Physics
; University of Colorado, Boulder, Colorado, USA
;
; FILENAME:
;   read_cips_ncdf1_raa.pro adapted from read_cips_ncdf1.pro
;
; AUTHOR:
;   Jim Craft
;
; DATE:    April 2, 2012
; 
; MODIFIED:
;   Cora Randall, 13 May 2012
;   V. Lynn Harvey, 22 Feb 2017
;
; PURPOSE:
;   This function accesses CIPS data stored in NetCDF Files and returns the data
;   in a structure.
;   
; KEYWORDS:
;   verbose - used for diagnostic purposes.  Using this keyword will provide user
;             with information about the file and the fields within the file.
;
; CALL:
;   data = read_cips_ncdf1(filename [, /verbose])
;      "data" is the name for the returned data structure.
;      "filename" must include the path and filename of the CIPS ncdf file.
;      "/verbose" is optional
;
; DIFFERENCES BETWEEN READ_CIPS_NCDF1_RAA.PRO AND READ_CIPS_NCDF2_RAA.PRO:
;   read_cips_ncdf1_raa.pro is a function. It returns all variables in a structure. 
;   The path and filename are included in the call statement.
;    
;   read_cips_ncdf2_raa.pro is a program. It reads all variables individually into the main level.
;   The path and filename are included in the first two statements in the code.
;   
; NOTES:
;    ncdump listings at the end are for the RAA L2A (scene) "cat", "alb", and "ang" files and L2B (orbit strip) "cat" and "alb" files
;    "cat" stands for catalog, "alb" stands for albedo, and "ang" stands for angle
;
; Example list of the CIPS RAA L2A and L2B files for each orbit (five total, including file sizes):
;       cips_raa_2a_orbit_52968_2017-001_v01.00_r03_alb.nc - 14 Mb
;       cips_raa_2a_orbit_52968_2017-001_v01.00_r03_ang.nc - 19 Mb
;       cips_raa_2a_orbit_52968_2017-001_v01.00_r03_cat.nc - 9.7 Mb
;       cips_raa_2b_orbit_52968_2017-001_v01.00_r03_alb.nc - 22 Mb
;       cips_raa_2b_orbit_52968_2017-001_v01.00_r03_cat.nc - 44 Mb
;    
function read_cips_ncdf1_raa, filename, verbose=verbose

  if ~ncdf_exists() then begin
    print, 'NetCDF Library is not available!'
    exit
  endif
  ; make sure the file exists
  if file_test( filename ) then begin
    if keyword_set( verbose ) then $
      print, 'Opening ', filename
    ; open it
    ncdfId = ncdf_open( filename )
    if keyword_set( verbose ) then $
      print, 'NetCDF ID = ', ncdfId
    ; grab the basic netCDF structure
    ncdfStruct = ncdf_inquire( ncdfId )
    if keyword_set( verbose ) then begin
      print
      print, 'The NetCDF Structure returned from the file has the following:'
      print, 'Ndims = ', ncdfStruct.Ndims
      print, 'Nvars = ', ncdfStruct.Nvars
      print, 'Ngatts = ', ncdfStruct.Ngatts
      print, 'RecDim = ', ncdfStruct.RecDim
      print
    endif
    
    ;loop through the structure and grab the tag names and associated data
    for index = 0, ncdfStruct.Nvars - 1 do begin
      ; inquire on the netCDF file variables
      zed = ncdf_varinq( ncdfId, index )
      if keyword_set( verbose ) then $
        print, 'NetCDF Structure Tag Name: ', zed.name
      ; get the variable id
      varId = ncdf_varid( ncdfId, zed.name )
      ; get the data
      ncdf_varget, ncdfId, varId, varData

      ; invoke some magic to check for string data
      ; masquerading as byte data, but don't convert
      ; byte data blindly, ie quality_flags is a 2-dimensional
      ; array of byte data.
      ; ***This is needed for PMC data, not RAA data but is left here to be backward compatible
      if ( ( size( varData, /n_dimensions )  EQ 1 ) && $
           ( size( varData, /type ) EQ 1 ) ) then $
        varData = string( varData )

      if keyword_set( verbose ) then $
        help, varData
      ; build the structure to return
      if ( index EQ 0 ) then begin
        ncdfDataStruct = create_struct( zed.name, varData )
      endif else begin
        ncdfDataStruct = create_struct( ncdfDataStruct, zed.name, varData )
      endelse
    endfor
    ; close the netCDF file
    ncdf_close, ncdfId
  endif else $  
    return, -1
  
  return, ncdfDataStruct
end

;*********************************************************************
;NCDUMP OUTPUT FOR LEVEL 2A CAT, ALB, and ANG files and LEVEL 2B CAT and ALB files
;
;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2A CATALOG FILE:
;netcdf cips_raa_2a_orbit_52968_2017-001_v01.00_r03_cat {
;dimensions:
;        string = 41 ;
;        dim1_UT_TIME = 15 ;
;        dim1_JD_TIME = 15 ;
;        \4 = 4 ;
;        NSCENES = 15 ;
;        dim1_LATITUDE = 293 ;
;        dim2_LATITUDE = 144 ;
;        dim3_LATITUDE = 15 ;
;        dim1_LONGITUDE = 293 ;
;        dim2_LONGITUDE = 144 ;
;        dim3_LONGITUDE = 15 ;
;        dim1_ZENITH_ANGLE = 293 ;
;        dim2_ZENITH_ANGLE = 144 ;
;        dim3_ZENITH_ANGLE = 15 ;
;        dim1_ORBIT_TRACK_X_AXIS = 3 ;
;        dim1_ORBIT_TRACK_Y_AXIS = 3 ;
;        dim1_ORBIT_TRACK_Z_AXIS = 3 ;
;variables:
;        int AIM_ORBIT_NUMBER ;
;                AIM_ORBIT_NUMBER:description = "orbit number (long) to which all data in the file applies" ;
;        char VERSION(string) ;
;                VERSION:description = "Data version number" ;
;        char REVISION(string) ;
;                REVISION:description = "Data revision number" ;
;        char PRODUCT_CREATION_TIME(string) ;
;                PRODUCT_CREATION_TIME:description = "String containing UT time at which data file was produced / Wed Feb 9 21:17:26 2011" ;
;        char UT_DATE(string) ;
;                UT_DATE:description = "UT date of orbit" ;
;                UT_DATE:units = "yyyymmdd (string)" ;
;        double UT_TIME(dim1_UT_TIME) ;
;                UT_TIME:description = "UT time for each scene / [NSCENES]" ;
;                UT_TIME:units = "yyyymmdd.dd" ;
;        double JD_TIME(dim1_JD_TIME) ;
;                JD_TIME:description = "JD time for each scene / [NSCENES]" ;
;                JD_TIME:units = "days from noon Universal Time on January 1, 4713 BC" ;
;        double ORBIT_START_TIME ;
;                ORBIT_START_TIME:description = "GPS start time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6218671e+014" ;
;                ORBIT_START_TIME:units = "microseconds" ;
;        char ORBIT_START_TIME_UT(string) ;
;                ORBIT_START_TIME_UT:description = "UT string for start time of orbit" ;
;        double ORBIT_END_TIME ;
;                ORBIT_END_TIME:description = "GPS end time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6219250e+014" ;
;                ORBIT_END_TIME:units = "microseconds" ;
;        int XDIM ;
;                XDIM:description = "Number of along-orbit-track elements in the data arrays" ;
;        int YDIM ;
;                YDIM:description = "Number of cross-orbit-track elements in the data arrays" ;
;        int NSCENES ;
;                NSCENES:description = "Number of scenes in the orbit" ;
;        float KM_PER_PIXEL ;
;                KM_PER_PIXEL:description = "Linear dimension of square pixel occupying area of CIPS resolution element." ;
;                KM_PER_PIXEL:units = "km" ;
;        int BBOX(NSCENES, \4) ;
;                BBOX:description = "Bounding Box: Bottom-Left indices and size of box within the larger grid orbit track grid" ;
;        double CENTER_LON ;
;                CENTER_LON:description = "Center longitude of the orbit. Lon at peak latitude (spring/summer hem). Divides ascending and descending node." ;
;                CENTER_LON:units = "Degrees" ;
;        float LATITUDE(dim3_LATITUDE, dim2_LATITUDE, dim1_LATITUDE) ;
;                LATITUDE:description = "Latitude of each element. Latitudes greater (less) than 90 (-90) indicate ascending node data./ [XDIM,YDIM,NSCENES]" ;
;                LATITUDE:units = "Degrees" ;
;        float LONGITUDE(dim3_LONGITUDE, dim2_LONGITUDE, dim1_LONGITUDE) ;
;                LONGITUDE:description = "Longitude of each element ranges from -180 to 180 / [XDIM,YDIM,NSCENES]" ;
;                LONGITUDE:units = "Degrees" ;
;        double ZENITH_ANGLE(dim3_ZENITH_ANGLE, dim2_ZENITH_ANGLE, dim1_ZENITH_ANGLE) ;
;                ZENITH_ANGLE:description = "Solar zenith angle (SZA) of each element. The value is specified at the altitude of the maximum contribution to the Rayleigh background. Generally around 55 km but increasing with increasing SZA. / [XDIM,YDIM,NSCENES]" ;
;                ZENITH_ANGLE:units = "Degrees" ;
;        double ORBIT_TRACK_X_AXIS(dim1_ORBIT_TRACK_X_AXIS) ;
;                ORBIT_TRACK_X_AXIS:description = "X axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_Y_AXIS(dim1_ORBIT_TRACK_Y_AXIS) ;
;                ORBIT_TRACK_Y_AXIS:description = "Y axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_Z_AXIS(dim1_ORBIT_TRACK_Z_AXIS) ;
;                ORBIT_TRACK_Z_AXIS:description = "Z axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_EPOCH ;
;                ORBIT_TRACK_EPOCH:description = "GPS microsecond Time at which the orbit track axis approximately matches the true orbit track. Due to the rotation of the earth the orbit track doesn\'t trace out a plane in earth fixed coordinates." ;
;        char DATA_PRODUCT(string) ;
;                DATA_PRODUCT:description = "Brief description of the data product." ;
;        char NOTES(string) ;
;                NOTES:description = "Any additional notes. / Blank." ;
;}
;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2A ALBEDO FILE:
;netcdf cips_raa_2a_orbit_52968_2017-001_v01.00_r03_alb {
;dimensions:
;        dim1_RAYLEIGH_ALBEDO_ANOMALY = 293 ;
;        dim2_RAYLEIGH_ALBEDO_ANOMALY = 144 ;
;        dim3_RAYLEIGH_ALBEDO_ANOMALY = 15 ;
;        dim1_RAYLEIGH_ALBEDO_ANOMALY_UNC = 293 ;
;        dim2_RAYLEIGH_ALBEDO_ANOMALY_UNC = 144 ;
;        dim3_RAYLEIGH_ALBEDO_ANOMALY_UNC = 15 ;
;        dim1_RAYLEIGH_ALBEDO = 293 ;
;        dim2_RAYLEIGH_ALBEDO = 144 ;
;        dim3_RAYLEIGH_ALBEDO = 15 ;
;        dim1_OVERLAP_OFFSET = 4 ;
;        dim2_OVERLAP_OFFSET = 15 ;
;        dim1_OVERLAP_ERROR = 5 ;
;        dim2_OVERLAP_ERROR = 15 ;
;        dim1_OVERLAP_ERROR0 = 5 ;
;        dim2_OVERLAP_ERROR0 = 15 ;
;variables:
;        double RAYLEIGH_ALBEDO_ANOMALY(dim3_RAYLEIGH_ALBEDO_ANOMALY, dim2_RAYLEIGH_ALBEDO_ANOMALY, dim1_RAYLEIGH_ALBEDO_ANOMALY) ;
;                RAYLEIGH_ALBEDO_ANOMALY:description = "Albedo Anomaly: (observed_albedo-basic_state_albedo)/basic_state_albedo*100 / [XDIM,YDIM,NSCENES]" ;
;                RAYLEIGH_ALBEDO_ANOMALY:units = "%" ;
;        double RAYLEIGH_ALBEDO_ANOMALY_UNC(dim3_RAYLEIGH_ALBEDO_ANOMALY_UNC, dim2_RAYLEIGH_ALBEDO_ANOMALY_UNC, dim1_RAYLEIGH_ALBEDO_ANOMALY_UNC) ;
;                RAYLEIGH_ALBEDO_ANOMALY_UNC:description = "Uncertainty in albedo anomaly due to noise. / [XDIM,YDIM,NSCENES]" ;
;                RAYLEIGH_ALBEDO_ANOMALY_UNC:units = "%" ;
;        double RAYLEIGH_ALBEDO(dim3_RAYLEIGH_ALBEDO, dim2_RAYLEIGH_ALBEDO, dim1_RAYLEIGH_ALBEDO) ;
;                RAYLEIGH_ALBEDO:description = "Total observed albedo./ [XDIM,YDIM,NSCENES]" ;
;                RAYLEIGH_ALBEDO:units = "G (10^-6 sr^-1)" ;
;        double OVERLAP_OFFSET(dim2_OVERLAP_OFFSET, dim1_OVERLAP_OFFSET) ;
;                OVERLAP_OFFSET:description = "Normalization applied to each image in a scene necessary to minimize the disagreement between overlaping fields of view for the cameras. This number is added to the orginal RAA for each image (PX,MX,PY,MY). / [4,NSCENES]" ;
;                OVERLAP_OFFSET:units = "%" ;
;        double OVERLAP_ERROR(dim2_OVERLAP_ERROR, dim1_OVERLAP_ERROR) ;
;                OVERLAP_ERROR:description = "Residual median difference between the overlap regions after the overlap offset is applied.(PX-PY,PX-MY,PY-MY,MX-PY,MX-MY) / [5,NSCENES]" ;
;                OVERLAP_ERROR:units = "%" ;
;        double OVERLAP_ERROR0(dim2_OVERLAP_ERROR0, dim1_OVERLAP_ERROR0) ;
;                OVERLAP_ERROR0:description = "Median difference between the overlap regions before applying overlap offset.(PX-PY,PX-MY,PY-MY,MX-PY,MX-MY) / [5,NSCENES]" ;
;                OVERLAP_ERROR0:units = "%" ;
;}
;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2A ANGLE FILE:
;netcdf cips_raa_2a_orbit_52968_2017-001_v01.00_r03_ang {
;dimensions:
;        dim1_VIEW_ANGLE = 293 ;
;        dim2_VIEW_ANGLE = 144 ;
;        dim3_VIEW_ANGLE = 15 ;
;        dim1_SCATTERING_ANGLE = 293 ;
;        dim2_SCATTERING_ANGLE = 144 ;
;        dim3_SCATTERING_ANGLE = 15 ;
;        dim1_VIEW_ANGLE_DERIVATIVE = 293 ;
;        dim2_VIEW_ANGLE_DERIVATIVE = 144 ;
;        dim3_VIEW_ANGLE_DERIVATIVE = 15 ;
;        dim1_ZENITH_ANGLE_DERIVATIVE = 293 ;
;        dim2_ZENITH_ANGLE_DERIVATIVE = 144 ;
;        dim3_ZENITH_ANGLE_DERIVATIVE = 15 ;
;variables:
;        double VIEW_ANGLE(dim3_VIEW_ANGLE, dim2_VIEW_ANGLE, dim1_VIEW_ANGLE) ;
;                VIEW_ANGLE:description = "Angle between the line of sight and the earth surface normal at the binning height (50 km)" ;
;                VIEW_ANGLE:units = "degrees" ;
;        double SCATTERING_ANGLE(dim3_SCATTERING_ANGLE, dim2_SCATTERING_ANGLE, dim1_SCATTERING_ANGLE) ;
;                SCATTERING_ANGLE:description = "Solar scattering angle." ;
;                SCATTERING_ANGLE:units = "degrees" ;
;        double VIEW_ANGLE_DERIVATIVE(dim3_VIEW_ANGLE_DERIVATIVE, dim2_VIEW_ANGLE_DERIVATIVE, dim1_VIEW_ANGLE_DERIVATIVE) ;
;                VIEW_ANGLE_DERIVATIVE:description = "Derivative of the view angle with altitude along the line of sight." ;
;                VIEW_ANGLE_DERIVATIVE:units = "degrees/km" ;
;        double ZENITH_ANGLE_DERIVATIVE(dim3_ZENITH_ANGLE_DERIVATIVE, dim2_ZENITH_ANGLE_DERIVATIVE, dim1_ZENITH_ANGLE_DERIVATIVE) ;
;                ZENITH_ANGLE_DERIVATIVE:description = "Derivative of the zenith angle with altitude along the line of sight." ;
;                ZENITH_ANGLE_DERIVATIVE:units = "degrees/km" ;
;}
;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2B CATALOG FILE:
;netcdf cips_raa_2b_orbit_52968_2017-001_v01.00_r03_cat {
;dimensions:
;        string = 46 ;
;        dim1_UT_TIME = 3600 ;
;        dim2_UT_TIME = 320 ;
;        dim1_JD_TIME = 3600 ;
;        dim2_JD_TIME = 320 ;
;        dim1_NLAYERS = 3600 ;
;        dim2_NLAYERS = 320 ;
;        \4 = 4 ;
;        dim1_LATITUDE = 3600 ;
;        dim2_LATITUDE = 320 ;
;        dim1_LONGITUDE = 3600 ;
;        dim2_LONGITUDE = 320 ;
;        dim1_ZENITH_ANGLE = 3600 ;
;        dim2_ZENITH_ANGLE = 320 ;
;        dim1_ORBIT_TRACK_X_AXIS = 3 ;
;        dim1_ORBIT_TRACK_Y_AXIS = 3 ;
;        dim1_ORBIT_TRACK_Z_AXIS = 3 ;
;variables:
;        int AIM_ORBIT_NUMBER ;
;                AIM_ORBIT_NUMBER:description = "orbit number (long) to which all data in the file applies" ;
;        char VERSION(string) ;
;                VERSION:description = "Data version number" ;
;        char REVISION(string) ;
;                REVISION:description = "Data revision number" ;
;        char PRODUCT_CREATION_TIME(string) ;
;                PRODUCT_CREATION_TIME:description = "String containing UT time at which data file was produced / Wed Feb 9 21:17:26 2011" ;
;        char UT_DATE(string) ;
;                UT_DATE:description = "UT date of orbit" ;
;                UT_DATE:units = "yyyymmdd (string)" ;
;        double UT_TIME(dim2_UT_TIME, dim1_UT_TIME) ;
;                UT_TIME:description = "UT time for each pixel. This time is an average of all scenes which observed this bin. / [XDIM,YDIM]" ;
;                UT_TIME:units = "yyyymmdd.dd" ;
;        double JD_TIME(dim2_JD_TIME, dim1_JD_TIME) ;
;                JD_TIME:description = "JD time for each pixel. This time is an average of all scenes which observed this bin. / [XDIM,YDIM]" ;
;                JD_TIME:units = "days from noon Universal Time on January 1, 4713 BC" ;
;        double ORBIT_START_TIME ;
;                ORBIT_START_TIME:description = "GPS start time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6218671e+014" ;
;                ORBIT_START_TIME:units = "microseconds" ;
;        char ORBIT_START_TIME_UT(string) ;
;                ORBIT_START_TIME_UT:description = "UT string for start time of orbit" ;
;        double ORBIT_END_TIME ;
;                ORBIT_END_TIME:description = "GPS end time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6219250e+014" ;
;                ORBIT_END_TIME:units = "microseconds" ;
;        int XDIM ;
;                XDIM:description = "Number of along-orbit-track elements in the data arrays" ;
;        int YDIM ;
;                YDIM:description = "Number of cross-orbit-track elements in the data arrays" ;
;        double NLAYERS(dim2_NLAYERS, dim1_NLAYERS) ;
;                NLAYERS:description = "Number of scenes avearged together for this pixel / [XDIM,YDIM]" ;
;        float KM_PER_PIXEL ;
;                KM_PER_PIXEL:description = "Linear dimension of square pixel occupying area of CIPS resolution element." ;
;                KM_PER_PIXEL:units = "km" ;
;        int BBOX(\4) ;
;                BBOX:description = "Bounding Box: Bottom-Left indices and size of box within the larger grid orbit track grid" ;
;        double CENTER_LON ;
;                CENTER_LON:description = "Center longitude of the orbit. Lon at peak latitude (spring/summer hem). Divides ascending and descending node." ;
;                CENTER_LON:units = "Degrees" ;
;        float LATITUDE(dim2_LATITUDE, dim1_LATITUDE) ;
;                LATITUDE:description = "Latitude of each element. Latitudes greater (less) than 90 (-90) indicate ascending node data. / [XDIM,YDIM]" ;
;                LATITUDE:units = "Degrees" ;
;        float LONGITUDE(dim2_LONGITUDE, dim1_LONGITUDE) ;
;                LONGITUDE:description = "Longitude of each element ranges from -180 to 180 / [XDIM,YDIM]" ;
;                LONGITUDE:units = "Degrees" ;
;        double ZENITH_ANGLE(dim2_ZENITH_ANGLE, dim1_ZENITH_ANGLE) ;
;                ZENITH_ANGLE:description = "Solar zenith angle (SZA) of each element. The value is specified at the altitude of the maximum contribution to the Rayleigh background. Generally around 55 km but increasing with increasing SZA. / [XDIM,YDIM]" ;
;                ZENITH_ANGLE:units = "Degrees" ;
;        double ORBIT_TRACK_X_AXIS(dim1_ORBIT_TRACK_X_AXIS) ;
;                ORBIT_TRACK_X_AXIS:description = "X axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_Y_AXIS(dim1_ORBIT_TRACK_Y_AXIS) ;
;                ORBIT_TRACK_Y_AXIS:description = "Y axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_Z_AXIS(dim1_ORBIT_TRACK_Z_AXIS) ;
;                ORBIT_TRACK_Z_AXIS:description = "Z axis of the orbit track ECEF coordinates used for binning grid." ;
;        double ORBIT_TRACK_EPOCH ;
;                ORBIT_TRACK_EPOCH:description = "GPS microsecond Time at which the orbit track axis approximately matches the true orbit track. Due to the rotation of the earth the orbit track doesn\'t trace out a plane in earth fixed coordinates." ;
;        char DATA_PRODUCT(string) ;
;                DATA_PRODUCT:description = "Brief description of the data product." ;
;        char NOTES(string) ;
;                NOTES:description = "Any additional notes. / Blank." ;
;}
;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2B ALBEDO FILE:
;netcdf cips_raa_2b_orbit_52968_2017-001_v01.00_r03_alb {
;dimensions:
;        dim1_RAYLEIGH_ALBEDO_ANOMALY = 3600 ;
;        dim2_RAYLEIGH_ALBEDO_ANOMALY = 320 ;
;        dim1_RAYLEIGH_ALBEDO_ANOMALY_UNC = 3600 ;
;        dim2_RAYLEIGH_ALBEDO_ANOMALY_UNC = 320 ;
;        dim1_RAYLEIGH_ALBEDO = 3600 ;
;        dim2_RAYLEIGH_ALBEDO = 320 ;
;variables:
;        double RAYLEIGH_ALBEDO_ANOMALY(dim2_RAYLEIGH_ALBEDO_ANOMALY, dim1_RAYLEIGH_ALBEDO_ANOMALY) ;
;                RAYLEIGH_ALBEDO_ANOMALY:description = "Albedo Anomaly: (observed_albedo-basic_state_albedo)/basic_state_albedo*100 / [XDIM,YDIM]" ;
;                RAYLEIGH_ALBEDO_ANOMALY:units = "%" ;
;        float RAYLEIGH_ALBEDO_ANOMALY_UNC(dim2_RAYLEIGH_ALBEDO_ANOMALY_UNC, dim1_RAYLEIGH_ALBEDO_ANOMALY_UNC) ;
;                RAYLEIGH_ALBEDO_ANOMALY_UNC:description = "Uncertainty in albedo anomaly due to noise./ [XDIM,YDIM]" ;
;                RAYLEIGH_ALBEDO_ANOMALY_UNC:units = "%" ;
;        double RAYLEIGH_ALBEDO(dim2_RAYLEIGH_ALBEDO, dim1_RAYLEIGH_ALBEDO) ;
;                RAYLEIGH_ALBEDO:description = "Total observed albedo./ [XDIM,YDIM]" ;
;                RAYLEIGH_ALBEDO:units = "G (10^-6 sr^-1)" ;
;}
