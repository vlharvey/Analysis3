;
; Laboratory for Atmospheric and Space Physics
; University of Colorado, Boulder, Colorado, USA
; 2012
;
; FILENAME:
;   read_cips_ncdf2_raa.pro adapted from read_cips_ncdf2.pro
;
; AUTHOR:
;   Cora E. Randall
;
; DATE:    30 March 2012. Modified 13 May 2012.
;
; MODIFIED:
;   V. Lynn Harvey, 22 Feb 2017
;
; PURPOSE:
;   Code to read in CIPS RAA level 2A or level 2B data.
; 
; CALL:
;   This is intended to be run as or within an IDL program.
;   Run it from the command line: .run read_cips_ncdf2_raa
;   Must edit the "path=" and "file=" lines to define the path and filename of the cips ncdf file.
;   Modify as desired to read multiple files, etc.
;   
; DIFFERENCES BETWEEN READ_CIPS_NCDF1_RAA.PRO AND READ_CIPS_NCDF2_RAA.PRO:
;   read_cips_ncdf1_raa.pro is a function. It returns all variables in a structure. 
;   The path and filename are included in the call statement.
;    
;   read_cips_ncdf2_raa.pro is a program. It reads all variables individually into the main level.
;   The path and filename are included in the first two statements in the code.
;   
; NOTES
;    ncdump listings at the end are for the RAA L2A (scene) "cat", "alb", and "ang" files and L2B (orbit strip) "cat" and "alb" files
;    "cat" stands for catalog, "alb" stands for albedo, and "ang" stands for angle
;
; Example list of the CIPS RAA L2A and L2B files for each orbit (five total, including file sizes):
;	cips_raa_2a_orbit_52968_2017-001_v01.00_r03_alb.nc - 14 Mb
;	cips_raa_2a_orbit_52968_2017-001_v01.00_r03_ang.nc - 19 Mb
;	cips_raa_2a_orbit_52968_2017-001_v01.00_r03_cat.nc - 9.7 Mb
;	cips_raa_2b_orbit_52968_2017-001_v01.00_r03_alb.nc - 22 Mb
;	cips_raa_2b_orbit_52968_2017-001_v01.00_r03_cat.nc - 44 Mb
;
path='/atmos/harvey/CIPS_data/Datfiles/RAA/'			; Path to CIPS *.nc data file. EDIT THIS
path='/Volumes/Data/CIPS_data/Pre_process/aim/data/north_2016/level_2b/'
file='cips_raa_2b_orbit_50416_2016-198_v01.00_r03_alb.nc'
ncfile1=path+file

ncid=ncdf_open(ncfile1)			;Open the *.nc file
result=ncdf_inquire(ncid)		;Inquire about the data
nvars=result.nvars			;# variables in the file

;Read in the data
for ivar=0,nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)	;get the data name, type, dimensions

;Puts data into array called "data" and variable name into "result.name":
    ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    
; Invoke some magic to check for string data masquerading as byte data, but don't convert
; byte data blindly, i.e., quality_flags is a 2-dimensional array of byte data.
; (This is done to account for a bug in the ncdf write routine.) 
; ***Note, this is needed for PMC data (not the RAA data) but is left here to be backwards compatible 
;   if ( ( size( data, /n_dimensions )  EQ 1 ) && $
;        ( size( data, /type ) EQ 1 ) ) then $
;     data = string( data )

;Extract each variable from the "data" structure and name it the corresponding "name" from "result.name":
    if Execute(result.name + ' = data') eq 0 then $
       Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
endfor

print,' '
print, 'All variables for '+file+' are now loaded into IDL.'
print,' '

end
;
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
