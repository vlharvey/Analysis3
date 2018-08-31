;read_cips.pro
;
; Laboratory for Atmospheric and Space Physics
; University of Colorado, Boulder, Colorado, USA
; 2012
;
; FILENAME:
;   read_cips_ncdf.pro
;
; AUTHOR:
;   Cora E. Randall
;
; DATE:    30 March 2012. Modified 13 May 2012.
;
; PURPOSE:
;   Code to read in CIPS level 2 or level 3a data.
; 
; CALL:
;   This is intended to be run as or within an IDL program.
;   Run it from the command line: .run read_cips_ncdf2
;   Must edit the "path=" and "file=" lines to define the path and filename of the cips ncdf file.
;   Modify as desired to read multiple files, etc.
;   
; DIFFERENCES BETWEEN READ_CIPS_NCDF1.PRO AND READ_CIPS_NCDF2.PRO:
;   read_cips_ncdf1.pro is a function. It returns all variables in a structure. 
;   The path and filename are included in the call statement.
;    
;   read_cips_ncdf2.pro is a program. It reads all variables individually into the main level.
;   The path and filename are included in the first two statements in the code.
;   
; NOTES
;   To get level 3a longitude/latitude grid, download the IDL save file
;   from http://lasp.colorado.edu/aim/download-data-L3A.html.
;
;   Some of the strings were mistakenly written out as bytes in the ncdf files.
;   This code includes a fix for this, but it does not work properly for
;   the variable DEPENDENT2AVERSION. See data version release notes for this variable.
;

path='C:\CIPS\data\v4.20_rev05\level2\'					;Path to CIPS *.nc data file
file='cips_sci_2_orbit_01047_2007-186_v04.20_r05_cld.nc'	;CIPS *.nc data file
ncfile1=path+file

ncid=ncdf_open(ncfile1)			;Open the *.nc file
result=ncdf_inquire(ncid)		;Inquire about the data
nvars=result.nvars				;# variables in the file

;Read in the data
for ivar=0,nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)	;get the data name, type, dimensions
    ;Puts data into array called "data" and variable name into "result.name":
    ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    
    ; Invoke some magic to check for string data
    ; masquerading as byte data, but don't convert
    ; byte data blindly, i.e., quality_flags is a 2-dimensional
    ; array of byte data. 
    ; (This is done to account for a bug in the ncdf write routine.)
    if ( ( size( data, /n_dimensions )  EQ 1 ) && $
         ( size( data, /type ) EQ 1 ) ) then $
      data = string( data )

    ;Extract each variable from the "data" structure and name it
    ;  the corresponding "name" from "result.name":
    if Execute(result.name + ' = data') eq 0 then $
       Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
endfor

print,' '
print, 'All variables for '+file+' are now loaded into IDL.'
print,' '

end

;******************************************************************
;NCDUMP OUTPUT FOR LEVEL 2 CLD, LEVEL 2 CAT, AND LEVEL 3A FILES
;
;;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2 CATALOG FILE:
;netcdf cips_sci_2_orbit_22858_2011-187_v04.20_r05_cat {
;dimensions:
;	string = 24 ;
;	dim1_UT_TIME = 1930 ;
;	dim2_UT_TIME = 490 ;
;	dim1_NLAYERS = 1930 ;
;	dim2_NLAYERS = 490 ;
;	dim1_RATALL = 1930 ;
;	dim2_RATALL = 490 ;
;	dim1_QUALITY_FLAGS = 1930 ;
;	dim2_QUALITY_FLAGS = 490 ;
;	4 = 4 ;
;	dim1_LATITUDE = 1930 ;
;	dim2_LATITUDE = 490 ;
;	dim1_LONGITUDE = 1930 ;
;	dim2_LONGITUDE = 490 ;
;	dim1_ZENITH_ANGLE_RAY_PEAK = 1930 ;
;	dim2_ZENITH_ANGLE_RAY_PEAK = 490 ;
;	dim1_COMMON_VOLUME_MAP = 1930 ;
;	dim2_COMMON_VOLUME_MAP = 490 ;
;variables:
;	short AIM_ORBIT_NUMBER ;
;		AIM_ORBIT_NUMBER:description = "Integer orbit number to which all data in the file applies / 17365" ;
;	char VERSION(string) ;
;		VERSION:description = "Data version number / 4.2" ;
;	char REVISION(string) ;
;		REVISION:description = "Data revision number / 04" ;
;	char PRODUCT_CREATION_TIME(string) ;
;		PRODUCT_CREATION_TIME:description = "String containing UT time at which data file was produced / Wed Feb 9 21:17:26 2011" ;
;	char DEPENDENT_1B_VERSION(string) ;
;		DEPENDENT_1B_VERSION:description = "Version of lower level 1B data used to produce this data set / 04.20" ;
;	char UT_DATE(string) ;
;		UT_DATE:description = "UT date / 2010184" ;
;		UT_DATE:units = "yyyyddd" ;
;	float UT_TIME(dim2_UT_TIME, dim1_UT_TIME) ;
;		UT_TIME:description = "UT time for each element (fractional hour) / [1933,412], range: 10.3413-10.6638" ;
;		UT_TIME:units = "hrs of day" ;
;	char HEMISPHERE(string) ;
;		HEMISPHERE:description = "N (north) or S (south)" ;
;	double ORBIT_START_TIME ;
;		ORBIT_START_TIME:description = "GPS start time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6218671e+014" ;
;		ORBIT_START_TIME:units = "microseconds" ;
;	char ORBIT_START_TIME_UT(string) ;
;		ORBIT_START_TIME_UT:description = "GPS start time of orbit (seconds from 0000 UT on 6 Jan 1980) / 9.6218671e+008" ;
;		ORBIT_START_TIME_UT:units = "seconds" ;
;	double ORBIT_END_TIME ;
;		ORBIT_END_TIME:description = "GPS end time of orbit (microseconds from 0000 UT on 6 Jan 1980) / 9.6219250e+014" ;
;		ORBIT_END_TIME:units = "microseconds" ;
;	short STACK_ID ;
;		STACK_ID:description = "Obsolete." ;
;	int XDIM ;
;		XDIM:description = "Number of along-orbit-track elements in the data arrays / 1933" ;
;	int YDIM ;
;		YDIM:description = "Number of cross-orbit-track elements in the data arrays / 412" ;
;	short NLAYERS(dim2_NLAYERS, dim1_NLAYERS) ;
;		NLAYERS:description = "Number of observations at the location of each element each observation corresponds to a different observing geometry and thus scattering angle in the phase function / [1933,412], range: 1 to 10." ;
;	float RATALL(dim2_RATALL, dim1_RATALL) ;
;		RATALL:description = "Indicator of forward vs. backward scattering ratio [see Bailey et al., 2009] / [1933,412], range: 0 to 1.28827" ;
;	float QUALITY_FLAGS(dim2_QUALITY_FLAGS, dim1_QUALITY_FLAGS) ;
;		QUALITY_FLAGS:description = "Indicators of data quality for each element. In v4.20 the QF is determined only by NLayers as follows: NLayers > 5, QF=0. NLayers = 4 or 5, QF=1. NLayers < 4, QF=2" ;
;	float KM_PER_PIXEL ;
;		KM_PER_PIXEL:description = "Linear dimension of square pixel occupying area of CIPS resolution element." ;
;		KM_PER_PIXEL:units = "km" ;
;	int BBOX(4) ;
;		BBOX:description = "Bounding Box: Bottom-Left and Top-Right indices of the smallest rectangle which both circumscribes a set of cells on a grid and is parallel to the grid axes / [410, 1187, 2342, 1598]" ;
;	double CENTER_LON ;
;		CENTER_LON:description = "Center longitude of the orbit / -69.760521" ;
;		CENTER_LON:units = "Degrees" ;
;	float LATITUDE(dim2_LATITUDE, dim1_LATITUDE) ;
;		LATITUDE:description = "Latitude of each element. Latitudes greater (less) than 90 (-90) indicate ascending node data. / [1933,412], range: 38.8969 to 129.829" ;
;		LATITUDE:units = "Degrees" ;
;	float LONGITUDE(dim2_LONGITUDE, dim1_LONGITUDE) ;
;		LONGITUDE:description = "Longitude of each element ranges from -180 to 180 / [1933,412], range: -179.999 to 180.000" ;
;		LONGITUDE:units = "Degrees" ;
;	float ZENITH_ANGLE_RAY_PEAK(dim2_ZENITH_ANGLE_RAY_PEAK, dim1_ZENITH_ANGLE_RAY_PEAK) ;
;		ZENITH_ANGLE_RAY_PEAK:description = "Solar zenith angle (SZA) of each element. The value is specified at the altitude of the maximum contribution to the Rayleigh background. Generally around 55 km but increasing with increasing SZA. / [1933,412]
;		ZENITH_ANGLE_RAY_PEAK:units = "Degrees" ;
;	byte COMMON_VOLUME_MAP(dim2_COMMON_VOLUME_MAP, dim1_COMMON_VOLUME_MAP) ;
;		COMMON_VOLUME_MAP:description = "Indicator for whether this location is within the single \"Common Volume\" where both CIPS and SOFIE observe each orbit." ;
;		COMMON_VOLUME_MAP:Indicator 1 = "in the common volume 0 = not in the common volume. / [1933,412], range: 0 to 0." ;
;	char NOTES(string) ;
;		NOTES:description = "Any additional notes. / Blank." ;
;}
;******************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 2 CLD FILE:
;netcdf cips_sci_2_orbit_22858_2011-187_v04.20_r05_cld {
;dimensions:
;	dim1_CLOUD_PRESENCE_MAP = 1930 ;
;	dim2_CLOUD_PRESENCE_MAP = 490 ;
;	dim1_CLD_ALBEDO = 1930 ;
;	dim2_CLD_ALBEDO = 490 ;
;	dim1_CLD_ALBEDO_UNC = 1930 ;
;	dim2_CLD_ALBEDO_UNC = 490 ;
;	dim1_PARTICLE_RADIUS = 1930 ;
;	dim2_PARTICLE_RADIUS = 490 ;
;	dim1_PARTICLE_RADIUS_UNC = 1930 ;
;	dim2_PARTICLE_RADIUS_UNC = 490 ;
;	dim1_ICE_WATER_CONTENT = 1930 ;
;	dim2_ICE_WATER_CONTENT = 490 ;
;	dim1_ICE_WATER_CONTENT_UNC = 1930 ;
;	dim2_ICE_WATER_CONTENT_UNC = 490 ;
;	dim1_ICE_COLUMN_DENSITY = 1930 ;
;	dim2_ICE_COLUMN_DENSITY = 490 ;
;	dim1_CHI_SQ = 1930 ;
;	dim2_CHI_SQ = 490 ;
;variables:
;	float PERCENT_CLOUDS ;
;		PERCENT_CLOUDS:description = "Ratio (×100) of the # clouds detected (cloud_presence_map = 1) to the # locations where it was possible to detect a cloud (cld_albedo ? 1). / 53.1908" ;
;		PERCENT_CLOUDS:units = "Percent" ;
;	float CLOUD_PRESENCE_MAP(dim2_CLOUD_PRESENCE_MAP, dim1_CLOUD_PRESENCE_MAP) ;
;		CLOUD_PRESENCE_MAP:description = "Indicator for whether a cloud was detected (1) or not (0). Zero is also used as a fill value (instead of NaN this is an error and will be corrected in future versions). / [1933,412], range: 0 to 1." ;
;	float CLD_ALBEDO(dim2_CLD_ALBEDO, dim1_CLD_ALBEDO) ;
;		CLD_ALBEDO:description = "Retrieved PMC albedo, defined as the albedo that would be viewed at 90° scattering angle and 0° view angle. Zero implies no cloud was detected at this location. / [1933,412], range: 0.00 to 65.9739." ;
;		CLD_ALBEDO:units = "10-6 sr-1" ;
;	float CLD_ALBEDO_UNC(dim2_CLD_ALBEDO_UNC, dim1_CLD_ALBEDO_UNC) ;
;		CLD_ALBEDO_UNC:description = "Cloud albedo uncertainty. Not yet populated. Caution is warranted if QF>0." ;
;	float PARTICLE_RADIUS(dim2_PARTICLE_RADIUS, dim1_PARTICLE_RADIUS) ;
;		PARTICLE_RADIUS:description = "Retrieved particle mode radius, defined as the mean radius for a Gaussian distribution of particles with an axial ratio of 2 and a distribution width that varies as 0.5×radius. Zero means no cloud was detected. A
;		PARTICLE_RADIUS:units = "nm" ;
;	float PARTICLE_RADIUS_UNC(dim2_PARTICLE_RADIUS_UNC, dim1_PARTICLE_RADIUS_UNC) ;
;		PARTICLE_RADIUS_UNC:description = "Particle radius uncertainty. Currently populated with -999 or NaN. Particular caution should be used if the particle radius is less than 20 nm." ;
;	float ICE_WATER_CONTENT(dim2_ICE_WATER_CONTENT, dim1_ICE_WATER_CONTENT) ;
;		ICE_WATER_CONTENT:description = "Ice water content at each observation location. Zero means no cloud was detected. A value of -999 is reported if QF>0. / [1933,412], range: -999, 0 to 1.2×105 (0.06 to 323 for particle radius > 20 nm)." ;
;		ICE_WATER_CONTENT:units = "micro grams m^-2" ;
;	float ICE_WATER_CONTENT_UNC(dim2_ICE_WATER_CONTENT_UNC, dim1_ICE_WATER_CONTENT_UNC) ;
;		ICE_WATER_CONTENT_UNC:description = "Ice Water Content uncertainty. Currently populated with -999 or NaN. Particular caution should be used if the particle radius is less than 20 nm." ;
;	float ICE_COLUMN_DENSITY(dim2_ICE_COLUMN_DENSITY, dim1_ICE_COLUMN_DENSITY) ;
;		ICE_COLUMN_DENSITY:description = "Ice Column Density. Zero means no cloud was detected. A value of -999 is reported if QF>0. / [1933,412], range: -999, 0, 1486.82 to 4.06×1014" ;
;		ICE_COLUMN_DENSITY:units = "ice particles" ;
;	float CHI_SQ(dim2_CHI_SQ, dim1_CHI_SQ) ;
;		CHI_SQ:description = "the chi-squared value calculated from the best fit to the measured phase function used to derived cloud albedo and particle size." ;
;}

;*********************************************************************
;OUTPUT OF NCDUMP FOR LEVEL 3 FILE:
;netcdf cips_sci_3a_2008-001_v04.20_r05 {
;dimensions:
;	string = 17 ;
;	dim1_DEPENDENT2AVERSION = 15 ;
;	dim1_PETAL_START_TIME = 15 ;
;	dim1_BBOX = 4 ;
;	dim1_ORBIT_NUMBERS = 15 ;
;	dim1_QUALITY_FLAGS = 1953 ;
;	dim2_QUALITY_FLAGS = 1953 ;
;	nCols = 1953 ;
;	nRows = 1953 ;
;variables:
;	int UT_DATE ;
;		UT_DATE:description = "UTC date YYYYMMDD." ;
;	char VERSION(string) ;
;		VERSION:description = "The version number of this data product." ;
;	char PRODUCT_CREATION_TIME(string) ;
;		PRODUCT_CREATION_TIME:description = "String containing UT time at which data Product_Creation_Time file was produced / 2011/042-22:26:35" ;
;	byte DEPENDENT2AVERSION(dim1_DEPENDENT2AVERSION) ;
;		DEPENDENT2AVERSION:description = "Version of lower level 2 data used to produce this data set. One value per orbit (Each orbit forms a \"petal\" in the daisy." ;
;	char HEMISPHERE(string) ;
;		HEMISPHERE:description = "S if this data is from the southern hemisphere or N if this data is from the northern hemisphere." ;
;	float CENTER_LONGITUDE ;
;		CENTER_LONGITUDE:description = "Center longitude of map projection, NOT data. Used for orienting the data horizontally." ;
;		CENTER_LONGITUDE:units = "Degrees" ;
;	double PETAL_START_TIME(dim1_PETAL_START_TIME) ;
;		PETAL_START_TIME:description = "GPS start time of each orbit (microseconds from 0000 UT on 6 Jan 1980) / [15], range: 9.6206507e14 to 9.6214616e14" ;
;		PETAL_START_TIME:units = "microseconds" ;
;	float FIRST_IMAGE_START ;
;		FIRST_IMAGE_START:description = "GPS start time of first image in orbit (microseconds from 0000 UT on 6 Jan 1980) / [15], range: 9.6206507e14 to 9.6214616e14" ;
;		FIRST_IMAGE_START:units = "microseconds" ;
;	float KM_PER_PIXEL ;
;		KM_PER_PIXEL:description = "Linear dimension of square pixel occupying area of CIPS resolution element." ;
;		KM_PER_PIXEL:units = "km" ;
;	int BBOX(dim1_BBOX) ;
;		BBOX:description = "{x, y} Bounding Box: Bottom-Left and Top-Right indices of the smallest rectangle which both circumscribes a set of cells on a grid and is parallel to the grid axes." ;
;	short ORBIT_NUMBERS(dim1_ORBIT_NUMBERS) ;
;		ORBIT_NUMBERS:description = "An array containing the orbit number for each petal in daisy." ;
;	byte QUALITY_FLAGS(dim2_QUALITY_FLAGS, dim1_QUALITY_FLAGS) ;
;		QUALITY_FLAGS:description = "Indicator of data quality. Measurements with values equal to or less than the value of \"Quality_Flags\" are included in the daisy. In v4.20 the QF is determined only by NLayers as follows: NLayers > 5, QF=0. NLayers
;	float ALBEDO(nRows, nCols) ;
;		ALBEDO:description = "Reported albedo is a ratio of observed cloud albedo normalized by earth\'s albedo. &" ;
;		ALBEDO:units = "10^-6 sr^-1" ;
;	char DELTA_FLAT_FILE(string) ;
;		DELTA_FLAT_FILE:description = "The delta flat file used to generate this data." ;
;	float DELTA_FLAT_NORMALIZATION_PX ;
;		DELTA_FLAT_NORMALIZATION_PX:description = "The PX normalization factor used to generate this data." ;
;	float DELTA_FLAT_NORMALIZATION_PY ;
;		DELTA_FLAT_NORMALIZATION_PY:description = "The PY normalization factor used to generate this data." ;
;	float DELTA_FLAT_NORMALIZATION_MX ;
;		DELTA_FLAT_NORMALIZATION_MX:description = "The MX normalization factor used to generate this data." ;
;	float DELTA_FLAT_NORMALIZATION_MY ;
;		DELTA_FLAT_NORMALIZATION_MY:description = "The MY normalization factor used to generate this data." ;
;}

