;
; this file appears to compile all necessary procedures and functions
;
restore,'read_cips_file.sav'
;
; read file
;
;fname='/aura7/harvey/CIPS_data/Datfiles/cips_sci_4_cvo_orbit_09192_2009-001_v03.20.nc'
fname='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2a_orbit_12109_2009-196_px_ascdsc_v03.20.nc'
data=read_cips_file(fname,/full_path)
;
; Compiled Procedures:
; $MAIN$
; ARRAY_CHUNK
; ARRAY_SLICE
; BBOX_LAT_LON
; CONTRACT_ARRAYS
; LEVEL_1B_LAT_LON
; LEVEL_4_LAT_LON
; PROJECT_LAMBERT
; READ_NETCDF
; RESOLVE_ALL
; SETUP_CIPS_TOOLS_VARS
; SHORTEN_ARRAY
; STR_FREE
; UPSAMPLE_1A
; 
; Compiled Functions:
; BASENAME
; CIPS_ICE
; DECOMPRESS 
; DEREFERENCE_SINGLE 
; GET_CIPS_FILENAME_BYNUM 
; GET_KMPERPIX 
; GET_LEVEL3A_STRUCTURE 
; GET_PETAL_STRUCTURE 
; GET_RETRIEVAL_STRUCTURE 
; GET_SINGLE_VAR_SAV 
; GET_STACK_STRUCTURE 
; GET_VERSION_FROM_FILENAME           
; GET_WGS84_CONST         
; INTERPOL    
; IS_SLICE_ALL_FILL       
; L1B_COMBINE_SCENES      
; L1B_COMBINE_SCENES1 
; LOGICAL_XOR         
; MAP_SIZE_LAMBERT        
; MERGE       
; MIE_PHASE   
; PATH_SEP    
; READ_BGREMOVE_FILE      
; READ_CIPS_FILE          
; READ_CIPS_FILE_BYNUM   
; READ_DAISY_FILE     
; READ_IMAGE_FILE         
; READ_PETAL_FILE         
; READ_STACK_FILE         
; STRREP      
; STRSPLIT    
; UNIQ        
; UPSAMPLE
; 
; IDL> help,/str,data
; ** Structure <284e404>, 36 tags, length=196, data length=190, refs=1:
;    AIM_ORBIT_NUMBER		INT           9192
;    VERSION			STRING    '03.20'
;    PRODUCT_CREATION_TIME	STRING    '2009/040-15:11:26'
;    DEPENDANT1BVERSION		STRING    '03.20'
;    UT_DATE			LONG           2009001
;    HEMISPHERE			STRING    'S'
;    ORBIT_START_TIME		DOUBLE       9.1483586e+14
;    ORBIT_START_TIME_UT	DOUBLE           10.102646
;    ORBIT_END_TIME		DOUBLE       9.1484165e+14
;    STACK_ID			INT              0
;    XDIM			INT            648
;    YDIM			INT            131
;    QUALITY_FLAGS		LONG                 0
;    X_TILE_DIM			INT              3
;    Y_TILE_DIM			INT              3
;    KM_PER_PIXEL		INT              5
;    BBOX			LONG      Array[4]
;    CENTER_LON			DOUBLE          -66.539687
;    NLAYERS			POINTER   <PtrHeapVar71>
;    UT_TIME			POINTER   <PtrHeapVar72>
;    CLOUD_PRESENCE_MAP		POINTER   <PtrHeapVar73>
;    COMMON_VOLUME_MAP		POINTER   <PtrHeapVar74>
;    LATITUDE			POINTER   <PtrHeapVar75>
;    LONGITUDE			POINTER   <PtrHeapVar76>
;    CLD_PHASE_ALBEDO		POINTER   <PtrHeapVar77>
;    CLD_PHASE_ALBEDO_UNC	POINTER   <PtrHeapVar78>
;    ZENITH_ANGLE_RAY_PEAK	POINTER   <PtrHeapVar79>
;    VIEW_ANGLE_RAY_PEAK	POINTER   <PtrHeapVar80>
;    SCATTERING_ANGLE		POINTER   <PtrHeapVar81>
;    CLD_ALBEDO			POINTER   <PtrHeapVar82>
;    CLD_ALBEDO_UNC		POINTER   <PtrHeapVar83>
;    ICE_WATER_CONTENT		POINTER   <PtrHeapVar84>
;    ICE_WATER_CONTENT_UNC	POINTER   <PtrHeapVar85>
;    PARTICLE_RADIUS		POINTER   <PtrHeapVar86>
;    PARTICLE_RADIUS_UNC	POINTER   <PtrHeapVar87>
; 
end
