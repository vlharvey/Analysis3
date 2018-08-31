;
; convert CIPS Level 3c IDL save files to NetCDF
;
; VLH 20180821
;
; Input files:
; cips_3c_north_18_v05.20_r02_all.sav
; cips_3c_north_18_v05.20_r02_cld.sav
; cips_3c_north_18_v05.20_r02_nocld.sav
;
; Define Input and Output directories
;
input_path='/atmos/harvey/CIPS_data/Datfiles/Level_3c_Summary/'
output_path='/atmos/harvey/CIPS_data/Datfiles/Level_3c_Summary/'
;
; 3-dimensional CIPS 3c files to convert
;
ifiles=[$
'cips_3c_north_18_v05.20_r02_all.nc',$
'cips_3c_north_18_v05.20_r02_cld.nc',$
'cips_3c_north_18_v05.20_r02_nocld.nc']
nfile=n_elements(ifiles)
;
; loop over files
;
for ii=0L,nfile-1L do begin
;
; build output filename
;
    result=strsplit(ifiles(ii),'.',/extract)
    ncfile=output_path+result(0)+'.'+result(1)+'.nc'
;
; open NetCDF and inquire about number of dimensions and variables
;
    ncid=ncdf_open(ncfile)
    result=ncdf_inquire(ncid)   ; Inquire about the data
;
; read dimensions
;
    ndims=result.ndims
    for idim=0,ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if Execute(name + ' = dim') eq 0 then $
           Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
    endfor
;
; read variables
;
    nvars=result.nvars
    for ivar=0,nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if ( ( size( data, /n_dimensions )  EQ 1 ) && $
             ( size( data, /type ) EQ 1 ) ) then $
               data = string( data )
        if Execute(result.name + ' = data') eq 0 then $
           Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
    endfor
    NCDF_CLOSE, ncid

stop

endfor	; end loop over input files
end
