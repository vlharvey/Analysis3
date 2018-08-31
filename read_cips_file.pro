;+  
;
; Laboratory for Atmospheric and Space Physics
; University of Colorado, Boulder, Colorado, USA
;
; FILENAME:
;   read_cips_file.pro
;
; AUTHOR:
;   Lon Riesberg 
;
; DATE:    March 5, 2007 
;
; PURPOSE:
;   Reads the data from a CIPS science netCDF file and returns the data 
;   to the user as an array of IDL structures.  This function reads files  
;   from the default cips/data directory unless a path has been provided 
;   (see full_path keyword description below)    
;  
; OUTPUT PARAMETERS:
;   attributes - if provided, will be populated with attributes from the
;      netCDF file with the provided filename.
;   status - if provided, will return the status returned from read_netcdf
;
; RETURN VALUE:
;   An array of CIPS data structures.
;
; KEYWORD ARGUMENTS:
;   /full_path - if set, indicates that the provided filename contains the
;                full path to the file. 
;   /pick_file - if set, brings up a pop up dialog that allows the user to
;                select the file from a list rather than type in the file name.
;   /by_image  - if NOT set, then all the images from a single scene will be combined
;                into a layer, and only scene layers will be returned. If set,
;                only images layers will be returned.
;   /no_upsample - if NOT set, then geolocation data will be upsampled from its storage
;                  form back to a per-pixel form. If set, geolocation data will be left
;                  in its storage form.
;
; USAGE EXAMPLE:
;	cips_data = read_cips_file(filename)   
;   cips_data = read_cips_file(path_to_file/filename, attributes=attributes, status=status, /full_path) 
;   cips_data = read_cips_file(/pick_file) 
;-

function read_cips_file, filename, attributes=attributes, status=status, no_pointers=no_pointers, full_path=full_path, pick_file=pick_file, by_image=by_image,no_upsample=no_upsample,_extra=extra

   ; init environment 
   COMMON CIPS_VARS, cips_vars_set
   if n_elements(cips_vars_set) eq 0 then begin

      ; get main directory
      pathsep = path_sep()
      path_to_here = file_dirname((routine_info('read_cips_file', /source, /functions)).path, /mark_directory) 
      main_dir_end_idx = strpos(path_to_here, pathsep + 'analysis' + pathsep + 'cips')
      main_dir =  strmid(path_to_here, 0, main_dir_end_idx)
      
      ; idl path
      !PATH = expand_path('+' + main_dir + pathsep + 'sds') + pathsep + $
              expand_path('+' + main_dir + pathsep + 'analysis' + pathsep + 'cips') + $
              pathsep + !PATH 
      
      ; remaining environment
      setup_cips_tools_vars, main_dir 
      data_path = !DATA_PRODUCT_PATH
      print, 'data path: ', data_path
   endif
   
   if keyword_set(pick_file) then begin
      filename = dialog_pickfile(filter = '*.nc*'             ,$
                                 path = data_path             ,$
                                 title = 'Locate a CIPS file' )
   endif else begin
      if ~keyword_set(full_path) then filename = data_path + filename  
   endelse

  print, 'before uncompress: ', filename 
   ; uncompress file if needed
   was_compressed = 0
   original_filename=filename
   if strmatch(filename, '*.gz') then begin
      print,'Decompressing '+filename
      filename=decompress(filename)
      was_compressed = 1
   endif else if file_test(filename + ".gz") then begin
      print,'Decompressing '+filename+".gz" 
      filename=decompress(filename+".gz")
      was_compressed = 1
   endif
   IF filename EQ -1 THEN return, -1

   net_cdf=strmatch(filename,'*.nc')

   ; determine product type   
   if strmatch(filename, '*cips_sci_1a_*', /fold_case) or strmatch(filename,'*level_1a_*',/fold_case) then product_type = '1A' else $
   if strmatch(filename, '*cips_sci_1b_*', /fold_case) or strmatch(filename,'*level_1b_*',/fold_case) then product_type = '1B' else $
   if strmatch(filename, '*cips_sci_1c_*', /fold_case) or strmatch(filename,'*level_1c_*',/fold_case) then product_type = '1C' else $
   if strmatch(filename, '*cips_sci_2a_*',  /fold_case) or strmatch(filename,'*level_2a_*',/fold_case) then product_type = '2A'  else $
   if strmatch(filename, '*cips_sci_3a_*',  /fold_case) or strmatch(filename,'*level_3a_*',/fold_case) then product_type = '3A'  else $
   if strmatch(filename, '*cips_sci_4*', /fold_case) or strmatch(filename,'*level_4_*',/fold_case) then product_type = '4' else $
   message, "invalid file type"

   ext = strmid(filename, strpos(filename,'.'))
   
   print, 'reading ', filename, '...'
   case product_type of

      '1A' : begin
               data=read_image_file(filename,net_cdf=net_cdf)                
                  
               ; upsample, if needed
               ; Notes: 
               ;   1) assumes that if the latitude field is downsampled, any other downsampled
               ;      fields are downsampled to the same degree

              if ~keyword_set(no_upsample) then upsample_1a,data
             end               
      '1B' : begin       
               data=read_stack_file(filename,net_cdf=net_cdf)
                                
               ;Combine scenes if so requested
                if ~keyword_set(by_image) then begin
                  data2=l1b_combine_scenes(data)
                  data2.common_volume_map = ptr_new(dereference_single(data.common_volume_map))
                  str_free,data
                  data=data2
                end

                ;Calculate the latitude and longitude and attach it to the structure
                junk=where(tag_names(data) eq 'HEMISPHERE',count)
                if count gt 0 then hem=data.hemisphere else hem='N'
                level_1b_lat_lon,data,lat=lat,lon=lon,south=(hem eq 'S')
                junk=where(tag_names(data) eq 'LATITUDE', count)
                if count eq 0 then begin
                  data=create_struct(data,'latitude',ptr_new(lat),'longitude',ptr_new(lon))
                end else begin
                  data.latitude=ptr_new(lat)
                  data.longitude=ptr_new(lon)
                end
             end
      '1C' : begin       
                data=read_bgremove_file(filename,net_cdf=net_cdf)
                version = get_version_from_filename(filename)
                               
                ;Combine scenes if so requested
                if ~keyword_set(by_image) then begin
                  data2=l1b_combine_scenes(data,/l1c)
                  str_free,data
                  data=data2
                end

                ;Go get the stuff from the matching 1B file
                ;check for a matching .nc file first and if not found, check for a .gz file
                matching_1b_fn=strrep(original_filename,'1c','1b',/regex)
                ext_pos = strpos(matching_1b_fn, '.nc', /reverse_search) 
                matching_1b_fn = strmid(matching_1b_fn, 0, ext_pos) + '.nc'  
                if ~file_test(matching_1b_fn) then begin 
                   matching_1b_fn = strmid(matching_1b_fn, 0, ext_pos) + '.nc.gz'  
                   if ~file_test(matching_1b_fn) then message, "Corresponding 1B File Not Found"
                endif

                ;By this point the user has already picked the file, so pick_file=0, and
                ;code above has filled out the path in the filename, so full_path=1
                matching_1b=read_cips_file(matching_1b_fn, attributes=attributes, status=status, $
                         /full_path, by_image=by_image,_extra=extra)
                data.latitude=matching_1b.latitude
                data.longitude=matching_1b.longitude
                data.common_volume_map=matching_1b.common_volume_map   & matching_1b.common_volume_map=ptr_new()
                data.scattering_angle=matching_1b.scattering_angle     & matching_1b.scattering_angle=ptr_new()
                data.common_volume_flag=matching_1b.common_volume_flag & matching_1b.common_volume_flag=ptr_new()
                if version le 3.00 then begin
                   data.view_angle=matching_1b.view_angle & matching_1b.view_angle=ptr_new()
                   data.zenith_angle=matching_1b.zenith_angle & matching_1b.zenith_angle=ptr_new()
                endif else begin
                   data.view_angle_ray_peak=matching_1b.view_angle_ray_peak & matching_1b.view_angle_ray_peak=ptr_new()
                   data.zenith_angle_ray_peak=matching_1b.zenith_angle_ray_peak & matching_1b.zenith_angle_ray_peak=ptr_new()                
                endelse
                heap_gc
             end
             
      '2A' : begin
               data=read_petal_file(filename,net_cdf=net_cdf)
            end
      '3A' : begin
               data=read_daisy_file(filename,net_cdf=net_cdf)
            end
      '4' : begin
               if strmatch(ext, '.sav') then begin
                  data = get_single_var_sav(filename)
               endif else begin
                  version = get_version_from_filename(filename)   

                  ; init retrieval structures 
                  retrieval_structure = get_retrieval_structure(version)
                  ext_retrieval_structure = get_retrieval_structure(version, /external) 
                  ozo_retrieval_structure = get_retrieval_structure(version, /ozone)

                  ; if filename is an ozone file
                  if strmatch(filename, '*_sci_4_ozo*') eq 1 then begin
                     read_netcdf, filename, ozone_data, attributes, status, structure=ozo_retrieval_structure
                     
                     ; corresponding external file?
                     ozo_idx = strpos(filename, '_sci_4_ozo')
                     ext_filename = strmid(filename, 0, ozo_idx+6) + strmid(filename, ozo_idx+10)
                     if file_test(ext_filename) then begin
                        print, 'reading ' + ext_filename
                        read_netcdf, ext_filename, external_data, attributes, status, structure=ext_retrieval_structure
                        struct_assign, external_data, retrieval_structure
                        struct_assign, ozone_data, retrieval_structure, /nozero
                        data = retrieval_structure
                     endif else begin
                        data = ozone_data
                     endelse

                  ; else filename is external
                  endif else begin
                     read_netcdf, filename, external_data, attributes, status, structure=ext_retrieval_structure
                     
                     ; corresponding ozone file?
                     l4_idx = strpos(original_filename, '_4_orbit')
                     ozo_filename = strmid(original_filename, 0, l4_idx+2) + '_ozo_' + strmid(original_filename, l4_idx+3)+"*"
                     f=file_search(ozo_filename,count=count)
                     if count gt 0 then begin
                        ozo_filename=f[0]
                        original_ozo_filename=ozo_filename
                        was_ozo_compressed=0
                        if strmatch(ozo_filename, '*.gz') then begin
                          print,'Decompressing '+ozo_filename
                          ozo_filename=decompress(ozo_filename)
                          was_ozo_compressed = 1
                        endif
                        print, 'reading ' + ozo_filename
                        read_netcdf, ozo_filename, ozone_data, attributes, status, structure=ozo_retrieval_structure
                        struct_assign, external_data, retrieval_structure
                        struct_assign, ozone_data, retrieval_structure, /nozero
                        data = retrieval_structure
                        if was_ozo_compressed then begin
                          print,'Deleting temp file '+ozo_filename
                          file_delete, ozo_filename
                        end
                     endif else begin
                        data = external_data
                     endelse 
                  endelse   ; external filename          
               endelse      ; netcdf file

               if data.version lt 3.12 then begin
                  ;Calculate the latitude and longitude and attach it to the structure
                  junk=where(tag_names(data) eq 'HEMISPHERE',count)
                  if count gt 0 then hem=data.hemisphere else hem='N'
                  level_4_lat_lon,data,lat=lat,lon=lon,south=(hem eq 'S')
                  data.latitude=ptr_new(lat)
                  data.longitude=ptr_new(lon)
               endif else if data.version ge 3.15 then begin
                  if ~ptr_valid(data.ice_water_content) then begin
                     data.ice_water_content=ptr_new(cips_ice(*data.cld_albedo,*data.particle_radius)) 
                  endif
               endif 
            end
      
   endcase 
   
   ; if original file was compressed, recompress it and delete the temp file
   if (was_compressed eq 1) then begin
      print,'Deleting temp file '+filename
      file_delete, filename
   endif   
   IF KEYWORD_SET( no_pointers ) THEN BEGIN
      pf_data = copy_structure( data, /no_pointers )
      return, pf_data 
   ENDIF ELSE BEGIN
      return, data
   ENDELSE
end
  
  
  
  
  
  
