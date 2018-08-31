;+
;
; Laboratory for Atmospheric and Space Physics
; University of Colorado, Boulder, Colorado, USA
;
; FILENAME:
;   analyze_cips_data.pro
;
; AUTHOR:
;   Lon Riesberg
;
; DATE:    April 5, 2007
;
; PURPOSE:
;   Provides an interface to the CIPS data analyzer.
;
; NOTES:
;
; USAGE EXAMPLE:  view_cips_data
;-
;


;---------------------------------------
; PROCEDURE set_orbit_list
;---------------------------------------
pro set_orbit_list

   COMMON CIPS_TOOLS_PATH, data_path
   COMMON FILES, file_list, orbit_idx, unique_orbit_list
   COMMON ORBITLISTS, orbit_list1, orbit_list2
   
   ; TODO: generalize this so version changes are easy... version numbers
   ;       should not be hard-coded here... maybe set data_path1 and data_path2?
   
   
   v_pos = strpos(data_path, 'v', /reverse_search)
   
   ; orbit list 1
   if n_elements(orbit_list1) eq 0 then begin
      tmp_path = data_path
      strput, tmp_path, '2', v_pos+1
      file_list = file_search(tmp_path +'/cips_sci_1b_*')
      uniq_file_list = file_list[UNIQ(file_list, SORT(file_list))]
        
      num_files = n_elements(uniq_file_list) 
      if num_files eq 0 then begin
         orbit_list1 = "No Data"
      endif else begin
         orbit_list1 = strarr(num_files)
         num_pos = strpos(uniq_file_list[0], 'orbit') + 6
         for file_idx=0L, num_files-1 do begin
            orbit_list1[file_idx] = strmid(uniq_file_list[file_idx], num_pos, 5)
         endfor
      endelse
   endif
   
   ; orbit list 2
   if n_elements(orbit_list2) eq 0 then begin
      tmp_path = data_path
      strput, tmp_path, '3', v_pos+1
      file_list = file_search(tmp_path +'/cips_sci_1b_*')
      uniq_file_list = file_list[UNIQ(file_list, SORT(file_list))]
      
      num_files = n_elements(uniq_file_list) 
      if num_files eq 0 then begin
         orbit_list2 = "No Data"
      endif else begin
         orbit_list2 = strarr(num_files)
         num_pos = strpos(uniq_file_list[0], 'orbit') + 6
         for file_idx=0L, num_files-1 do begin
            orbit_list2[file_idx] = strmid(uniq_file_list[file_idx], num_pos, 5)
         endfor
      endelse    
   endif  

   ; set current list to use 
   if strmid(data_path, v_pos+1, 1) eq '2' then begin
      unique_orbit_list = orbit_list1
   endif else begin
      unique_orbit_list = orbit_list2
   endelse

end


;---------------------------------------
; FUNCTION get_orbit_num_idx
;---------------------------------------
function get_orbit_num_idx, orbit_num

   COMMON FILES, file_list, orbit_idx, unique_orbit_list

   found = 0
   
   ; check for exact match
   tmp_idx = where(strmatch(unique_orbit_list, orbit_num) eq 1, n_idx)
   if n_idx eq 1 then begin
      orbit_idx = tmp_idx
      found = 1
      ;print, 'changing orbit num to ', unique_orbit_list[orbit_idx]
   endif else begin

      ; if not found, try ignoring leading zeros
      first_nonzero_digit = stregex(orbit_num, '[1-9]')
      orbit_num = strmid(orbit_num, first_nonzero_digit)
      num_orbit_list = n_elements(unique_orbit_list)
      tmp_orbit_list = strarr(num_orbit_list)
      for i=0, num_orbit_list-1 do begin
         first_nonzero_digit = stregex(unique_orbit_list[i], '[1-9]')
         orbit_list_entry = strmid(unique_orbit_list[i], first_nonzero_digit)
         if strmatch(orbit_list_entry, orbit_num) eq 1 then begin
            orbit_idx = i
            found = 1
            ;print, 'changing orbit num to ', unique_orbit_list[orbit_idx]
            break
         endif
      endfor
   endelse 
   if found eq 1 then return, orbit_idx else return, -1
end


;---------------------------------------
; PROCEDURE get_1a_images
;---------------------------------------
pro get_1a_images, l1a_files, px_data=px_data, py_data=py_data, $
                   mx_data=mx_data, my_data=my_data

   ; px
   px_idx = where(strpos(l1a_files, 'px') ne -1, num_px)
   if num_px eq 1 then begin   
      print, "Loading file: ", l1a_files[px_idx]
      px_data = read_cips_file((l1a_files[px_idx])[0], status=status, /full_path)
   endif
   
   ; mx
   mx_idx = where(strpos(l1a_files, 'mx') ne -1, num_mx)
   if num_mx eq 1 then begin   
      print, "Loading file: ", l1a_files[mx_idx]
      mx_data = read_cips_file((l1a_files[mx_idx])[0], status=status, /full_path)
   endif  
   
   ; py
   py_idx = where(strpos(l1a_files, 'py') ne -1, num_py)
   if num_py eq 1 then begin   
      print, "Loading file: ", l1a_files[py_idx]
      py_data = read_cips_file((l1a_files[py_idx])[0], status=status, /full_path)
   endif 

   ; my
   my_idx = where(strpos(l1a_files, 'my') ne -1, num_my)
   if num_my eq 1 then begin   
      print, "Loading file: ", l1a_files[my_idx]
      my_data = read_cips_file((l1a_files[my_idx])[0], status=status, /full_path)
   endif   

   return
end


;---------------------------------------
; PROCEDURE VIEW_CIPS_DATA_EVENT
;---------------------------------------
pro view_cips_data_event, event

   COMMON CIPS_TOOLS_PATH, data_path
   COMMON ANALYZER_REF, analyzer
   COMMON FILES, file_list, orbit_idx, unique_orbit_list

   WIDGET_CONTROL, event.id, get_uvalue=uvalue
   WIDGET_CONTROL, event.handler, get_uvalue=base_uvalue

   status = 0

   CASE STRUPCASE(WIDGET_INFO(event.id, /UNAME)) OF
     
     ; --- Select Orbit tab ---
     'NEWDATAPATH': BEGIN
        WIDGET_CONTROL, base_uvalue.path_box, get_value=curr_path

        ; if path has NOT been manually edited, popup file picker
        if (curr_path eq data_path) then begin
           file = dialog_pickfile(dialog_parent = event.top    ,$
                                  filter = '*.nc*'             ,$
                                  path = data_path             ,$
                                  get_path = new_path          ,$
                                  title = 'Locate CIPS file to change path' )

           ; check for valid CIPS directory
           junk=file_search(new_path +'/cips_*.nc', count=cips_file_count)
           if (file eq '') then  return

           if (cips_file_count le 0) then begin
              void = dialog_message('No CIPS files found in selected directory', /center)
              return
           endif

           ; set new data path
           data_path = new_path
           WIDGET_CONTROL, base_uvalue.path_box, SET_VALUE=data_path

           ; reset orbit list and put orbit index into orbit text box
           set_orbit_list
           num_pos = strpos(file, 'orbit') + 6
           new_orbit = strmid(file, num_pos, 5)
           new_orbit_idx = where(unique_orbit_list eq new_orbit, n_files)
           if n_files eq 1 then orbit_idx=new_orbit_idx else orbit_idx=0
           WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]

        ; else if path has been edited manually, check for validity
        endif else begin
           junk=file_search(curr_path +'cips_sci_*.nc', count=cips_file_count)
           if (cips_file_count gt 0) then begin
              data_path = curr_path
              set_orbit_list
              orbit_idx = 0
              WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
           endif else void = dialog_message('No CIPS files found in selected directory', /center)
           WIDGET_CONTROL, base_uvalue.path_box, SET_VALUE=data_path
        endelse
      END
      
     'VERSION': BEGIN
        ver2 = widget_info(base_uvalue.ver2_btn, /BUTTON_SET)
        v_pos = strpos(data_path, 'v', /reverse_search)
        
        ; TODO: verify that new path exists before strput
        
        ; TODO: generalize version calls so new versions are easy to add
        
        ; TODO: add popup to indicate that orbit list is being updated
        ;       then close that popup when update is complete
        
        if ver2 eq 1 then begin
           strput, data_path, '2', v_pos+1
        endif else begin
           strput, data_path, '3', v_pos+1
        endelse
         
        ; reset orbit list 
        set_orbit_list
        orbit_idx = n_elements(unique_orbit_list) - 1
        if orbit_idx ge 0 then WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
        
        ; reset displayed data path 
        WIDGET_CONTROL, base_uvalue.path_box, SET_VALUE=data_path
           
      END

     'OFIRST': BEGIN
        orbit_idx = 0
        WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
      END

     'OLAST': BEGIN
        orbit_idx = n_elements(unique_orbit_list) - 1
        WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
      END

     'OPREV': BEGIN
        orbit_idx--
        WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
      END

     'ONEXT': BEGIN
        orbit_idx++
        WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
      END

     'ORBITNUM': BEGIN
        WIDGET_CONTROL, base_uvalue.orbit_txt, get_value=input
        tmp_idx = get_orbit_num_idx(input)
        if tmp_idx gt -1 then orbit_idx = tmp_idx
        WIDGET_CONTROL, base_uvalue.orbit_txt, SET_VALUE=unique_orbit_list[orbit_idx]
      END

     'GO1': BEGIN
        level_1a = widget_info(base_uvalue.level_1a_btn, /BUTTON_SET)
        level_1b = widget_info(base_uvalue.level_1b_btn, /BUTTON_SET)
        level_1c = widget_info(base_uvalue.level_1c_btn, /BUTTON_SET)
        retrieved = widget_info(base_uvalue.retrieved_btn, /BUTTON_SET)
        retrieved_test = widget_info(base_uvalue.retrieved_test_btn, /BUTTON_SET)

        ; no product selected
        if (level_1a eq 0) and (level_1b eq 0) and (retrieved eq 0) and $
        (retrieved_test eq 0) and (level_1c eq 0) then begin
           void = dialog_message('Please Select Data Type', /center)
           return
        endif

        if (analyzer->has_valid_itool_handle() eq 1) then begin
           message, 'ONLY ONE ANALYZER TOOL CAN EXIST PER IDL SESSION!', /info
           return
        endif
        
        ; if previously open, reset analyzer 
        if analyzer->has_data() eq 1 then analyzer->reset
        
        ; check that the user didn't manually enter an orbit with no matching data
        WIDGET_CONTROL, base_uvalue.orbit_txt, get_value=input
        first_nonzero_digit = stregex(unique_orbit_list[orbit_idx], '[1-9]')
        no_leading_zeros_orbit_num = strmid(unique_orbit_list[orbit_idx], first_nonzero_digit)
        if (strcmp(no_leading_zeros_orbit_num, input) eq 0) then begin
           tmp_idx = get_orbit_num_idx(input)
           if tmp_idx gt -1 then orbit_idx = tmp_idx else begin
              void = dialog_message('Data Not Found', /center)
              return
           endelse
        endif
        
        ; get data
        orbit_num = unique_orbit_list[orbit_idx]

        if level_1a eq 1 then begin
           search_string = 'cips_sci_1a_orbit_' + orbit_num + '_*cam_*'
           l1a_files = file_search(data_path + search_string)        
           
           ; NOTE: this assumes that there could only be a prelim and a definitive file
           ;       for each camera for a total of 8 possible matching files.  definitive
           ;       files are prioritized over prelim... if files of multiple versions are 
           ;       to be stored in the same directories, this will need to be updated...
        
            
           ; px
           px_idx = where(strpos(l1a_files, 'px') ne -1, num_px)
           if num_px gt 0 then begin
              definitive_px_idx = where(strpos(l1a_files[px_idx], 'prelim') eq -1, num_px_definitive)
              if num_px_definitive eq 1 then px_idx=px_idx[definitive_px_idx]
              best_1a_files = l1a_files[px_idx]
           endif else message, 'PX FILE NOT FOUND', /info           

           ; mx
           mx_idx = where(strpos(l1a_files, 'mx') ne -1, num_mx)
           if num_mx gt 0 then begin
              definitive_mx_idx = where(strpos(l1a_files[mx_idx], 'prelim') eq -1, num_mx_definitive)
              if num_mx_definitive eq 1 then mx_idx=mx_idx[definitive_mx_idx]
              if n_elements(best_1a_files) eq 0 then best_1a_files = l1a_files[mx_idx] $
              else best_1a_files = [ best_1a_files, l1a_files[mx_idx] ]   
           endif else message, 'MX FILE NOT FOUND', /info   
              
           ; py
           py_idx = where(strpos(l1a_files, 'py') ne -1, num_py)
           if num_py gt 0 then begin
              definitive_py_idx = where(strpos(l1a_files[py_idx], 'prelim') eq -1, num_py_definitive)
              if num_py_definitive eq 1 then py_idx=py_idx[definitive_py_idx]
              if n_elements(best_1a_files) eq 0 then best_1a_files = l1a_files[py_idx] $
              else best_1a_files = [ best_1a_files, l1a_files[py_idx] ] 
           endif else message, 'PY FILE NOT FOUND', /info  
                         
           ; my
           my_idx = where(strpos(l1a_files, 'my') ne -1, num_my)
           if num_my gt 0 then begin
              definitive_my_idx = where(strpos(l1a_files[my_idx], 'prelim') eq -1, num_my_definitive)
              if num_my_definitive eq 1 then my_idx=my_idx[definitive_my_idx]
              if n_elements(best_1a_files) eq 0 then best_1a_files = l1a_files[my_idx] $
              else best_1a_files = [ best_1a_files, l1a_files[my_idx] ] 
           endif else message, 'MY FILE NOT FOUND', /info 
           
           get_1a_images, best_1a_files, px_data=px_data, py_data=py_data, $
                          mx_data=mx_data, my_data=my_data
        endif

        if level_1b eq 1 then begin
           search_string = 'cips_sci_1b_orbit_' + orbit_num + '*'
           l1b_file = file_search(data_path + search_string)
           
           ; prioritize any definitive files over prelim files
           ; NOTE: this assumes that each orbit only has two possibilities: prelim and definitive...
           ;       if multiple versions were to be included in the same directory, this would need
           ;       need to be modified
           definitive_idx = where(strpos(l1b_file, 'prelim') eq -1, num_definitive)
           if (num_definitive ge 1) then begin
              l1b_file = l1b_file[definitive_idx]
           endif

           if (n_elements(l1b_file) eq 1) and (l1b_file ne '') then begin
              print, "Loading file: ", l1b_file[0]
              stack_data = read_cips_file(l1b_file[0], status=status, /full_path)
              if (status ne 0) then message, 'read_cips_file returned with status ' + string(status, format='(i0)'), /info
           endif else if (l1b_file eq '') then print, 'Matching Level 1B file not found' $
           else if n_elements(l1b_file) gt 1 then print, 'Multiple Level 1B files found'
        endif

        if level_1c eq 1 then begin
           search_string = 'cips_sci_1c_orbit_' + orbit_num + '*'
           l1c_file = file_search(data_path + search_string)
           
           ; prioritize any definitive files over prelim files
           ; NOTE: this assumes that each orbit only has two possibilities: prelim and definitive...
           ;       if multiple versions were to be included in the same directory, this would need
           ;       need to be modified
           definitive_idx = where(strpos(l1c_file, 'prelim') eq -1, num_definitive)
           if (num_definitive ge 1) then begin
              l1c_file = l1c_file[definitive_idx]
           endif

           if (n_elements(l1c_file) eq 1) and (l1c_file ne '') then begin
              print, "Loading file: ", l1c_file[0]
              l1c_data = read_cips_file(l1c_file[0], status=status, /full_path)
              if (status ne 0) then message, 'read_cips_file returned with status ' + string(status, format='(i0)'), /info
           endif else if (l1c_file eq '') then print, 'Matching Level 1C file not found' $
           else if n_elements(l1c_file) gt 1 then print, 'Multiple Level 1C files found'
        endif

        if retrieved eq 1 then begin
           search_string = 'cips_sci_4_orbit_' + orbit_num + '*_v*'
           l4_file = file_search(data_path + search_string)
        endif else if retrieved_test eq 1 then begin
           search_string = 'cips_sci_4_orbit_' + orbit_num + '*_t*'
           l4_file = file_search(data_path + search_string)   
        endif        
           
        if n_elements(l4_file) ge 1 then begin
        
           ; prioritize any definitive files over prelim files
           ; Note: this grabs the first matching file which works fine as long
           ;       as there aren't multiple versions of each file in the current
           ;       version's directory
           definitive_idx = where(strpos(l4_file, 'prelim') eq -1, num_definitive)
           if (num_definitive ge 1) then begin
              l4_file = l4_file[definitive_idx]
           endif  
           
           if (n_elements(l4_file) ge 1) and (l4_file[0] ne '') then begin
              print, "Loading file: ", l4_file[0]
              properties_data = read_cips_file(l4_file[0], status=status, /full_path)
              if (status ne 0) then message, 'read_cips_file returned with status ' + string(status, format='(i0)'), /info
           endif else if (l4_file[0] eq '') then print, 'Matching Level 4 file not found' $
           else if n_elements(l4_file) gt 1 then begin
              message, 'Multiple Level 4 files found', /info
              print, l4_file
           endif
        endif
        
        ; load analyzer
        if (n_elements(px_data) gt 0) or (n_elements(py_data) gt 0) or (n_elements(mx_data) gt 0) $
           or (n_elements(my_data) gt 0) or (n_elements(stack_data) gt 0) or (n_elements(l1c_data) gt 0) or $
           (n_elements(properties_data) gt 0) then begin
              
              print, 'Loading analyzer...'
              title='AIM Orbit ' + string(orbit_num, format='(i0)')
              status=analyzer->analyze_data(cam_mx=mx_data, cam_px=px_data, cam_my=my_data, cam_py=py_data, $
                                            stack=stack_data, l1c=l1c_data, sim_retrievals=properties_data, $
                                            title=title)
              
              ; update the orbit list in case processing is currently underway   
              print, 'updating orbit list...'                           
        endif else begin
           ; no matching files found
           void = dialog_message('No Matching Files Found', /center)
           return
        endelse

     END

     ; Select File tab
     'BROWSE': BEGIN
        file = dialog_pickfile(dialog_parent = event.top    ,$
                               filter = '*.nc*'             ,$
                               path = data_path             ,$
                               title = 'Locate a CIPS file' )
        file = file[0]
        if (file eq '') then begin
           widget_control, base_uvalue.go_btn2, set_uvalue={filename:''}
           return;
        endif

        ; check that file was selected
        is_it_nc = STRPOS(file, '.nc')

        if (is_it_nc eq -1) then begin
           void = dialog_message('Improper file selected', /center)
           return
        endif

        WIDGET_CONTROL, base_uvalue.filename_box, SET_VALUE=file

        ; Attach the filename to the 'Go' button
        widget_control, base_uvalue.go_btn2, set_uvalue={filename:file}
      END

     'GO2': BEGIN
        WIDGET_CONTROL, base_uvalue.go_btn2, get_uvalue=go_uvalue
        if (go_uvalue.filename eq '') then begin
           void = dialog_message('  No file selected  ', /center)
           return
        endif

        if (analyzer->has_valid_itool_handle() eq 1) then begin
           message, 'ONLY ONE ANALYZER TOOL CAN EXIST PER IDL SESSION!', /info
           return
        endif
        
        ; if previously open, reset analyzer 
        if analyzer->has_data() eq 1 then analyzer->reset
        
        filename_base = file_basename(go_uvalue.filename)
        file_path = file_dirname(go_uvalue.filename, /mark_directory)
        
        ; analyzer title
        num_pos = strpos(filename_base, 'orbit') + 6
        orbit_number = strmid(filename_base, num_pos, 5)
        title='AIM Orbit ' + string(orbit_number, format='(i0)')
        
        ; 1A
        if strmatch(filename_base, '*cips_sci_1a_*', /fold_case) then begin
            
           ; NOTE: this will select files that correspond to the user's 
           ;       selection.  no other preference is given for definitive
           ;       or prelim files.
            
           ; px
           strput, filename_base, 'px', 37
           px_file = file_path + filename_base
           if (file_test(px_file) eq 1) then l1a_files = px_file 
        
           ; mx
           strput, filename_base, 'mx', 37
           mx_file = file_path + filename_base
           if (file_test(mx_file) eq 1) then begin
              if n_elements(l1a_files) eq 0 then l1a_files = mx_file $
              else l1a_files = [ l1a_files, mx_file ] 
           endif           
        
           ; py
           strput, filename_base, 'py', 37
           py_file = file_path + filename_base
           if (file_test(py_file) eq 1) then begin
              if n_elements(l1a_files) eq 0 then l1a_files = py_file $
              else l1a_files = [ l1a_files, py_file ] 
           endif     
        
           ; my
           strput, filename_base, 'my', 37
           my_file = file_path + filename_base
           if (file_test(my_file) eq 1) then begin
              if n_elements(l1a_files) eq 0 then l1a_files = my_file $
              else l1a_files = [ l1a_files, my_file ] 
           endif     

           get_1a_images, l1a_files, px_data=px_data, py_data=py_data, mx_data=mx_data, my_data=my_data
           status=analyzer->analyze_data(cam_mx=mx_data, cam_px=px_data, cam_my=my_data, cam_py=py_data, title=title)

        ; 1B
        endif else if strmatch(filename_base, '*cips_sci_1b_*', /fold_case) then begin

           print, "Loading file: ", file_path + filename_base
           stack_data = read_cips_file(file_path + filename_base, /full_path)
           status=analyzer->analyze_data(stack=stack_data, title=title)

        ; 4
        endif else if strmatch(filename_base, '*cips_sci_4*', /fold_case) then begin

           print, "Loading file: ", file_path + filename_base
           properties_data = read_cips_file(file_path + filename_base, /full_path)
           status=analyzer->analyze_data(sim_retrievals=properties_data, title=title)

        endif else begin
           void = dialog_message('  Unrecognized File Type ', /center)
        endelse

     END

     else: BEGIN
     ENDELSE
   ENDCASE

   ; Desensitize the first/previous buttons
   if (orbit_idx eq 0) then begin
      WIDGET_CONTROL, base_uvalue.o_first, SENSITIVE=0
      WIDGET_CONTROL, base_uvalue.o_prev, SENSITIVE=0
   endif else begin
      WIDGET_CONTROL, base_uvalue.o_first, SENSITIVE=1
      WIDGET_CONTROL, base_uvalue.o_prev, SENSITIVE=1
   endelse

   ; Desensitize the next/last buttons
   if orbit_idx eq (n_elements(unique_orbit_list)-1) then begin
      WIDGET_CONTROL, base_uvalue.o_last, SENSITIVE=0
      WIDGET_CONTROL, base_uvalue.o_next, SENSITIVE=0
   endif else begin
      WIDGET_CONTROL, base_uvalue.o_last, SENSITIVE=1
      WIDGET_CONTROL, base_uvalue.o_next, SENSITIVE=1
   endelse

end


;---------------------------------------
; PROCEDURE clean
;---------------------------------------
pro clean, base
   COMMON ANALYZER_REF, analyzer
   if analyzer->has_data() eq 1 then analyzer->reset    
   return
end


;---------------------------------------
; FUNCTION VIEW_CIPS_DATA
;---------------------------------------
; PURPOSE:
;   This function creates a simple user interface to the CIPS data analyzer.
;
pro view_cips_data

   COMMON ANALYZER_REF, analyzer
   COMMON FILES, file_list, orbit_idx, unique_orbit_list

   ; init environment 
   COMMON CIPS_TOOLS_PATH, data_path
   COMMON CIPS_VARS, cips_vars_set
   if n_elements(cips_vars_set) eq 0 then begin

      ; get main directory
      path_to_here = file_dirname((routine_info('view_cips_data', /source)).path, /mark_directory) 
      main_dir_end_idx = strpos(path_to_here, '/analysis/cips')
      main_dir =  strmid(path_to_here, 0, main_dir_end_idx)
      
      ; idl path
      pathsep = path_sep(/search_path)  
      !PATH = expand_path('+' + main_dir + '/sds') + pathsep + expand_path('+' + main_dir + '/analysis/cips') + $
              pathsep + !PATH + pathsep + expand_path('+ ' + '~aimsds/misc')
      
      ; remaining environment
      setup_cips_tools_vars, main_dir 
      data_path = !data_product_path
      cips_vars_set = 1
   endif

   set_debug_level, 0

   analyzer = obj_new('cips_data_analyzer')
   base = widget_base(TITLE='VIEW CIPS DATA', /BASE_ALIGN_CENTER, $
                      /COLUMN, TLB_FRAME_ATTR=1, UNAME='MAINBASE')  
   WIDGET_CONTROL, base, kill_notify='clean'
   tab = widget_tab(base)

   ;
   ; SELECT ORBIT TAB
   ;
   ; determine which orbits have data files in the data_path directory
   set_orbit_list

   ; setup product selection
   select_base = widget_base(tab, /COLUMN, /BASE_ALIGN_CENTER, TITLE=' Select Orbit ')
   space = widget_base(select_base, ysize=10)

   ; editable path box
   path_base = widget_base(select_base, /FRAME, /COLUMN, /BASE_ALIGN_CENTER)
   space = widget_label(path_base, value='') & space = widget_label(path_base, value='')
   label = widget_label(path_base, value='Path to Data:')
   space = widget_base(path_base, ysize=5)
   path_txt = widget_text(path_base, value=data_path, xsize=100, ysize=1, $
               /ALL_EVENTS, /EDITABLE, /SCROLL, UVALUE=data_path, UNAME='DATAPATH')
   space = widget_label(path_base, value='')
   diff_path_btn = widget_button(path_base, value='  Change Data Path  ', UNAME='NEWDATAPATH')
   space = widget_base(path_base, ysize=10)
   
   ; version
   version_base = widget_base(select_base, /COLUMN, /FRAME, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   space = widget_base(version_base, ysize=10)
   version_sel_base = widget_base(version_base, /ROW, /EXCLUSIVE)
   ver2_btn = widget_button(version_sel_base, value='Version 2     ', UVALUE={none:0}, UNAME='VERSION')
   ver3_btn = widget_button(version_sel_base, value='Version 3     ', UVALUE={none:0}, UNAME='VERSION')
   space = widget_base(version_base, ysize=10)

   ; product selection
   prod_base = widget_base(select_base, /COLUMN, /FRAME, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   space = widget_label(prod_base, value='') & space = widget_label(prod_base, value='')
   label = widget_label(prod_base, value='Select Data Type (multiple selections OK):')
   space = widget_base(prod_base, ysize=5)
   prod_type_base = widget_base(prod_base, /COLUMN, /NONEXCLUSIVE)
   level_1a_btn = widget_button(prod_type_base, value='Level 1A: Scenes', UVALUE={none:0}, UNAME='LEVEL1A')
   level_1b_btn = widget_button(prod_type_base, value='Level 1B: Stack Layers', UVALUE={none:0}, UNAME='LEVEL1B')
   level_1c_btn = widget_button(prod_type_base, value='Level 1C: Stack Layers with Background Removed', UVALUE={none:0}, UNAME='LEVEL1C')
   retrieved_base = widget_base(prod_base, /ROW)
   label = widget_label(retrieved_base, value='Cloud Properties:')
   retrieved_btn_base = widget_base(retrieved_base, /ROW, /EXCLUSIVE)
   retrieved_btn = widget_button(retrieved_btn_base, value='Production', UVALUE={none:0}, UNAME='RETRIEVED')
   retrieved_test_btn = widget_button(retrieved_btn_base, value='Test', UVALUE={none:0}, UNAME='RETRIEVEDTEST')
   no_retrieved_btn = widget_button(retrieved_btn_base, value='None', UVALUE={none:0}, UNAME='RETRIEVEDNONE')
   space = widget_label(prod_base, value='')

   ; setup orbit selection
   orbit_idx = n_elements(unique_orbit_list)-1
   orbit_base = widget_base(select_base, /COLUMN, /FRAME, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   space = widget_label(orbit_base, value='') & space = widget_label(orbit_base, value='')
   label = widget_label(orbit_base, value='Orbit Number: ')
   orbit_slider_base = widget_base(orbit_base, /ROW)
   o_first = widget_button(orbit_slider_base, value=' << ', UVALUE={nothing:0}, UNAME='OFIRST')
   o_prev = widget_button(orbit_slider_base, value=' < ', UVALUE={nothing:0}, UNAME='OPREV')
   orbit_txt = widget_text(orbit_slider_base, value=unique_orbit_list[orbit_idx], xsize=7, $
               /WRAP, /EDITABLE, UVALUE={nothing:0}, UNAME='ORBITNUM')
   space = widget_base(orbit_base, ysize=20)
   o_next = widget_button(orbit_slider_base, value=' > ', UVALUE={nothing:0}, UNAME='ONEXT')
   o_last = widget_button(orbit_slider_base, value=' >> ', UVALUE={nothing:0}, UNAME='OLAST')

   ; go1
   go_base = widget_base(select_base, /COLUMN, /FRAME, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   label = widget_label(go_base, value=' ')
   if strpos(data_path, 'v2') ne -1 then WIDGET_CONTROL, ver2_btn, /SET_BUTTON $
   else if strpos(data_path, 'v3') ne -1 then WIDGET_CONTROL, ver3_btn, /SET_BUTTON
   space = widget_base(go_base, ysize=10)
   go_btn1 = widget_button(go_base, value='   View Data -->   ', UNAME='GO1')
   space = widget_base(go_base, ysize=20)

   if n_elements(unique_orbit_list) eq 1 then begin  
      WIDGET_CONTROL, o_first, SENSITIVE=0 
      WIDGET_CONTROL, o_prev, SENSITIVE=0
      WIDGET_CONTROL, o_last, SENSITIVE=0 
      WIDGET_CONTROL, o_next, SENSITIVE=0      
   endif else if orbit_idx eq (n_elements(unique_orbit_list)-1)  then begin
      WIDGET_CONTROL, o_first, SENSITIVE=1 
      WIDGET_CONTROL, o_prev, SENSITIVE=1
      WIDGET_CONTROL, o_last, SENSITIVE=0 
      WIDGET_CONTROL, o_next, SENSITIVE=0  
   endif else if orbit_idx eq 0 then begin
      WIDGET_CONTROL, o_first, SENSITIVE=0 
      WIDGET_CONTROL, o_prev, SENSITIVE=0
      WIDGET_CONTROL, o_last, SENSITIVE=1 
      WIDGET_CONTROL, o_next, SENSITIVE=1   
   endif

   ;
   ; SELECT FILE TAB
   ;
   browse_file_base = widget_base(tab, /COLUMN, TITLE=' Select File ', /BASE_ALIGN_CENTER)
   space = widget_base(browse_file_base, ysize=10)

   file_base = widget_base(browse_file_base, /COLUMN, /FRAME, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   space = widget_label(file_base, value='') & space = widget_label(file_base, value='')
   browse_btn = widget_button(file_base, value='   Find File...   ', UNAME='BROWSE')
   ;space = widget_base(file_base, ysize=10)
   filename_box = widget_text(file_base, /SCROLL, UNAME='FILEBOX', /ALL_EVENTS, xsize=100, ysize=1)

   go2_base =  widget_base(browse_file_base, /COLUMN, /BASE_ALIGN_CENTER, xsize=(WIDGET_INFO(path_base,/geom,units=0)).xsize)
   space = widget_base(go2_base, ysize=20)
   go_btn2 = widget_button(go2_base, value='   View Data -->   ', UVALUE={filename:''}, UNAME='GO2')
   space = widget_base(go2_base, ysize=20)


   WIDGET_CONTROL, base, SET_UVALUE={ go_btn1: go_btn1, $
                                      go_btn2: go_btn2, $
                                      path_box:path_txt, $
                                      level_1a_btn:level_1a_btn, $
                                      level_1b_btn:level_1b_btn, $
                                      level_1c_btn:level_1c_btn, $
                                      retrieved_btn: retrieved_btn, $
                                      retrieved_test_btn:retrieved_test_btn, $
                                      ver2_btn:ver2_btn, $
                                      ver3_btn:ver3_btn, $
                                      orbit_txt:orbit_txt, $
                                      o_first:o_first, $
                                      o_prev:o_prev, $
                                      o_next:o_next, $
                                      o_last:o_last, $
                                      filename_box: filename_box}
   WIDGET_CONTROL, base, /REALIZE
 
   XMANAGER, 'view_cips_data', base

end
