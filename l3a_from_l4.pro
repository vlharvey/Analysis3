pro l3a_from_l4,day_number
   set_cips_production_vars
   
   ;1) Pick a day
   ;   Get the orbit infos for all the orbits in that day
   orbit_info=get_orbit_info(1d6*yd2gps(day_number),1d6*yd2gps(day_number+1))
   print,orbit_info[*].orbit_number
   num_orbits = n_elements(orbit_info)
   message, "Number of orbits found within time range: " + string(num_orbits, format='(i0)'), /info
   
   petal_start_time=dblarr(n_elements(orbit_info))
   petal_version=bytarr(n_elements(orbit_info))  
   daisy_map=ptrarr(2)
   map_size=map_size_lambert(kmperpix=15)
   daisy_map[0]=ptr_new(make_array(map_size,map_size,/double,value=!values.f_nan))
   bboxes=intarr(2,4)
   bboxes[0,*]=[0,0,map_size-1,map_size-1]
   bboxes[1,*]=[0,0,map_size-1,map_size-1]
   daisy_map[1]=ptr_new(make_array(map_size,map_size,/double,value=!values.f_nan))
      
   ; for each orbit, get the level 4 images and merge them into a daisy
   orbit_numbers = orbit_info[*].orbit_number
   for orbit_index=0, num_orbits-1 do begin

      message, string(orbit_info[orbit_index].orbit_number, orbit_info[orbit_index].start_time, usec2vms(orbit_info[orbit_index].start_time),format='(%"Orbit %05d start time: %17.0f (%s)")'), /info

      ; retrieve the retrieved cloud images for the specified time
      if !version.os eq "linux" then begin
         file=file_search(string(format='(%"/aim/data/cips/v2/cips_sci_4_orbit_%05d_*.nc")',orbit_info[orbit_index].orbit_number))
	 if file[0] eq "" then begin
	    good=0
	 end else begin
            level_4=read_cips_file(/f,/b,file[0])
            good=1
	 end
      endif else begin
         file=file_search(string(format='(%"I:\\data\\cips\\v2\\cips_sci_4_orbit_%05d_*.nc")',orbit_info[orbit_index].orbit_number))
         if file[0] eq "" then begin
	    good=0
	 end else begin
	    level_4=read_cips_file(/f,/b,file[0])
	    good=1
	 end
      end
      
      if good eq 1 then begin
         hem=level_4.hemisphere
         south=hem eq 'S'
         l4_bbox=level_4.bbox/3
	 print,l4_bbox
	 xmin=l4_bbox[0]
	 ymin=l4_bbox[1]
	 l4_center_lon=level_4.center_lon
         albedo=*level_4.cld_albedo
	 petal_start_time[orbit_index]=level_4.orbit_start_time
         petal_version[orbit_index]=level_4.version
         str_free,level_4
	   
         s=size(albedo,/dimensions)
	 print,s
	 xmax=xmin+s[0]-1
	 ymax=ymin+s[1]-1
	 print,ymax
	 if xmin lt 0 then xmin=0
	 if ymin lt 0 then ymin=0
	 if xmax ge map_size then xmax=map_size-1
	 if ymax ge map_size then ymax=map_size-1
	 print,ymax
	 print,map_size
	 xspan=xmax-xmin+1
     ;; USED MY FREE NEGATIVE SIGN HERE ;;  	 
	 yspan=ymax-ymin+1
	 (*daisy_map[1])[xmin:xmax,ymin:ymax]=albedo[0:xspan-1,0:yspan-1]
         rot_amount=-l4_center_lon
         if south then rot_amount=180-rot_amount
	 (*daisy_map[1])=rot(*daisy_map[1],rot_amount,missing=!values.f_nan)
	 result=merge(daisy_map,bboxes,/brightest)
	 str_free,daisy_map[0]
	 *(daisy_map[1]) *= !values.f_nan
	 daisy_map[0]=ptr_new(result)
         result=0   
      end
   endfor
   
   ptr_free,daisy_map[1]

   ;6) Make a daisy
   print,"% Creating Daisy"
   result=*daisy_map[0]
   str_free,daisy_map
      
   bbox=findbbox(finite(result))
   result=result[bbox[0]:bbox[2]-1,bbox[1]:bbox[3]-1]

   ; define return structure
   level_3a_daisy = get_level3a_structure()
      
   level_3a_daisy.petal_start_time = ptr_new(orbit_start_time)
   level_3a_daisy.first_image_start = petal_start_time[0]
   level_3a_daisy.version=lookup_version('Level3A')
   ymd=jd2ymd(usec2jd(petal_start_time[0]))
   ut_date=ymd[0]*10000L+ymd[1]*100L+ymd[2]
   level_3a_daisy.hemisphere=hem
   level_3a_daisy.ut_date=long(ut_date)
   level_3a_daisy.orbit_numbers = ptr_new(orbit_numbers)
   level_3a_daisy.albedo = ptr_new(result)
   level_3a_daisy.bbox=bbox
      
   ; write daisy to file
   ;self->write_cips_file, level_3a_daisy, fn, data_level='3a', net_cdf=net_cdf

   heap_gc
   help,/mem
   tvscl,*level_3a_daisy.albedo,/nan
   ;draw_daisy,level_3a_daisy,/west_ct,png_fn
end
