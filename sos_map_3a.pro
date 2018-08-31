pro sos_map_3a,day,day0,res=res,asc=asc,dsc=dsc,px=px,south=south

  if keyword_set(asc) then suffix='_asc'
  if keyword_set(dsc) then suffix='_dsc'
  if n_elements(day0) eq 0 then if keyword_set(south) then day0=2007317L else day0=2007144L
  if n_elements(res) eq 0 then res=2048
  old_plot=!d.name
  set_plot,'z'
  old_map=bytarr(res,res/2)
  erase
  device,set_resolution=[res,res/2]

  l3a=read_cips_file_bynum("/aim/data/cips/v3",'3a',day,suffix,succeed=succeed)

  if ~succeed then begin
    print,"Didn't find file for orbit "+string(orbit)
  end else begin
    print,yd2vms(day)
    openw,17,/append,'times_3A.txt'
    printf,17,string(format='(%"%s Day %7d")',yd2vms(day),day)
    close,17
    bbox_lat_lon,/no_co,l3a.bbox,lat=lat_data,lon=lon_data
    data=*l3a.albedo
    data=data>10<60
    s=size(data,/dim)
    lat_data=lat_data[0:s[0]-1,0:s[1]-1]
    lon_data=lon_data[0:s[0]-1,0:s[1]-1]

    ll_data=compose_grid(lat_data,lon_data)
    lat_data=0
    lon_data=0
    llp_data=oblique(ll_data,[0,-30.5],/deg)
    llp_data=oblique(ll_data,[0,0],/deg)
    resolve_grid,llp_data,x=latp_data,y=lonp_data,n_dimension_vec=2
    llp_data=0
    x_data=linterp(-180.0,0,180.0,res,lonp_data)
    lonp_data=0
    y_data=linterp(-90.0,0,90.0,res/2,latp_data)
    latp_data=0
    data_map=map_patch_lambert(data,x=x_data,y=y_data,bbox=bbox)
    data=0
    x_data=0
    y_data=0
    f=data_map le 10
    data_map=byte(((data_map-10)>0)*255/50)
    old_map[bbox[0]:bbox[2],bbox[1]:bbox[3]]*=f
    data_map[where(f)]=0
    old_map[bbox[0]:bbox[2],bbox[1]:bbox[3]]+=data_map
  end
  tv,old_map
;  plots,x_coast,y_coast,/device
;  xyouts,charsize=1,475,370,'AIM Cloud Presence',/device
;  xyouts,charsize=1,475,355,string(orbit,format='(%"Orbit %4d")'),/device
;  xyouts,charsize=1,475,340,usec2vms(l2a.stack_start_time),/device
;  xyouts,charsize=1,475,325,'NASA/HU/VT/CU LASP',/device
  frame_n=day-day0
  if keyword_set(south) then south_tag='_S' else south_tag=''
  saveimage,/png,string(south_tag,south_tag,frame_n,format='(%"/home/horsehead/jeppesen/pov/SOS_3A%s/SOS_Map_3A%s_%05d.png")')
  set_plot,old_plot
  str_free,l2a
  l2a=0
  heap_gc
end
