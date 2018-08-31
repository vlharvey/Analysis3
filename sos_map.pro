pro sos_map,l3a,map_fn,res=res
  if n_elements(res) eq 0 then res=2048
  restore,'sds/common/input_data/coast.sav'
  lat_coast=reform(coast[1,*])
  lon_coast=reform(coast[0,*])
  coast=0
  ll_coast=compose_grid(lat_coast,lon_coast)
  lat_coast=0 ;save_memory
  lon_coast=0 ;save_memory
  llp_coast=oblique(ll_coast,[0,-30.5],/deg)
  ll_coast=0 ;save memory
  latp_coast=llp_coast[*,0]
  lonp_coast=llp_coast[*,1]
  llp_coast=0 ;save memory
  x_coast=linterp(-180.0,0,180.0,res,lonp_coast)
  y_coast=linterp(-90.0,0,90.0,res/2,latp_coast)
  latp_coast=0 ;save_memory
  lonp_coast=0 ;save_memory

  bbox_lat_lon,/no_co,l3a.bbox,lat=lat_data,lon=lon_data
  data=*l3a.albedo>0<50
  s=size(data,/dim)
  lat_data=lat_data[0:s[0]-1,0:s[1]-1]
  lon_data=lon_data[0:s[0]-1,0:s[1]-1]

  ll_data=compose_grid(lat_data,lon_data)
  lat_data=0
  lon_data=0
  llp_data=oblique(ll_data,[0,-30.5],/deg)
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
  old_plot=!d.name
  set_plot,'z'
  device,set_resolution=[res,res/2]
  tvscl,/nan,data_map,bbox[0],bbox[1]
  plots,x_coast,y_coast,/device
  saveimage,/jpeg,map_fn
  set_plot,old_plot
end
