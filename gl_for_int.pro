;Do geolocation for Mike Callan's integrated rayleigh background
;  input
;    t           - Image_tlm_timestamp, GPS microseconds
;    cam_name    - Camera name, one of ['px','mx','py','my']
;    alt         - 1D Array of altitudes to do geolocation for
;    x,y         - size of grid, in pixels
;    /grid       - use grid nodes instead of pixel centers
;  output
;    lat=lat     - Latitude in radians,           3D array [pix_x,pix_y,alt]
;    lon=lon     - Longitude in radians,          3D array [pix_x,pix_y,alt]
;    ssa=ssa     - Scattering angle in radians,   2D array [pix_x,pix_y]
;    sva=sva     - View angle in radians,         2D array [pix_x,pix_y]
;    sza=sza     - Solar Zenith Angle in radians, 3D array [pix_x,pix_y,alt]
;    p_sza=p_sza - Solar Zenith Angle at peak contribution altitude, in radians,
;                                                 2D array [pix_x,pix_y]
pro gl_for_int,t,cam_name,alt,x,y,grid=grid,lat=lat,lon=lon,ssa=ssa,sva=sva,sza=sza,p_sza=p_sza
  caminfo=loadcaminfo([1,1,1,1]*x,[1,1,1,1]*y,grid=grid)

  lat=dblarr(x,y,n_elements(alt))
  lon=lat
  sza=lat

  quat=loadquat(start=t-600d6,stop=t+600d6)
  
  w=where(cam_name eq ['px','mx','py','my'])
  if w ge 2 then begin
    ;TDI camera - use shutter close time
    delay=2773000d
  end else begin
    ;Snapshot camera - use mid-image time
    delay=2773000d -714000d/2d
  end
  
  for i=0,n_elements(alt)-1 do begin
    
    result=cam_to_lla(cam_name,t+delay,alt[i],caminfo,quat,/view)
    lat[*,*,i]=result.lat
    lon[*,*,i]=result.lon
    ssa=result.ssa
    sva=result.sva
    p_sza=result.sza_p
    sza[*,*,i]=result.sza
  end 
end
