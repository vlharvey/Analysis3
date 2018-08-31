FUNCTION vortexshape, marker, lat, lon
  ;NAME:
  ; vortexshape
  ;
  ;PURPOSE:
  ;
  ;CALLING SEQUENCE:
  ;
  ;INPUTS:
  ; marker- a 3-D array of cyclone and anti-cyclone markers where the
  ;          1st dimension = longitude location
  ;          2nd dimenstion = latitude location
  ;          3rd dimension = vertical surface location (z, pressure, theta, ect)
  ; lat- a 1-D array of latitude coordinates arranged from -90 to +90
  ; lon- a 1-D array of longitude coordinates
  ;
  ;OUTPUTS:
  ; shape- a structure

  marker_size = size(marker)
  nx = marker_size(1)
  ny = marker_size(2)
  nz = marker_size(3)


  ;Constants
  R_e =6378100    ;[m] radius of earth
  R_e2 =6371000.    ;[m] radius of earth (assuming a perfect sphere, limiting errors due to oblateness)
  d_lat_deg = lat(1)-lat(0) ;[degrees] distance between latitude parallels
  d_lat_rad = d_lat_deg*!pi/180.
  d_lat = R_e*d_lat_deg*!pi/180.  ;[m] distance between latitude parallels
  d_lon_rad = (lon(1)-lon(0))*!pi/180.*cos(lat*!pi/180) ;[rad] between lines of longitude
  ;dr = 1.0    ;change in radius in spherical coordinate, but on a constant shell
  ;  lon_1= lon
  ;  lons_id = where(lon_1 gt 180.0)
  ;  lon_1(lons_id) = lon(lons_id)-360.
  ;  lon_1 = lon_1(sort(lon_1))

  ;Position of each grid point in x-y-z space
  x = make_array(nx,ny)
  y = make_array(nx,ny)
  z = make_array(nx,ny)
  dx = make_array(nx,ny)
  dy = make_array(nx,ny)
  dz = make_array(nx,ny)
  for i=0,nx-1 do begin
    for j=0,ny-1 do begin
      x(i,j) = R_e*cos(lon(i)*!pi/180.)*cos(lat(j)*!pi/180.)
      y(i,j) = R_e*sin(lon(i)*!pi/180.)*cos(lat(j)*!pi/180.)
      z(i,j) = R_e*sin(lat(j)*!pi/180.)
      dx(i,j) = (-1.0)*R_e*sin(lon(i)*!pi/180.)*cos(lat(j)*!pi/180.)*d_lon_rad(j) + R_e*cos(lon(i)*!pi/180.)*sin(lat(j)*!pi/180.)*d_lat_rad
      dy(i,j) = R_e*cos(lon(i)*!pi/180.)*cos(lat(j)*!pi/180.)*d_lon_rad(j) + R_e*sin(lon(i)*!pi/180.)*sin(lat(j)*!pi/180.)*d_lat_rad
      dz(i,j) = R_e*(-1.0)*cos(lat(j)*!pi/180.)*d_lat_rad
    endfor
  endfor

  ;Remove anit-clyclones for now
  mark_neg_id = where(marker lt 0)
  marker(mark_neg_id) = 0

  ;=================================================================================
  ;Find Centroid & Area of Vortex
  location = make_array(2,nz)
  da = make_array(nx,ny)
  NH_A= make_array(nz,1)
  SH_A= make_array(nz,1)
  NH_R = make_array(nz,1)
  NH_lam = make_array(nz,1)
  NH_phi = make_array(nz,1)
  for j=0,ny-1 do begin
    da(*,j) = abs(dx(*,j)*dy(*,j)*dz(*,j)) ;Area element
  endfor
  for k=0,nz-1 do begin
    ;Calculate Total Area of Vortex
    NH_A(k) = total(marker(*,floor(ny/2):ny-1,k)*da(*,floor(ny/2):ny-1)) ;Total area of vortex
    SH_A(k) = total(marker(*,0:floor(ny/2),k)*da(*,0:floor(ny/2)))
    x_bar =total(total(marker(*,floor(ny/2):ny-1,k)*x(*,floor(ny/2):ny-1)*da(*,floor(ny/2):ny-1),2))/NH_A(k)
    y_bar =total(total(marker(*,floor(ny/2):ny-1,k)*y(*,floor(ny/2):ny-1)*da(*,floor(ny/2):ny-1),1))/NH_A(k)
    z_bar =total(total(marker(*,floor(ny/2):ny-1,k)*z(*,floor(ny/2):ny-1)*da(*,floor(ny/2):ny-1)))/NH_A(k)

    ;Convert x-y-z coordinates back to spherical coordinates
    NH_R(k) = sqrt(x_bar^2.+y_bar^2.+z_bar^2.)
    NH_lam(k) = atan(y_bar,x_bar)*180./!pi
    NH_phi(k) = asin(z_bar/NH_r(k))*180./!pi

  endfor

  ;=========================================================================
  ;Ellipticity

  ;To find perimeter points, remove all interior points
  ;Speed up later by only searching marker points

  NH_boundary = make_array(nx,ny,nz)
  NH_majoraxis = make_array(nz)
  NH_minoraxis = make_array(nz)
  NH_major_x = make_array(nz)
  NH_major_y = make_array(nz)
  NH_minor_x = make_array(nz)
  NH_minor_y = make_array(nz)
  for k=0,nz-1 do begin
    mark_wrap=marker(*,*,k)  ;Need to wrap markers around in longitude to avoid discontinuity
    boundary = mark_wrap
;Northern Hemisphere
    for j=0,ny-1 do begin
      if j lt ny/2 then begin
        boundary(*,j) = 0.
      endif
      if j gt ny/2 then begin 
        for i=0,nx-1 do begin
          if (i eq 0) AND (j lt ny-1) then begin
            if (mark_wrap(i+1,j) eq 1) AND (mark_wrap(i,j+1) eq 1) AND (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
          if (i eq nx-1) AND (j lt ny-1) then begin
            if (mark_wrap(i,j+1) eq 1) AND (mark_wrap(i-1,j) eq 1) AND (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
          if (i eq 0) AND (j eq ny-1) then begin
            if (mark_wrap(i+1,j) eq 1) AND (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
          if (i eq nx-1) AND (j eq ny-1) then begin
            if (mark_wrap(i-1,j) eq 1) AND (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
          if (j eq ny-1) AND (i gt 0) AND (i lt nx-1) then begin
            if (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
          if (i gt 0) AND (i lt nx-1) AND (j lt ny-1) then begin
            if (mark_wrap(i+1,j) eq 1) AND (mark_wrap(i,j+1) eq 1) AND (mark_wrap(i-1,j) eq 1) AND (mark_wrap(i,j-1)) then boundary(i,j)=0
          endif
        endfor
      endif
    endfor
    NH_boundary(*,*,k)=boundary
    ;Now find lon/lat boundary points
    index = where(NH_boundary(*,*,k) eq 1)
    s = size(NH_boundary)
    ncol = s(1)
    p_lon = index mod ncol    ;longitude indices of perimeter of vortex
    p_lat = index / ncol      ;latitude indices of perimeter of vortex
    
    ;Using Haversine Formula, calculate the distance between
    ;every point on the perimeter of the vortex and the centroid location
    a = make_array(n_elements(p_lon),1)
    for nq = 0,n_elements(p_lon)-1 do begin
      lon1 = lon(p_lon(nq))*!pi/180.
      lat1 = lat(p_lat(nq))*!pi/180.
      lon2 = NH_lam(k)*!pi/180.
      lat2 = NH_phi(k)*!pi/180.
      dlon = abs(lon1-lon2)
      ;if dlon gt 2*!pi then dlon = abs(lon1-lon2)-2.*!pi
      if dlon gt !pi then dlon = abs((lon1-lon2)-2.*!pi)
      dlat = abs(lat1-lat2)
      a(nq) = (sin(dlat/2.))^2.+cos(lat1)*cos(lat2)*(sin(dlon/2.))^2.
    endfor
    distances= R_e2*2.*atan(sqrt(a),sqrt(1-a))
    if n_elements(distances) eq 1 then NH_majoraxis(k) = 0
    if n_elements(distances) eq 1 then NH_minoraxis(k) = 0
    if n_elements(distances) gt 1 then begin
      NH_majoraxis(k) = max(distances, ID) ;Find the longest distance - the semi-major axis - and which points the line connects
      ix = ID mod n_elements(p_lon)
      iy = ID/n_elements(p_lat)
      lon_a = p_lon(ix)
      lat_a = p_lat(ix)
      lon_b = p_lon(iy)
      lat_b = p_lat(iy)
      NH_major_x(k) = lon(lon_a)
      NH_major_y(k) = lat(lat_a)

      ;Find the right-angle points and determine which is shorter for semi-minor axis
      a_c = make_array(n_elements(p_lon),1) ;Calculate the distance between semi-major and all possible semi-minor points on boundary
      for nq = 0,n_elements(p_lon)-1 do begin
        lon1 = lon(p_lon(nq))*!pi/180.
        lat1 = lat(p_lat(nq))*!pi/180.
        lon2 = NH_major_x(k)*!pi/180.
        lat2 = NH_major_y(k)*!pi/180.
        dlon = abs(lon1-lon2)
        ;if dlon gt 2*!pi then dlon = abs(lon1-lon2)-2.*!pi
        if dlon gt !pi then dlon = abs((lon1-lon2)-2.*!pi)
        dlat = abs(lat1-lat2)
        a_c(nq) = (sin(dlat/2.))^2.+cos(lat1)*cos(lat2)*(sin(dlon/2.))^2.
      endfor
      a_d = NH_majoraxis(k)/R_e2    ;Semi-major axis length (on unit circle)
      b_d = distances/R_e2       ;possible semi-minor axis lengths (on unit circle)
      c_d= 2.*atan(sqrt(a_c),sqrt(1-a_c))    ;length connecting major and minor axis (on unit circle)
      alpha = acos((cos(a_d)-cos(b_d)*cos(c_d))/(sin(b_d)*sin(c_d)))
      c_angle = asin((sin(alpha)*sin(c_d))/(sin(a_d)))
      ;Look for all angles between 85 and 90 degrees
      qq1 = where(abs(c_angle-!pi/2.) lt 5.*!pi/180.)
      if qq1(0) ge 0 then begin
      NH_minoraxis(k) = min(distances(where(abs(c_angle-!pi/2.) lt 5.*!pi/180.)),id)
      NH_minor_x(k) = lon(p_lon(qq1(id)))
      NH_minor_y(k) = lat(p_lat(qq1(id)))
      endif
      if qq1(0) lt 0 then begin ;If there isn't an angle that is near 90 degrees (hopefully, not true)
        NH_minor_x(k) = !VALUES.F_NAN
        NH_minor_y(k) = !VALUES.F_NAN
        NH_minoraxis(k) = !VALUES.F_NAN
      endif

    endif

  endfor
 


  ;==========================================================================
  ;Return Findings
  Shape = {area:FLTARR(2,nz),NHcentroid:fltarr(2,nz),axis:fltarr(2,nz),NHmajoraxisloc:fltarr(2,nz),NHminoraxisloc:fltarr(2,nz)}
  Shape.area = [transpose(NH_A),transpose(SH_A)]
  Shape.NHcentroid = [transpose(NH_lam),transpose(NH_phi)]
  ;Shape.SHcentroid = [transpose(SH_lam),transpose(SH_phi)]
  Shape.axis = [transpose(NH_majoraxis),transpose(NH_minoraxis)]
  Shape.NHmajoraxisloc = [transpose(NH_major_x),transpose(NH_major_y)]
  Shape.NHminoraxisloc = [transpose(NH_minor_x),transpose(NH_minor_y)]

  return, Shape

END
