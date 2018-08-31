pro range_ring, stlat, stlon, range, npts, bearing, latp, lonp		$
	      , start_bear=start_bear, ccw = ccw

;+
; NAME:
;   range_ring
; PURPOSE:
;   calculates the array of latitudes and longitudes
;   that are at a particular range from a central latitude
;   longitude site (forming a crcle around that point)
; CATEGORY:
;   geophysics
; CALLING SEQUENCE:
;   range_ring,stlat,stlon,range,npts,bearing,latp,lonp
; INPUTS:   
;         stlat   = center latitude (in degs.)
;         stlon   = center longitude (in degs.)
;         range   = distance of the circle from the central point (in KM)
;         npts    = number of points in the latitude, longitude, distance
;                      arrays to be returned (if zero, defaults to 36)
; OPTIONAL INPUT PARAMETERS:   
; KEYWORD PARAMETERS:
;         start_bear = starting bearing (default = 0.0) 
;         ccw        = if set figuress bearings in the counter-clockwise
;			sense, rather than clockwise  
; OUTPUTS:  
;         bearing = direction of point from the central point (0=N, 180=S, 
;                                                            270=W, 90=E)
;         latp    = an npts element vector of latitudes spaced 
;                   at del km increments
;         lonp    = corresponding longitudes for latp
; OPTIONAL OUTPUT PARAMETERS:  
; COMMON BLOCKS:   
; SIDE EFFECTS:   
; RESTRICTIONS:   
; PROCEDURE:   
; REQUIRED ROUTINES:   
; MODIFICATION HISTORY: 
; pan 9/10/90
;    $Header: /science/general/misc/programs//range_ring.pro,v 1.3 93/02/25 16:08:42 newman Exp $
;-


oo=n_params(0)

  if (oo lt 4) then begin
     print,'range_ring: wrong number of parameters'
     return
  endif


  if (abs(stlat) gt 90) then begin
     print,'range_ring: check your central latitude, must be in degrees'
     return
  endif

;  lons=pos_angle(float(stlon))
  lons=abs(float(stlon))

  if npts eq 0 then npts=36

  npts=long(npts)

  if n_elements(start_bear) eq 0 then start_bear = 0.
  if n_elements(ccw) eq 0 then ccw = 1.0 else ccw = -1
  bearing=ccw * double(360.*findgen(npts)/npts) + start_bear

  latp=fltarr(npts)
  lonp=fltarr(npts)

; ** radius of the earth, radians

  re=40000./2./!pi
  rad=double(180./!pi)

; ** c is great circle angle between st and our points

  case stlat of

      90: begin
            sina=1.0e-10
            cosa=sqrt(1.-sina^2)
          end
     -90: begin
            sina=1.0e-10
            cosa=-1.0*sqrt(1.-sina^2)
          end
    else: begin
            a=(90.-stlat)/rad
            sina=sin(a)
            cosa=cos(a)
          end
  endcase

  BB=bearing/rad

  c=range/re

  if (range eq 20000.) then begin
     print,'range_ring: Your distance is 180. deg of latitude, truncating to 180. - 1.0e-5'
     range=19999.
     c=range/re
  endif

  sinc=sin(c)
  cosc=cos(c)

  cosBB=cos(BB)
  sinBB=sin(BB)

  cosb=cosa*cosc+sina*sinc*cosBB

  b=acos(cosb)

  sinb=sin(b)

  latp=90.-b*rad

; ** sincu and coscu are the arrays of sines and cosines of the longitudes

  sinAA = sina * sinBB /sinb

  cosAA = (cosa-cosc*cosb) /sinc /sinb

  sinCC = sinc * sinBB /sinb

  cosCC = - cosAA * cosBB + sinAA * sinBB * cosc
  
; ** calculate the lats and lons

  lonp= atan(sinCC,cosCC) * rad + lons

return
end
