pro marker_highs_v6,dat,mark,qdf,zeta,u,v,x,y,pv

; smallest anticyclone spans 4 lats and 4 lons w/12 gridpoints
;
; when integrated QDF around an SF isopleth is negative
; calculate integrated QDF within the area enclosed by the
; SF isopleth.  If the area integrated QDF is also negative
; SF area is filled within the sector domain.  90 degree
; longitude sectors are rotated by 30 degrees in search
; of anticyclones.  If a high is marked the next sector
; longitude min = last sector longitude max.

nbins=20
nr=n_elements(y)
nc=n_elements(x)
nmark0=0.
imark=0
isect=0
dsect=90.
dx=x(1)-x(0)
dy=y(1)-y(0)
lon=0.*mark
lat=0.*mark
for i=0,nc-1 do lat(i,*)=y
for j=0,nr-1 do lon(*,j)=x
datbin=0.0*fltarr(nbins)

; Northern Hemisphere
kk=where(lat gt 0.)
datmin=min(dat(kk))
datmax=max(dat(kk))
datavg=(datmin+datmax)/2.0
datint=(datmax-datavg)/(nbins-1)
datbin=datavg+datint*findgen(nbins)
;
; if theta surface is discontinuous poleward of 40 degrees latitude
;
index=where(pv eq 1.00000e+12)
if index(0) ne -1L then begin
;  print,' discontinuous up to ',max(lat(index))
   if max(lat(index)) gt 40. then goto,dosh
endif

; begin with 90 degree longitude sectors (dsect=90.)
; loop over sectors.  lonmin of n+1 sector = lonmax of n sector
lonmin=x(0)
dloop=lonmin
lonmax=lonmin+dsect
lonmin_save=lonmin
lonmax_save=lonmax
WHILE (dloop le 360.*2. and imark eq 0) or $
      (dloop le 360.*4. and imark gt 0) do begin
      if lonmin gt x(nc-1) then lonmin=lonmin-360.
      if lonmin lt x(0)    then lonmin=lonmin+360.
      if lonmax gt x(nc-1) then lonmax=lonmax-360.
      if lonmax lt x(0)    then lonmax=lonmax+360.

; save initial sector bounds before looping over isopleths
      lonmin_save=lonmin
      lonmax_save=lonmax
      latmin_save=0.
      latmax_save=90.

; loop over SF isopleths
      for n = 0,nbins-2 do begin

; for each isopleth reset sector bounds to what they were for 1st isopleth
          lonmin=lonmin_save
          lonmax=lonmax_save
          latmin=latmin_save
          latmax=latmax_save

; identify gridpoints within SF bin and in sector that are unmarked
          if lonmin lt lonmax then $
             t=where(lat gt 0.0 and lon ge lonmin and lon le lonmax and $
                     dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)
          if lonmax lt lonmin then $
             t=where(lat gt 0.0 and (lon ge lonmin or lon le lonmax) and $
                     dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)

; require at least 12 gridpoints (2 rows of 4 lons, 2 columns of 3 lats)
          if it lt 12 then goto,jumpnhbin

; very first thing that must be done is make sure t gridpoints are
; unimodal in longitude and latitude (no gaps)

; if vortex does not span GM
          if min(lon(t)) gt x(0) or max(lon(t)) lt x(nc-1) then begin
             sindex=sort(lon(t))
             sortedlons=lon(t(sindex))
             jj=findgen(it-1)
             index=where(abs(sortedlons(jj+1)-sortedlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpnhbin
             if index(0) ne -1 then begin
                for jj=0,it-2 do begin
                    if abs(sortedlons(jj+1)-sortedlons(jj)) gt dx then begin
                       lonmin=sortedlons(jj+1)
                       lonmax=sortedlons(it-1)
                       goto,jump1
                    endif
                endfor
             endif
             jump1:
          endif 
; if vortex spans GM
          if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
             sindex=sort(lon(t))
             sortedlons=lon(t(sindex))
             index=where(sortedlons gt 180.,itt)
             highlons=sortedlons(index)
             if itt eq 1 then goto,jump2
             jj=findgen(itt-1)
             index=where(abs(highlons(jj+1)-highlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpnhbin
             if index(0) ne -1 then begin
                lonmin=min(highlons)
                for jj=0,itt-2 do begin
                    if abs(highlons(jj+1)-highlons(jj)) gt dx then begin
                       lonmin=highlons(jj+1)
                       goto,jump2
                    endif
                endfor
             endif
             jump2:

             index=where(sortedlons lt 180.,itt)
             lowlons=sortedlons(index)
             if itt eq 1 then goto,jump3
             jj=findgen(itt-1)
             index=where(abs(lowlons(jj+1)-lowlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpnhbin
             if index(0) ne -1 then begin
                lonmax=max(lowlons)
                for jj=0,itt-2 do begin
                    if abs(lowlons(jj+1)-lowlons(jj)) gt dx then begin
                       lonmax=lowlons(jj)
                       goto,jump3
                    endif
                endfor
             endif
             jump3:
          endif

; gridpoints in new longitude bounds
          if lonmin ne lonmin_save or lonmax ne lonmax_save then begin
             if lonmin lt lonmax then $
                t=where(lat gt 0.0 and lon ge lonmin and lon le lonmax and $
                        dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)
             if lonmax lt lonmin then $
                t=where(lat gt 0.0 and (lon ge lonmin or lon le lonmax) and $
                        dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)
             if it lt 12 then goto,jumpnhbin
             if max(lat(t))-min(lat(t)) lt 4.*dy then goto,jumpnhbin
             if max(lon(t))-min(lon(t)) lt 4.*dx then goto,jumpnhbin
             if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
                tmpx=lon(t)
                index=where(tmpx lt 180.)
                tmpx(index)=tmpx(index)+360.
                if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpnhbin
             endif
          endif

; make sure that latitude distribution is unimodal
          sindex=sort(lat(t))
          sortedlats=lat(t(sindex))
          jj=findgen(it-1)
          index=where(abs(sortedlats(jj+1)-sortedlats(jj)) gt dx,ii)
          if ii gt 1 then goto,jumpnhbin
          if index(0) ne -1 then begin
             for jj=0,it-2 do begin
                 if abs(sortedlats(jj+1)-sortedlats(jj)) gt dy*1.1 then begin
                    latmin=sortedlats(jj+1)
                    latmax=sortedlats(it-1)
                    goto,jump4
                 endif
             endfor
             jump4:
          endif

; gridpoints in new latitude bounds
          if latmin ne latmin_save or latmax ne latmax_save then begin
             if lonmin lt lonmax then $
                t=where(lat gt latmin and lat lt latmax and $
                        lon ge lonmin and lon le lonmax and $
                        dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)
             if lonmax lt lonmin then $
                t=where(lat gt latmin and lat lt latmax and $
                       (lon ge lonmin or lon le lonmax) and $
                        dat gt datbin(n) and dat le datbin(n+1) and mark eq 0.,it)
             if it lt 12 then goto,jumpnhbin
             if max(lat(t))-min(lat(t)) lt 4.*dy then goto,jumpnhbin
             if max(lon(t))-min(lon(t)) lt 4.*dx then goto,jumpnhbin
             if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
                tmpx=lon(t)
                index=where(tmpx lt 180.)
                tmpx(index)=tmpx(index)+360.
                if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpnhbin
             endif
          endif

; QDF and Zeta integrated along SF isopleth
          avgq = total(qdf(t))/float(it)
          avgz = total(zeta(t))/float(it)

; require negative integrated QDF and zeta in SF isopleth
          if avgz gt 0. or avgq gt 0. then goto,jumpnhbin

; space fill inside SF>=datbin(n) 
          if lonmin lt lonmax then $
             s=where(lat gt latmin and lat lt latmax and $
                     lon ge lonmin and lon le lonmax and $
                     dat ge datbin(n) and mark eq 0.,is)
          if lonmax lt lonmin then $ 
             s=where(lat gt latmin and lat lt latmax and $
                    (lon ge lonmin or lon le lonmax) and $
                     dat ge datbin(n) and mark eq 0.,is)

          if lonmin eq lonmax then goto,jumpnhbin

          if is lt 12 then goto,jumpnhbin
          if max(lat(s))-min(lat(s)) lt 4.*dy then goto,jumpnhbin
          if max(lon(s))-min(lon(s)) lt 4.*dx then goto,jumpnhbin

; if line crosses GM the above logic will return anomalous width
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             tmpx=lon(s)
             index=where(tmpx lt 180.)
             tmpx(index)=tmpx(index)+360.
             if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpnhbin
          endif

; area integrated QDF and Zeta
          avgq = total(qdf(s))/is
          avgz = total(zeta(s))/is

; must have negative integrated QDF and zeta in area
          if avgz gt 0. or avgq gt 0. then goto,jumpnhbin

; TEST LOGIC
; reset lonmax, lonmin, latmin, and latmax to edges of s gridpoints.  then
; expand and see if the extra gridpoints included still touch boundaries
if min(lon(s)) ne x(0) or max(lon(s)) ne x(nc-1) then begin
   lonmin=min(lon(s))
   lonmax=max(lon(s))
endif
if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
   index=where(lon(s) gt 180.)
   lonmin=min(lon(s(index)))
   index=where(lon(s) lt 180.)
   lonmax=max(lon(s(index)))
endif
if lonmin lt 0. then lonmin=lonmin+360.
if lonmax gt 360. then lonmax=lonmax-360.
latmin=min(lat(s))
latmax=max(lat(s))

; previously marked NH vortices
          pr=where(lat gt 0. and mark ne 0.,ip)

; make sure gridpoints do not touch sector edge or previously marked vortex
          lonmin0=lonmin
          lonmax0=lonmax
          latmin0=latmin
          latmax0=latmax
          if lonmax0 gt lonmin0 then width=lonmax0-lonmin0
          if lonmin0 gt lonmax0 then width=(360.-lonmin0)+lonmax0
          while min(abs(lon(s)-lonmin0)) le dx or $
                min(abs(lon(s)-lonmax0)) le dx or $
                (min(abs(lat(s)-latmax0)) le dy and latmax0 ne 90.) or $
                (min(abs(lat(s)-latmin0)) le dy and latmin0 ne 0.) do begin

; expand sector longitudes. Do not mark directly next to sector edges
                if min(abs(lon(s)-lonmin0)) le dx then lonmin0=lonmin0-10.
                if lonmin0 lt x(0) then lonmin0=lonmin0+360.
                if min(abs(lon(s)-lonmax0)) le dx then lonmax0=lonmax0+10.
                if lonmax0 gt x(nc-1) then lonmax0=lonmax0-360.
; expand sector latitudes
                if min(abs(lat(s)-latmin0)) le dy then latmin0=latmin0-10.
                if latmin0 lt 0. then latmin0=0.
                if min(abs(lat(s)-latmax0)) le dy then latmax0=latmax0+10.
                if latmax0 gt 90. then latmax0=90.

; do not mark if sector edge is directly next to previously marked vortices
                if pr(0) ne -1 then begin
                   if min(abs(lon(pr)-lonmin0)) le dx or $
                      min(abs(lon(pr)-lonmax0)) le dx then begin
                      goto,jumpnhbin
                   endif
                endif

; area fill with new sector boundaries
                if lonmin0 lt lonmax0 then $
                   s=where(lat ge latmin0 and lat le latmax0 and $
                           lon ge lonmin0 and lon le lonmax0 and $
                           dat ge datbin(n) and mark eq 0.,is)
                if lonmin0 gt lonmax0 then $
                   s=where(lat ge latmin0 and lat le latmax0 and $
                          (lon ge lonmin0 or lon le lonmax0) and $
                           dat ge datbin(n) and mark eq 0.,is)

; if after adjusting longitude nothing is marked...
                if s(0) eq -1 then goto,jumpnhbin

; monitor cumulative degrees of expansion.  allow sector to be up to 180 degrees wide
                if lonmax0 gt lonmin0 then width=lonmax0-lonmin0
                if lonmin0 gt lonmax0 then width=(360.-lonmin0)+lonmax0
                if width gt 180. then goto,jumpnhbin
          endwhile
          lonmin=lonmin0
          lonmax=lonmax0
          latmin=latmin0
          latmax=latmax0

; jump bin if canditate marks touch previously marked vortex
          if pr(0) ne -1 then $
             for jj=0,is-1 do $
                 if min(abs(lon(pr)-lon(s(jj)))) le dx and $
                    min(abs(lat(pr)-lat(s(jj)))) le dy then goto,jumpnhbin

; make sure that longitude distribution is unimodal

; if vortex does not span GM
          if min(lon(s)) gt x(0) or max(lon(s)) lt x(nc-1) then begin
             sindex=sort(lon(s))
             sortedlons=lon(s(sindex))
             for jj=0,is-2 do begin
                 if abs(sortedlons(jj+1)-sortedlons(jj)) gt dx then begin
                    lonmin=sortedlons(jj+1)
                    lonmax=sortedlons(is-1)
                    goto,nhjumpout1
                 endif
             endfor
             nhjumpout1:
          endif

; if vortex spans GM then check modality of longitudes on either side of GM 
; separately.  First check "high longitudes"
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             sindex=sort(lon(s))
             sortedlons=lon(s(sindex))
             index=where(sortedlons gt 180.,is)
             highlons=sortedlons(index)
             lonmin=min(highlons)
             for jj=0,is-2 do begin
                 if abs(highlons(jj+1)-highlons(jj)) gt dx then begin
                    lonmin=highlons(jj+1)
                    goto,nhjumpoutgm1
                 endif
             endfor
             nhjumpoutgm1:

; then check "low longitudes"
             index=where(sortedlons lt 180.,is)
             lowlons=sortedlons(index)
             lonmax=max(lowlons)
             for jj=0,is-2 do begin
                 if abs(lowlons(jj+1)-lowlons(jj)) gt dx then begin
                    lonmax=lowlons(jj)
                    goto,nhjumpoutgm2
                 endif
             endfor
             nhjumpoutgm2:
          endif			; GM logic

; now make sure that the latitude distribution is unimodal
          sindex=sort(lat(s))
          sortedlats=lat(s(sindex))
          for jj=0,is-2 do begin
              if abs(sortedlats(jj+1)-sortedlats(jj)) gt dy*1.1 then begin
                 latmin=sortedlats(jj+1)
                 latmax=sortedlats(is-1)
                 goto,nhjumpout2
              endif
          endfor
          nhjumpout2:

          if lonmin lt lonmax then $
             s=where(lat gt latmin and lat le latmax and $
                     lon ge lonmin and lon le lonmax and $
                     dat ge datbin(n) and mark eq 0.,is)
          if lonmax lt lonmin then $
             s=where(lat gt latmin and lat le latmax and $
                    (lon ge lonmin or lon le lonmax) and $
                     dat ge datbin(n) and mark eq 0.,is)

          if is lt 12 then goto,jumpnhbin
          if max(lat(s))-min(lat(s)) lt 4.*dy then goto,jumpnhbin
          if max(lon(s))-min(lon(s)) lt 4.*dx then goto,jumpnhbin

; if line crosses GM the above logic will return anomalous width
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             tmpx=lon(s)
             index=where(tmpx lt 180.)
             tmpx(index)=tmpx(index)+360.
             if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpnhbin
          endif

          avgq = total(qdf(s))/is
          avgz = total(zeta(s))/is

; require negative area-integrated QDF in stretched longitudinal sector
          if avgz gt 0. or avgq gt 0. then goto,jumpnhbin

; if you've made it this far then mark the sucker
          mark(s)=-1.0*(imark+1.0)
;         print,'NH ',imark,'MARKED EDGE ',datbin(n),' BIN # ',n
          imark=imark+1
          goto, jumpnhsect
          jumpnhbin:
      endfor		; loop over SF isopleths

      jumpnhsect:
      isect=isect+1

; current number of marked gridpoints in NH
      s=where(lat gt 0. and mark ne 0.,nmark1)

; monitor cumulative degrees of rotation
      if nmark1 eq nmark0 then dloop=dloop+30.
      if nmark1 gt nmark0 then begin
         if lonmin lt lonmax then dloop=dloop+(lonmax-lonmin)
         if lonmin gt lonmax then dloop=dloop+(360.-lonmin)+lonmax
      endif

; set new sector bounds : rotate by 30 degrees if nothing was marked
; set lonmin=lonmax if an anticyclone was marked (no overlap)
      if nmark1 eq nmark0 then lonmin=lonmin+30.
      if nmark1 gt nmark0 then lonmin=lonmax
      if lonmin gt x(nc-1) then lonmin=lonmin-360.
      lonmax=lonmin+dsect
      if lonmax gt x(nc-1) then lonmax=lonmax-360.

; retain number of gridpoints marked
      nmark0=nmark1
ENDWHILE		; loop over longitude sectors

dosh:

; Southern Hemisphere
nmark0=0.
imark=0
isect=0
dsect=90.
kk=where(lat lt 0.)
datmin=min(dat(kk))
datmax=max(dat(kk))
datavg=(datmin+datmax)/2.0
datint=(datmax-datavg)/(nbins-1)
datbin=datavg-datint*findgen(nbins)
;
; if theta surface is discontinuous poleward of 40 degrees latitude
;
index=where(pv eq 1.00000e+12)
if index(0) ne -1L then begin
;  print,' discontinuous up to ',min(lat(index))
   if min(lat(index)) lt -40. then return
endif

; begin with 90 degree longitude sectors (dsect=90.)
; loop over sectors.  lonmin of n+1 sector = lonmax of n sector
lonmin=x(0)
dloop=lonmin
lonmax=lonmin+dsect
lonmin_save=lonmin
lonmax_save=lonmax
WHILE (dloop le 360.*2. and imark eq 0) or $
      (dloop le 360.*4. and imark gt 0) do begin
      if lonmin gt x(nc-1) then lonmin=lonmin-360.
      if lonmin lt x(0)    then lonmin=lonmin+360.
      if lonmax gt x(nc-1) then lonmax=lonmax-360.
      if lonmax lt x(0)    then lonmax=lonmax+360.

; save initial sector bounds before looping over isopleths
      lonmin_save=lonmin
      lonmax_save=lonmax
      latmin_save=-90.
      latmax_save=0.

; loop over SF isopleths
      for n = 0,nbins-2 do begin

; for each isopleth reset sector bounds to what they were for 1st isopleth
          lonmin=lonmin_save
          lonmax=lonmax_save
          latmin=latmin_save
          latmax=latmax_save

; identify gridpoints within SF bin and in sector that are unmarked
          if lonmin lt lonmax then $
             t=where(lat lt 0.0 and lon ge lonmin and lon le lonmax and $
                     dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)
          if lonmax lt lonmin then $
             t=where(lat lt 0.0 and (lon ge lonmin or lon le lonmax) and $
                     dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)

; require at least 12 gridpoints (2 rows of 4 lons, 2 columns of 3 lats)
          if it lt 12 then goto,jumpshbin

; very first thing that must be done is make sure t gridpoints are
; unimodal in longitude and latitude (no gaps)

; if vortex does not span GM
          if min(lon(t)) gt x(0) or max(lon(t)) lt x(nc-1) then begin
             sindex=sort(lon(t))
             sortedlons=lon(t(sindex))
             jj=findgen(it-1)
             index=where(abs(sortedlons(jj+1)-sortedlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpshbin
             if index(0) ne -1 then begin
                for jj=0,it-2 do begin
                    if abs(sortedlons(jj+1)-sortedlons(jj)) gt dx then begin
                       lonmin=sortedlons(jj+1)
                       lonmax=sortedlons(it-1)
                       goto,jump11
                    endif
                endfor
             endif
             jump11:
          endif 
; if vortex spans GM
          if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
             sindex=sort(lon(t))
             sortedlons=lon(t(sindex))
             index=where(sortedlons gt 180.,itt)
             highlons=sortedlons(index)
             if itt eq 1 then goto,jump22
             jj=findgen(itt-1)
             index=where(abs(highlons(jj+1)-highlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpshbin
             if index(0) ne -1 then begin
                lonmin=min(highlons)
                for jj=0,itt-2 do begin
                    if abs(highlons(jj+1)-highlons(jj)) gt dx then begin
                       lonmin=highlons(jj+1)
                       goto,jump22
                    endif
                endfor
             endif
             jump22:

             index=where(sortedlons lt 180.,itt)
             lowlons=sortedlons(index)
             if itt eq 1 then goto,jump33
             jj=findgen(itt-1)
             index=where(abs(lowlons(jj+1)-lowlons(jj)) gt dx,ii)
             if ii gt 1 then goto,jumpshbin
             if index(0) ne -1 then begin
                lonmax=max(lowlons)
                for jj=0,itt-2 do begin
                    if abs(lowlons(jj+1)-lowlons(jj)) gt dx then begin
                       lonmax=lowlons(jj)
                       goto,jump33
                    endif
                endfor
             endif
             jump33:
          endif

; gridpoints in new longitude bounds
          if lonmin ne lonmin_save or lonmax ne lonmax_save then begin
             if lonmin lt lonmax then $
                t=where(lat lt 0. and lon ge lonmin and lon le lonmax and $
                        dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)
             if lonmax lt lonmin then $
                t=where(lat lt 0. and (lon ge lonmin or lon le lonmax) and $
                        dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)
             if it lt 12 then goto,jumpshbin
             if max(lat(t))-min(lat(t)) lt 4.*dy then goto,jumpshbin
             if max(lon(t))-min(lon(t)) lt 4.*dx then goto,jumpshbin
             if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
                tmpx=lon(t)
                index=where(tmpx lt 180.)
                tmpx(index)=tmpx(index)+360.
                if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpshbin
             endif
          endif

; make sure that latitude distribution is unimodal
          sindex=sort(lat(t))
          sortedlats=lat(t(sindex))
          jj=findgen(it-1)
          index=where(abs(sortedlats(jj+1)-sortedlats(jj)) gt dx,ii)
          if ii gt 1 then goto,jumpshbin
          if index(0) ne -1 then begin
             for jj=0,it-2 do begin
                 if abs(sortedlats(jj+1)-sortedlats(jj)) gt dy*1.1 then begin
                    latmin=sortedlats(0)
                    latmax=sortedlats(jj)
                    goto,jump44
                 endif
             endfor
             jump44:
          endif

; fill bin again if bounds have changed
          if latmin ne latmin_save or latmax ne latmax_save then begin
             if lonmin lt lonmax then $
                t=where(lat gt latmin and lat lt latmax and $
                        lon ge lonmin and lon le lonmax and $
                        dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)
             if lonmax lt lonmin then $
                t=where(lat gt latmin and lat lt latmax and $
                       (lon ge lonmin or lon le lonmax) and $
                        dat lt datbin(n) and dat ge datbin(n+1) and mark eq 0.,it)
             if it lt 12 then goto,jumpshbin
             if max(lat(t))-min(lat(t)) lt 4.*dy then goto,jumpshbin
             if max(lon(t))-min(lon(t)) lt 4.*dx then goto,jumpshbin
             if min(lon(t)) eq x(0) and max(lon(t)) eq x(nc-1) then begin
                tmpx=lon(t)
                index=where(tmpx lt 180.)
                tmpx(index)=tmpx(index)+360.
                if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpshbin
             endif
          endif

; QDF and Zeta integrated along SF isopleth
          avgq = total(qdf(t))/float(it)
          avgz = total(zeta(t))/float(it)

; negative integrated QDF and zeta in SF isopleth
          if avgz lt 0. or avgq gt 0. then goto,jumpshbin

; space fill inside SF>=datbin(n) 
          if lonmin lt lonmax then $
             s=where(lat ge latmin and lat le latmax and $
                     lon ge lonmin and lon le lonmax and $
                     dat le datbin(n) and mark eq 0.,is)
          if lonmax lt lonmin then $ 
             s=where(lat ge latmin and lat le latmax and $
                    (lon ge lonmin or lon le lonmax) and $
                     dat le datbin(n) and mark eq 0.,is)

          if lonmin eq lonmax then goto,jumpshbin

          if is lt 12 then goto,jumpshbin
          if max(lat(s))-min(lat(s)) lt 4.*dy then goto,jumpshbin
          if max(lon(s))-min(lon(s)) lt 4.*dx then goto,jumpshbin

; if line crosses GM the above logic will return anomalous width
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             tmpx=lon(s)
             index=where(tmpx lt 180.)
             tmpx(index)=tmpx(index)+360.
             if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpshbin
          endif

; area integrated QDF and Zeta
          avgq = total(qdf(s))/is
          avgz = total(zeta(s))/is

; must have negative integrated QDF and zeta in area
          if avgz lt 0. or avgq gt 0. then goto,jumpshbin

; TEST LOGIC
; reset lonmax, lonmin, latmin, and latmax to edges of s gridpoints.  then
; expand and see if the extra gridpoints included still touch boundaries
if min(lon(s)) ne x(0) or max(lon(s)) ne x(nc-1) then begin
   lonmin=min(lon(s))
   lonmax=max(lon(s))
endif
if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
   index=where(lon(s) gt 180.)
   lonmin=min(lon(s(index)))
   index=where(lon(s) lt 180.)
   lonmax=max(lon(s(index)))
endif
if lonmin lt 0. then lonmin=lonmin+360.
if lonmax gt 360. then lonmax=lonmax-360.
latmin=min(lat(s))
latmax=max(lat(s))

; previously marked SH vortices
          pr=where(lat lt 0. and mark ne 0.,ip)

; make sure gridpoints do not touch sector edge or previously marked vortex
          lonmin0=lonmin
          lonmax0=lonmax
          latmin0=latmin
          latmax0=latmax
          if lonmax0 gt lonmin0 then width=lonmax0-lonmin0
          if lonmin0 gt lonmax0 then width=(360.-lonmin0)+lonmax0
          while min(abs(lon(s)-lonmin0)) le dx or $
                min(abs(lon(s)-lonmax0)) le dx or $
                (min(abs(lat(s)-latmin0)) le dy and latmin0 ne -90.) or $
                (min(abs(lat(s)-latmax0)) le dy and latmax0 ne 0.) do begin

; expand sector longitudes. Do not mark directly next to sector edges
                if min(abs(lon(s)-lonmin0)) le dx then lonmin0=lonmin0-10.
                if lonmin0 lt x(0) then lonmin0=lonmin0+360.
                if min(abs(lon(s)-lonmax0)) le dx then lonmax0=lonmax0+10.
                if lonmax0 gt x(nc-1) then lonmax0=lonmax0-360.
; expand sector latitudes
                if min(abs(lat(s)-latmin0)) le dy then latmin0=latmin0-10.
                if latmin0 lt -90. then latmin0=-90.
                if min(abs(lat(s)-latmax0)) le dy then latmax0=latmax0+10.
                if latmax0 gt 0. then latmax0=0.

; do not mark if sector edge is directly next to previously marked vortices
                if pr(0) ne -1 then begin
                   if min(abs(lon(pr)-lonmin0)) le dx or $
                      min(abs(lon(pr)-lonmax0)) le dx then begin
                      goto,jumpshbin
                   endif
                endif

; area fill with new sector boundaries
                if lonmin0 lt lonmax0 then $
                   s=where(lat ge latmin0 and lat le latmax0 and $
                           lon ge lonmin0 and lon le lonmax0 and $
                           dat le datbin(n) and mark eq 0.,is)
                if lonmin0 gt lonmax0 then $
                   s=where(lat ge latmin0 and lat le latmax0 and $
                          (lon ge lonmin0 or lon le lonmax0) and $
                           dat le datbin(n) and mark eq 0.,is)

; if after adjusting longitude nothing is marked...
                if s(0) eq -1 then goto,jumpshbin

; monitor cumulative degrees of expansion.  allow sector to be up to 180 degrees wide
                if lonmax0 gt lonmin0 then width=lonmax0-lonmin0
                if lonmin0 gt lonmax0 then width=(360.-lonmin0)+lonmax0
                if width gt 180. then goto,jumpshbin
          endwhile
          lonmin=lonmin0
          lonmax=lonmax0
          latmin=latmin0
          latmax=latmax0

; jump bin if canditate marks touch previously marked vortex
          if pr(0) ne -1 then $
             for jj=0,is-1 do $
                 if min(abs(lon(pr)-lon(s(jj)))) le dx and $
                    min(abs(lat(pr)-lat(s(jj)))) le dy then goto,jumpshbin

; make sure that longitude distribution is unimodal

; if vortex does not span GM
          if min(lon(s)) gt x(0) or max(lon(s)) lt x(nc-1) then begin
             sindex=sort(lon(s))
             sortedlons=lon(s(sindex))
             for jj=0,is-2 do begin
                 if abs(sortedlons(jj+1)-sortedlons(jj)) gt dx then begin
                    lonmin=sortedlons(jj+1)
                    lonmax=sortedlons(is-1)
                    goto,shjumpout1
                 endif
             endfor
             shjumpout1:
          endif

; if vortex spans GM then check modality of longitudes on either side of GM 
; separately.  First check "high longitudes"
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             sindex=sort(lon(s))
             sortedlons=lon(s(sindex))
             index=where(sortedlons gt 180.,is)
             highlons=sortedlons(index)
             lonmin=min(highlons)
             for jj=0,is-2 do begin
                 if abs(highlons(jj+1)-highlons(jj)) gt dx then begin
                    lonmin=highlons(jj+1)
                    goto,shjumpoutgm1
                 endif
             endfor
             shjumpoutgm1:

; then check "low longitudes"
             index=where(sortedlons lt 180.,is)
             lowlons=sortedlons(index)
             lonmax=max(lowlons)
             for jj=0,is-2 do begin
                 if abs(lowlons(jj+1)-lowlons(jj)) gt dx then begin
                    lonmax=lowlons(jj)
                    goto,shjumpoutgm2
                 endif
             endfor
             shjumpoutgm2:
          endif			; GM logic

; now make sure that the latitude distribution is unimodal
          sindex=sort(lat(s))
          sortedlats=lat(s(sindex))
          for jj=0,is-2 do begin
              if abs(sortedlats(jj+1)-sortedlats(jj)) gt dy*1.1 then begin
                 latmin=sortedlats(0)
                 latmax=sortedlats(jj)
                 goto,shjumpout2
              endif
          endfor
          shjumpout2:

          if lonmin lt lonmax then $
             s=where(lat gt latmin and lat le latmax and $
                     lon ge lonmin and lon le lonmax and $
                     dat le datbin(n) and mark eq 0.,is)
          if lonmax lt lonmin then $
             s=where(lat gt latmin and lat le latmax and $
                    (lon ge lonmin or lon le lonmax) and $
                     dat le datbin(n) and mark eq 0.,is)

          if is lt 12 then goto,jumpshbin
          if max(lat(s))-min(lat(s)) lt 4.*dy then goto,jumpshbin
          if max(lon(s))-min(lon(s)) lt 4.*dx then goto,jumpshbin

; if line crosses GM the above logic will return anomalous width
          if min(lon(s)) eq x(0) and max(lon(s)) eq x(nc-1) then begin
             tmpx=lon(s)
             index=where(tmpx lt 180.)
             tmpx(index)=tmpx(index)+360.
             if max(tmpx)-min(tmpx) lt 4.*dx then goto,jumpshbin
          endif

          avgq = total(qdf(s))/is
          avgz = total(zeta(s))/is

; negative area-integrated QDF enclosed by SF isopleth in stretched longitudinal sector
          if avgz lt 0. or avgq gt 0. then goto,jumpshbin

; if you've made it this far then mark the sucker
          mark(s)=-1.0*(imark+1.0)
;          print,'SH ',imark,'MARKED EDGE ',datbin(n),' BIN # ',n
;erase
;!type=2^2+2^3
;map_set,0,180,0,/contin,/grid,/noeras
;contour,dat,x,y,levels=reverse(datbin),/noeras,/overplot
;contour,dat,x,y,levels=[datbin(n+1),datbin(n)],/noeras,/overplot,thick=5
;index=where(mark lt 0. and lat lt 0.)
;if index(0) ne -1 then oplot,lon(index),lat(index),psym=4,color=.9*255
;stop
          imark=imark+1
          goto, jumpshsect
          jumpshbin:
      endfor		; loop over SF isopleths

      jumpshsect:
      isect=isect+1

; current number of marked gridpoints in NH
      s=where(lat lt 0. and mark ne 0.,nmark1)

; monitor cumulative degrees of rotation
      if nmark1 eq nmark0 then dloop=dloop+30.
      if nmark1 gt nmark0 then begin
         if lonmin lt lonmax then dloop=dloop+(lonmax-lonmin)
         if lonmin gt lonmax then dloop=dloop+(360.-lonmin)+lonmax
      endif

; set new sector bounds : rotate by 30 degrees if nothing was marked
; set lonmin=lonmax if an anticyclone was marked (no overlap)
      if nmark1 eq nmark0 then lonmin=lonmin+30.
      if nmark1 gt nmark0 then lonmin=lonmax
      if lonmin gt x(nc-1) then lonmin=lonmin-360.
      lonmax=lonmin+dsect
      if lonmax gt x(nc-1) then lonmax=lonmax-360.

; retain number of gridpoints marked
      nmark0=nmark1
ENDWHILE		; loop over longitude sectors

return
end
