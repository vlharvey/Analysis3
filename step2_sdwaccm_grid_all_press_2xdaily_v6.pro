; save SDWACCM v6 CO, T, Z data (saved like MLS) on lat/lon grid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 'c_cesm2_fswd_2005_cntrl.cam.hs.MLS.'+syr+smn+sdy+'.sav
;
@kgmt

loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
!p.background=icolmax
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.10,0.55]
yorig=[0.3,0.3]
xlen=0.4
ylen=0.4
cbaryoff=0.1
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

MLS_path='/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_v6/'
output_path=MLS_path

;LONGITUDE (lon)
lon=dindgen(144L)*2.5
nlon=n_elements(lon) & lonstep=(lon[1:*]-lon[0:nlon-2])[nlon/2]

;LATITUDE (lat)
lat=dindgen(96L)* 1.89474 -90.0
nlat=n_elements(lat) & latstep=(lat[1:*]-lat[0:nlat-2])[nlat/2]

; time range
start_date='20050101'
end_date='20141231'
ndays=julday(strmid(end_date,4,2),strmid(end_date,6,2),strmid(end_date,0,4)) - $
      julday(strmid(start_date,4,2),strmid(start_date,6,2),strmid(start_date,0,4))+1L
current_date = start_date
start_julday = julday(strmid(start_date,4,2),strmid(start_date,6,2),strmid(start_date,0,4))
;
; loop over days
;
for iday=0,ndays-1 do begin
;
; month, day, year of this jday
;
    current_julday = start_julday + iday
    caldat, current_julday, month, day, year
    date_string = strmid(strtrim(strcompress(year),2),0,4)+strtrim(string(format='(I2.2)',month),2)+strtrim(string(format='(I2.2)',day),2)
    print,date_string
;
; read file. skip if input file does not exist or if gridded file already exists
;
    spawn,'ls '+MLS_path+'c_cesm2_fswd_2005_cntrl.cam.hs.MLS.*'+date_string+'.sav',tpfiles
    dum=findfile(output_path+'SDWACCM_grid5_ALL_U_V_v6_'+date_string+'.sav')
;   if tpfiles(0) eq '' or dum(0) ne '' then goto,jumpday
    if tpfiles(0) eq '' then goto,jumpday
;
; CO              FLOAT     = Array[3494, 37]
; DATE            LONG      = Array[3494]
; LATITUDE        DOUBLE    = Array[3494]
; LONGITUDE       DOUBLE    = Array[3494]
; PMLS            FLOAT     = Array[37]
; PMLS2           FLOAT     = Array[55]
; T               FLOAT     = Array[3494, 55]
; TIME            FLOAT     = Array[3494]
; Z               FLOAT     = Array[3494, 55]
;
    result10=size(tpfiles)
    if result10(0) ne -1 then restore,tpfiles(0)
;
; extract some catalog information 
;
    mprof=n_elements(latitude)
    mlev=n_elements(pmls)
    mlev2=n_elements(pmls2)
;
; filter out any data? maybe use pressure range
;
    comls=CO
    comask=0.*comls
    cobad=where(pmls gt 215. or pmls lt 0.0046)                   ; do not use outside Pressure range 215-0.0046 hPa
    if cobad[0] ne -1L then comask[*,cobad]=-99.

    gpmls=Z
    gpmask=0.*gpmls

    tpmls=T
    tpmask=0.*tpmls
    tpbad=where(pmls2 gt 261. or pmls2 lt 0.001)                 ; do not use outside Pressure range: 261-0.001 hPa
    if tpbad[0] ne -1L then tpmask[*,tpbad]=-99.
    if tpbad[0] ne -1L then gpmask[*,tpbad]=-99.
;
    index=where(longitude lt 0. and longitude ne -999.990)
    if index[0] ne -1L then longitude(index)=longitude(index)+360.
;
; build fractional day from day of year and UT time
;
    fdoy=0.*time
    sdate=strcompress(date,/remove_all)
    for i=0L,mprof-1L do begin
        kyr=long(strmid(sdate(i),0,4))
        kmn=long(strmid(sdate(i),4,2))
        kdy=long(strmid(sdate(i),6,2))
        z = kgmt(kmn,kdy,kyr,kday)
        fdoy(i)=float(kday-1)+time(i)/24.
    endfor
;
; CALCULATE LOCAL TIME AND LOCAL DAY.  THE TIME DIFFERENCE FROM TIME, dH, IS THE EAST
; LONGITUDE MULTIPLIED BY 24 HOURS/360 DEGREES.  0 LONGITUDE IS UTC, 180 LONGITUDE IS
; INTERNATIONAL DATE LINE.  THEREFORE, THE LOCAL TIME CAN BE CALCULATED AS:
;     0 LE LON LE 180:  LTIME=UT+dH.  IF LT>24 THEN LT=LT-24 AND LOCAL DAY=DAY+1.
;   180 LT LON LT 360:  LTIME=UT-dH.  IF LT<0  THEN LT=LT+24 AND LOCAL DAY=DAY-1.
;
    LDAY=FIX(FDOY) & LTIME=TIME
    L=LONGITUDE
    X=WHERE(L GT 180.,NX)
    IF NX GT 0 THEN L(X)=360.-L(X)    ; L is the Delta Longitude from 0, ranging from 0-180.
    X=WHERE(LONGITUDE LE 180. and LONGITUDE ne -999.990,NX)
    IF NX GT 0 THEN LTIME(X)=TIME(X)+L(X)*24./360.
    X=WHERE(LONGITUDE GT 180.,NX)
    IF NX GT 0 THEN LTIME(X)=TIME(X)-L(X)*24./360.
    X=WHERE(LTIME GT 24.,NX)
    IF NX GT 0 THEN BEGIN
      LTIME(X)=LTIME(X)-24.
      LDAY(X)=fix(FDOY(X))+1
    ENDIF
    X=WHERE(LTIME LT 0.,NX)
    IF NX GT 0 THEN BEGIN
       LTIME(X)=LTIME(X)+24.
       LDAY(X)=fix(FDOY(X))-1
    ENDIF
;
; calculate geometric altitude from geopotential height
; use geometric altitude to interpolate data from pressure levels to SOSST altitude grid
;
     ks=1.931853d-3
     ecc=0.081819
     gamma45=9.80
     rtd=double(180./!pi)
     dtr=1./rtd
     zmls=0.*gpmls
     gpkm=gpmls/1000.
     for j=0L,mprof-1L do begin
         sin2=sin( (latitude[j]*dtr)^2.0 )
         numerator=1.0+ks*sin2
         denominator=sqrt( 1.0 - (ecc^2.0)*sin2 )
         gammas=gamma45*(numerator/denominator)
         r=6378.137/(1.006803-(0.006706*sin2))
         zmls[j,*]= (r*gpkm[j,*])/ ( (gammas/gamma45)*r - gpkm[j,*] )
     endfor
     index=where(gpmls eq -999.990)
     if index[0] ne -1L then zmls(index)=-999.990
;
; produce ascending/descending flag
; 
      lat_gradient=convol(latitude,[-1,0,1],/normalize,/center,/nan,/edge_truncate)
;     node=1*(lat_gradient gt 0)-1*(lat_gradient lt 0) ;1 = ascending, -1 = descending, 0 = neither		; separate by node into 2 LTs

morning=where(time le 12.,comp=evening)
UT_6Z_18Z = time*0L
UT_6Z_18Z[morning]=-1
UT_6Z_18Z[evening]=1
node=1*(lat_gradient gt 0)-1*(lat_gradient lt 0) ;1 = ascending, -1 = descending, 0 = neither			; separate into 2 UT - average these to minimize tides
;  
      x=where(latitude lt -100 or ltime lt 0)
      ltime[x]=!values.f_nan
      latitude[x]=!values.f_nan
      x=where(node eq 1)
;device,decompose=0
;loadct,0
;plot, ltime[x],latitude[x],xtitle='local time',ytitle='Latitude',yrange = [-90,90],xrange = [0,24],charsize=2,ystyle=1,yticks=6,title='MLS '+date_string,psym=2,color=250,/nodata
;loadct,39
;oplot, ltime[x],latitude[x],psym=2,color=100
;x=where(node eq -1)
;oplot, ltime[x],latitude[x],color = 250,psym=4
;stop

      x = where(gpmls lt 0.,nx)
      if nx gt 0L then gpmls[x] = !values.f_nan
;check for coordinates out of range
      index=where(longitude lt 0 or longitude gt 360,nindex)
      if nindex ne 0 then longitude[index]=!values.f_nan
      index=where(latitude lt -90 or latitude gt 90,nindex)
      if nindex ne 0 then latitude[index]=!values.f_nan
      index=where(finite(longitude) ne 1 or finite(latitude) ne 1,nindex)
;  produce sunlight flag
      earth_radius=6367500. ;in m
      sza_sunset=180.-180./!pi*asin(earth_radius/(earth_radius))
;sunlight=transpose(rebin(sza,n_elements(sza),mlev2)) lt sza_sunset  ;1 = sunlight, 0 = darkness
;  
; GRID DATA

	lev = pmls
	nlev = n_elements(lev)
	CO_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	CO_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	x = where(COmls lt 0. or COmask eq -99.,nx)
	if x(0) ne -1L then COmls[x] = !values.f_nan
	value = COmls
	ntimes = n_elements(COmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  
        index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(COmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(COmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: CO_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: CO_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	CO_grid=mean(CO_grid_node,dim=4,/nan)

	lev = pmls2
	nlev = n_elements(lev)
	gp_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	gp_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	x = where(gpmls lt 0. or gpmask eq -99.,nx)
	if x(0) ne -1L then gpmls[x] = !values.f_nan
	value = gpmls
	ntimes = n_elements(gpmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  
        index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(gpmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(gpmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: gp_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: gp_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	gp_grid=mean(gp_grid_node,dim=4,/nan)

	lev = pmls2
	nlev = n_elements(lev)
	tp_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	tp_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	x = where(tpmls lt 0. or tpmask eq -99.,nx)
	if x(0) ne -1L then tpmls[x] = !values.f_nan
	value = tpmls
	ntimes = n_elements(tpmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  
        index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(tpmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(tpmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: tp_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: tp_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	tp_grid=mean(tp_grid_node,dim=4,/nan)

 co = 	co_grid_node
 gph = 	gp_grid_node
 t = 	tp_grid_node
  
;Derive geostophic winds

 U =gp_grid_node*0.
 V = gp_grid_node*0.
 dZx =gp_grid_node[*,*,*,0]*0.
 dZy = gp_grid_node[*,*,*,0]*0.
 dZys = fltarr(n_elements(lon) + 5L, n_elements(lat), n_elements(pmls2))
 
 R = 6378100. ;radius of the earth (m)
 g = 9.81 ; gravit (m/s^2)
 omega = 7.292*(10.^(-5.))
 f = 2.*omega*sin(!DtoR*lat)
 
 dlat = 2.*!Pi*R/360. * (lat[1]-lat[0])
 dlon = 2.*!Pi*R/360.*cos(!DtoR*lat) * (lon[1]-lon[0])
 
;create wraparound for derivatives
 gp_grid_nodes = fltarr(n_elements(lon) + 5L, n_elements(lat), n_elements(pmls2),2)
 gp_grid_nodes[0:n_elements(lon)-1L,*,*,*] = gp_grid_node
 gp_grid_nodes[n_elements(lon):n_elements(lon)+4L,*,*,*] = gp_grid_node[0L:4L,*,*,*]
 lons = fltarr(n_elements(lon) + 5L)
 lons[0:n_elements(lon)-1L] = lon
 lons[n_elements(lon):n_elements(lon)+4L] = lon[0L:4L]
 
 for kk = 0L, n_elements(pmls2) - 1L do begin
 	for ii = 0L, n_elements(lons) - 1L do dZys[ii,*,kk] = deriv(smooth(gp_grid_nodes[ii,*,kk,0],5,/nan,/edge_truncate))
 	for ii = 0L, n_elements(lat) - 1L do dZx[*,ii,kk] = deriv(smooth(gp_grid_node[*,ii,kk,0],5,/nan,/edge_truncate))
 	
;Fix wraparound at Greenwhich Meridian
 	dZy[*,*,kk] = dzys[0:n_elements(lon)-1,*,kk]
 	dzy[0,*,kk] = dzys[n_elements(lon),*,kk]
 	dzy[1,*,kk] = dzys[n_elements(lon)+1,*,kk]
 	
; Calculate geostrophic winds at each lat/lon
 	for ii = 0L, n_elements(lon) - 1L do begin
            U[ii,*,kk,0] = -(g/f)*(dZy[ii,*,kk]/dlat)
            V[ii,*,kk,0] = (g/f)*(dZx[ii,*,kk]/dlon[*])
 	endfor
 	for ii = 0L, n_elements(lons) - 1L do dZys[ii,*,kk] = deriv(smooth(gp_grid_nodes[ii,*,kk,1],5,/nan,/edge_truncate))
 	for ii = 0L, n_elements(lat) - 1L do dZx[*,ii,kk] = deriv(smooth(gp_grid_node[*,ii,kk,1],5,/nan,/edge_truncate))
 	
;Fix wraparound at Greenwhich Meridian
 	dZy[*,*,kk] = dzys[0:n_elements(lon)-1,*,kk]
 	dzy[0,*,kk] = dzys[n_elements(lon),*,kk]
 	dzy[1,*,kk] = dzys[n_elements(lon)+1,*,kk]
 	
; Calculate geostrophic winds at each lat/lon
 	for ii = 0L, n_elements(lon) - 1L do begin
            U[ii,*,kk,1] = -(g/f)*(dZy[ii,*,kk]/dlat)
            V[ii,*,kk,1] = (g/f)*(dZx[ii,*,kk]/dlon[*])
 	endfor
 endfor
 
UT_mean=[6,18]
 
if mprof lt 3150L then save, filename = output_path+'SDWACCM_grid5_ALL_U_V_v6_'+date_string+'.sav.bad',lon,lat,co,gph,t,pmls,pmls2,u,v,UT_mean
if mprof ge 3150L then begin
   save, filename = output_path+'SDWACCM_grid5_ALL_U_V_v6_'+date_string+'.sav',lon,lat,co,gph,t,pmls,pmls2,u,v,UT_mean
;;
;; check
;;
;erase
;mcolor=byte(!p.color)
;device,decompose=0
;mcolor=byte(!p.color)
;xmn=xorig(0)
;xmx=xorig(0)+xlen
;ymn=yorig(0)
;ymx=yorig(0)+ylen
;set_viewport,xmn,xmx,ymn,ymx
;!type=2^2+2^3
;nlvls=26L
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;ilev=25
;xyouts,.3,.8,date_string+' '+strcompress(pmls2(ilev),/r)+' hPa',color=0,/normal,charsize=2,charthick=2
;;map_set,90,0,-90,/ortho,/contin,/grid,/noerase,color=0,charsize=1.5,title='SD-WACCM from MLS'
;map_set,0,0,0,/contin,/grid,/noerase,color=0,charsize=1.5,title='SD-WACCM from MLS'
;dum=reform(gph(*,*,ilev,0))/1000.
;;dum=reform(t(*,*,ilev,0))
;;dum=reform(co(*,*,ilev,0))
;nc=n_elements(lon)
;nr=n_elements(lat)
;lon1=[lon,lon(0)+360.]
;dum1=fltarr(nc+1,nr)
;dum1(0:nc-1,0:nr-1)=dum
;dum1(nc,*)=dum1(0,*)
;dum1save=dum1
;index=where(dum1 ne 0.)
;imin=min(dum1(index))-0.05*min(dum1(index))
;imax=max(dum1(index))+0.05*max(dum1(index))
;level=imin+((imax-imin)/float(nlvls-1L))*findgen(nlvls)
;;level=-120.+10.*findgen(nlvls)
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,c_color=col1,/cell_fill,/overplot
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,color=0,/foll,/overplot,thick=2
;;oplot,longitude,latitude,psym=1
;map_set,0,0,0,/contin,/grid,/noerase,color=mcolor
;;map_set,90,0,-90,/ortho,/contin,/grid,/noerase,color=mcolor
;dum=reform(u(*,*,ilev,0))
;;contour,dum,lon,lat,/overplot,levels=[20,40,60,80,100,210],c_colors=[50,90,130,180,220,250],thick=3,/foll
;;contour,dum,lon,lat,/overplot,levels=-100+20.*findgen(4),color=mcolor,c_linestyle=5,thick=3,/foll
;;
;; plot original WACCM
;;
;restore,MLS_path+'c_cesm2_fswd_2005_cntrl.cam.h1.'+date_string+'.sav'
;;
;; COGRD           FLOAT     = Array[144, 96, 88]
;; LAT             DOUBLE    = Array[96]
;; LEV             DOUBLE    = Array[88]
;; LON             DOUBLE    = Array[144]
;; NO2GRD          FLOAT     = Array[144, 96, 88]
;; NOGRD           FLOAT     = Array[144, 96, 88]
;; O3GRD           FLOAT     = Array[144, 96, 88]
;; PGRD            FLOAT     = Array[144, 96, 88]
;; TGRD            FLOAT     = Array[144, 96, 88]
;; UGRD            FLOAT     = Array[144, 96, 88]
;; VGRD            FLOAT     = Array[144, 96, 88]
;; ZGRD            FLOAT     = Array[144, 96, 88]
;;
;; interpolate to pmls2(ilev)
;;
;tlev=fltarr(n_elements(lon),n_elements(lat))
;gplev=fltarr(n_elements(lon),n_elements(lat))
;ulev=fltarr(n_elements(lon),n_elements(lat))
;colev=fltarr(n_elements(lon),n_elements(lat))
;for j=0L,n_elements(lat)-1L do begin
;for i=0L,n_elements(lon)-1L do begin
;    tprof=reform(TGRD(i,j,*))
;    zprof=reform(ZGRD(i,j,*))/1000.
;    uprof=reform(UGRD(i,j,*))
;    coprof=reform(COGRD(i,j,*))
;    pprof=reform(PGRD(i,j,*))
;    tlev(i,j)=interpol(tprof,alog(pprof),alog(pmls2(ilev)))
;    gplev(i,j)=interpol(zprof,alog(pprof),alog(pmls2(ilev)))
;    ulev(i,j)=interpol(uprof,alog(pprof),alog(pmls2(ilev)))
;    colev(i,j)=interpol(coprof,alog(pprof),alog(pmls(ilev)))
;endfor
;endfor
;
;;index=where(abs(lev-pmls2(ilev)) eq min(abs(lev-pmls2(ilev))))		; don't interpolate
;;gplev=ZGRD(*,*,index(0))/1000.
;
;xmn=xorig(1)
;xmx=xorig(1)+xlen
;ymn=yorig(1)
;ymx=yorig(1)+ylen
;set_viewport,xmn,xmx,ymn,ymx
;;map_set,90,0,-90,/ortho,/contin,/grid,/noerase,color=0,charsize=1.5,title='original SD-WACCM'
;map_set,0,0,0,/contin,/grid,/noerase,color=0,charsize=1.5,title='original SD-WACCM'
;dum1=fltarr(nc+1,nr)
;dum1(0:nc-1,0:nr-1)=gplev
;;dum1(0:nc-1,0:nr-1)=tlev
;;dum1(0:nc-1,0:nr-1)=colev
;dum1(nc,*)=dum1(0,*)
;dum2save=dum1
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,c_color=col1,/cell_fill,/overplot
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,color=0,/foll,/overplot,thick=2
;;map_set,90,0,-90,/ortho,/contin,/grid,/noerase,color=mcolor
;map_set,0,0,0,/contin,/grid,/noerase,color=mcolor
;dum=ulev
;;contour,dum,lon,lat,/overplot,levels=[20,40,60,80,100,210],c_colors=[50,90,130,180,220,250],thick=3,/foll
;;contour,dum,lon,lat,/overplot,levels=-100+20.*findgen(4),color=mcolor,c_linestyle=5,thick=3,/foll
;
;set_viewport,.1,.9,.05,.25 
;plot,lat,mean(DUM1SAVE,dim=1),color=0,thick=7,yrange=[min(dum1save)-0.5,max(dum1save)+0.5],xtitle='Latitude',ytitle='GPH'
;oplot,lat,mean(dum2save,dim=1),color=240,thick=4
;xyouts,.2,.1,'MLS locations',/normal,color=0,charsize=2,charthick=2
;xyouts,.2,0.075,'Original',/normal,color=240,charsize=2,charthick=2
;print,date_string,' ',mean(mean(DUM1SAVE,dim=1)-mean(dum2save,dim=1))
;wait,1
endif
;
jumpday:
endfor		;end of iday loop
end
