@/Users/franceja/IDL_files/standard_scripts
@/Users/franceja/IDL_files/wavelet/wave_signif
@/Users/franceja/IDL_files/date2doy

plottype=1
pre_plot,plottype
device,decompose=0

x1=[.1,.4,.7]
x2=[.35,.65,.95]
y1=[.53,.2]
y2=[.8,.47]


cips_doy_old = 0



for iloop = 0, 2 do begin
skip=0
;opens and gets all the variables from the .nc files
;file='era40_ua_12Z_19850101.nc'
findfiles=FILE_SEARCH('/Users/franceja/CIPS_analysis/save_gw_scene_north_2016_*',COUNT=nfiles)
for ifile = 0, nfiles - 1L do begin
	CIPS_lat = float(strmid(findfiles[ifile],72,3))
	CIPS_lon = strmid(findfiles[ifile],76,4)
	Date2DOY, '2016'+strmid(findfiles[ifile],61,4), CIPS_doy
	if cips_doy_old ne cips_doy then begin
		cips_doy_old = cips_doy
		CIPS_frac_day = float(strmid(findfiles[ifile],65,3))
		path='/Users/franceja/AIRS_data/airs_pert_jun2016/data/'
		ncfile=path+'pert_airs_l1_2016_'+CIPS_doy+'.nc'
		
		
		;Open the *.nc file
		 ncid=ncdf_open(ncfile)
		 result=ncdf_inquire(ncid)   ;Inquire about the data
		 nvars=result.nvars        ;# variables in the file
		 ;Read in the data
		for ivar=0,nvars-1 do begin
				result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
				;Puts data into array called "data" and variable name into "result.name":
				ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
			    
			 ; Invoke some magic to check for string data
			 ; masquerading as byte data, but don't convert
			 ; byte data blindly, i.e., quality_flags is a 2-dimensional
			 ; array of byte data. 
			 ; (This is done to account for a bug in the ncdf write routine.)
				  if ( ( size( data, /n_dimensions )  EQ 1 ) && $
				    ( size( data, /type ) EQ 1 ) ) then $
				    data = string( data )
			
			 ;Extract each variable from the "data" structure and name it
			;  the corresponding "name" from "result.name":
			       if Execute(result.name + ' = data') eq 0 then $
			       Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            
		endfor
		       PRINT, RESULT.NAME
		       NCDF_CLOSE, ncid
		       
	;      	  BT_15MU_HIGH    FLOAT     = Array[90, 32400]
	;	       BT_15MU_HIGH_PT FLOAT     = Array[90, 32400]
	;	       BT_15MU_HIGH_VAR FLOAT     = Array[90, 32400]
	;	       BT_15MU_LOW     FLOAT     = Array[90, 32400]
	;	       BT_15MU_LOW_PT  FLOAT     = Array[90, 32400]
	;	       BT_15MU_LOW_VAR FLOAT     = Array[90, 32400]
	;	       BT_4MU          FLOAT     = Array[90, 32400]
	;	       BT_4MU_PT       FLOAT     = Array[90, 32400]
	;	       BT_4MU_VAR      FLOAT     = Array[90, 32400]
	;	       BT_8MU          FLOAT     = Array[90, 32400]
	;	       DATA            FLOAT     = Array[90, 32400]
	;	       LAT             DOUBLE    = Array[90, 32400]
	;	       LON             DOUBLE    = Array[90, 32400]
	;	       TIME            DOUBLE    = Array[90, 32400]
	
	endif
	x=where(finite(time) eq 0. or time lt 0.,nx)
	if nx gt 0L then time[x] = !values.f_nan
	AIRS_frac_day = (time-mean(time) + 43200.)/86400.; Julday to fraction of day
x = where(abs(airs_frac_day - CIPS_frac_day) le .25 and abs(lat-cips_lat) lt 25. and (abs(lon - cips_lon) lt 30. or abs(lon+360. - cips_lon) lt 90.),nx) ; measurements within 6 hours of CIPS observations

	if iloop eq 0 then begin
		VAR15 = reform(BT_15MU_low_VAR[x])
		plot_title = 'AIRS 15mu Low Var -- lat:'+string(CIPS_lat,format='(i4.2)')+' lon:'+ string(CIPS_lon,format='(i4.2)')+' DOY:'+$
			string(CIPS_doy,format='(i4.2)')+strmid(string(cips_frac_day,format='(f5.3)'),1,3)
		filename = 'AIRS_variance_'+strtrim(string(cips_lat,format = '(i4.2)'),2)+'lat_'$
			+strtrim(string(cips_lon,format = '(i5.3)'),2)+'lon_DOY_'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+'_15mu_low.png
	endif
	if iloop eq 1 then begin
		VAR15 = reform(BT_15MU_high_VAR[x])
		plot_title = 'AIRS 15mu High Var -- lat:'+string(CIPS_lat,format='(i4.2)')+' lon:'+ string(CIPS_lon,format='(i4.2)')+' DOY:'+$
			string(CIPS_doy,format='(i4.2)')+strmid(string(cips_frac_day,format='(f5.3)'),1,3)
		filename = 'AIRS_variance_'+strtrim(string(cips_lat,format = '(i4.2)'),2)+'lat_'$
			+strtrim(string(cips_lon,format = '(i5.3)'),2)+'lon_DOY_'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+'_15mu_high.png
	endif
	if iloop eq 2 then begin
		VAR15 = reform(BT_4MU_VAR[x])
		plot_title = 'AIRS 4mu Var -- lat:'+string(CIPS_lat,format='(i4.2)')+' lon:'+ string(CIPS_lon,format='(i4.2)')+' DOY:'+$
			string(CIPS_doy,format='(i4.2)')+strmid(string(cips_frac_day,format='(f5.3)'),1,3)
		filename = 'AIRS_variance_'+strtrim(string(cips_lat,format = '(i4.2)'),2)+'lat_'$
			+strtrim(string(cips_lon,format = '(i5.3)'),2)+'lon_DOY_'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+'_4mu.png
	endif
	lats = reform(lat[x])
	lons = reform(lon[x])
	help, var15
	
nlat = 91
gridlat = findgen(nlat)*(180./(nlat-1)) -90.
nlon = 181
gridlon = findgen(nlon)*(360./(nlon-1)) -180.
gridvar = fltarr(nlon,nlat)*!values.f_nan
for ilat = 0, nlat-1 do begin
	for ilon = 0, nlon-1 do begin
		x=where(abs(lats -gridlat[ilat]) le 2. and abs(lons-gridlon[ilon]) lt 5.,nx)
		if nx gt 0 then gridvar[ilon,ilat] = mean(var15[x],/nan)
	endfor
endfor


erase
!type=2^2+2^3

plot, [0,0],[0,0],xstyle = 4, ystyle =  4,$
position = [.01,.01,.99,.96], charsize = 1.125,thick = 4 ; style= 4 supresses axis

loadct,0
Map_Set, cips_lat, cips_lon, /Satellite, SAT_P=[1.1,0.0,0.0], /NoErase, position = [.1,.1,.9,.9],$
	/ISOTROPIC, color=0, $
	/CONTINENTS, TITLE=plot_title
;	
;Center of projection = 41.5N latitude, –74W longitude
;P (altitude) = 1.025 = (1.0 + 160 / 6371km)
;Gamma (rotation of projection plane) = 150 degrees
;Omega (tilt of projection plane) = 0 degrees
;The eight element LIMIT keyword array specifies the latitude/longitude locations of points at the bottom, left, top, and right of the map respectively.
;The HORIZON keyword draws a horizon line.
loadct,39

level1 = [0,.02,.04,.08,.1,.15,.2,.25,.3,.35,.4,.5,1]/10.
nlvls  = n_elements(level1)
col1 = (1 + indgen(nlvls)) * 250. / nlvls	; define colors

contour, gridvar, gridlon,gridlat, /overplot, levels=level1, /cell_fill, c_color = col1,/noerase,$;
	/follow, /close, color = 0

	
	loadct,0
Map_Set, cips_lat, cips_lon, /Satellite, SAT_P=[1.1,0.0,0.0],  /NoErase, position = [.1,.1,.9,.9],$
	/ISOTROPIC, color=0, $
	/CONTINENTS,/horizon, /grid
	xyouts, cips_lon,CIPS_lat, 'x', charsize =1, color = 0,align = .5
lonlab = findgen(36)*10.-180.
latlab = findgen(39)*5.-90.

for i = 0, n_elements(latlab) - 1L do xyouts, floor(cips_lon/15.)*15, latlab, string(latlab,format = '(i3.2)'),align = .5
for i = 0, n_elements(lonlab) - 1L do xyouts, lonlab,floor(cips_lat/10.)*10,  string(lonlab,format = '(i4.3)'),align = .5
	
	loadct,39
	

;print, max(meanGEOS5strats)
;plot the color bar
!type=2^2+2^3+2^6			; no y title or ticsks
imin=min(level1)
imax=max(level1)
slab=' '+strarr(n_elements(level1))

!p.title = ' '
plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level1)-1L,$
	position = [.1,0.09, .9,.17],xstyle=1,xtickname=slab,xtitle= 'Variance (K^2)'
	
ybox=[0,10,10,0,0]

x2=0
for j=1,n_elements(col1)-1 do begin
	dx= 1./(n_elements(col1)-1)
	xbox=[x2,x2,x2+dx,x2+dx,x2]
	polyfill,xbox,ybox,color=col1(j-1)
	x2=x2+dx
endfor

slab=strcompress(string(format='(f7.3)',level1),/remove_all)
slabcolor = fltarr(n_elements(level1))*0.
slabcolor[0:5] = 255

slabcolor = fltarr(n_elements(level1))*0.
slabcolor[0:5] = 255
x1=dx/2 + dx

for i=1L,n_elements(slab)-1L do begin
	slab0=slab(i)
	flab0=float(slab(i))
	slab0=strcompress(string(format='(f7.3)',flab0),/remove_all)
	xyouts,x1-dx/2,5,slab0,charsize=1.4,/data,color=slabcolor[i],charthick=4, orientation= 90., align = .5
	x1=x1+dx
endfor

if plottype eq 1 then begin
	device, /close
	spawn, $
		'gs -dBATCH -sDEVICE=png16m -r300 -dNOPAUSE -sOutputFile=/Users/franceja/CIPS_GW_paper/Figures/'+filename+' idl.ps'
endif


endfor
endfor; iloop
end