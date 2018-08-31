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

orbitlist=['49914'];,'49873','49873','49861','49899','49884','49884','49883','49873','49903','49904','49908','49909','49914','49923','49923','49924','49928','49940','49955','49958','49914','49873','49858']
iscene = [14];,12,13,14,13,13,14,14,13,13,13,13,12,14,12,13,13,14,13,13,14,14,12,13]


for cipsplot = 0,1 do begin
	for airsplot = 0,1 do begin
		if cipsplot eq 0 and airsplot eq 0 then continue
		
		
		;-------------------------------------READ IN CIPS DATA-------------------------------------------
		for iorbit = 0, n_elements(orbitlist) - 1L do begin
			if orbitlist[iorbit] eq '49884' then continue
			dir = '/Users/franceja/CIPS_GW_paper/Data/'
			file=FILE_SEARCH('/Users/franceja/CIPS_GW_paper/Data/ray_gw_dat/cips_ray_gw_north_2016_'+orbitlist[iorbit]+'*.sav',COUNT=nfiles)
			;EXTRACT DATE, ORBIT, LAT/LON INFORMATION FROM FILENAME
			Date2DOY, strmid(file,strlen(dir)+29,4)+strmid(file,strlen(dir)+40,4), CIPS_doy
			orbitnum = strmid(file,strlen(dir)+34,5)
			restore, file
			CIPS_frac_day = float(strmid(file,strlen(dir)+44,3))
			CIPS_time = 24.*float(strmid(file,strlen(dir)+44,3))
			scenetimestr = strmid(scene.ORBIT_START_TIME_UT,9,5)
			scenetime = float(strmid(scenetimestr,0,2)) + float(strmid(scenetimestr,3,2))/100.; This determines scene time in fraction of hour
			; look at AIRS data from 0-12Z or 12-24Z
			if scenetime lt 12. then ut_airs=.25
			if scenetime ge 12. then ut_airs=.75
			
			
			;-------------------------------------READ IN AIRS DATA-------------------------------------------
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
			;	       
			;	       
			;-------------------------------------END: READ IN AIRS DATA-------------------------------------------
		
			; strip out CIPS scene data
			scenestr = string(iscene[iorbit],format = '(I2.2)')
			alb_pert_cips = scene.alb[*,*,iscene[iorbit]]
			lon_cips = scene.lon[*,*,iscene[iorbit]]
			lat_cips = scene.lat[*,*,iscene[iorbit]]
		
			;UNIT CIRCLE TRIGONOMETRY TO GET AVERAGE LONGITUDE
			coslon = cos(!dtor*(lon_cips))
			sinlon = sin(!dtor*(lon_cips))
			;x and y of unit cirle:
			x= mean(coslon,/nan)
			y= mean(sinlon,/nan)
			;x value indicates absolute value of longitude between 0 and 180, sign of y indicates what hemisphere
			cips_lon_mean = acos(x)/!dtor * (y/abs(y)) ; determine x component on unit circle and y direction
			cips_lat_mean = mean(lat_cips,/nan)
		
		
		
			;Reform AIRS data
			AIRS_frac_day_all = (time-mean(time) + 43200.)/86400.; Julday to fraction of day
			; Flag measurements within 6 hours of CIPS observations and near the CIPS scene
			data = where(abs(airs_frac_day_all - ut_airs) le .25 and abs(lat-cips_lat_mean) lt 40. $
			and (abs(lon - cips_lon_mean) lt 50. or $
			abs(lon+360. - cips_lon_mean) lt 50. or $
			abs(lon-360. - cips_lon_mean) lt 50.),nx) 
			lats = reform(lat[data])
			lons = reform(lon[data])
			times = reform(AIRS_frac_day_all[data])
			x=where(finite(times) eq 0. or times lt 0.,nx)
			if nx gt 0L then times[x] = !values.f_nan
			AIRS_frac_day = mean(times,/nan); Julday to fraction of day
	
			; Determine range of conincidence times from AIRS
			AIRS_coin = where(abs(lats-cips_lat_mean) lt 10. and abs(lons-cips_lon_mean) lt 5.,nx)
			if nx eq 0L then stop
			AIRS_coin_time_min =24.*min(times[airs_coin])
			AIRS_coin_time_max =24.*max(times[airs_coin])
			AIRS_coin_time_min_minutes = 60.*(AIRS_coin_time_min - floor(AIRS_coin_time_min))
			AIRS_coin_time_max_minutes = 60.*(AIRS_coin_time_max - floor(AIRS_coin_time_max))
					
			
			; Set file and plot titles				
			if cipsplot eq 1 and airsplot eq 1 then begin
				AIRS_plot_data = reform(BT_4MU_pt[data])
	
				plot_title = 'AIRS 4mu High BT Pert, CIPS Albedo Pert!C'$
				+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+$
				'Z, AIRS coin time: '+string(airs_coin_time_min,format='(i2.2)')+':'+string(AIRS_coin_time_min_minutes,format = '(i2.2)')+'-'+$
				string(airs_coin_time_max,format='(i2.2)')+':'+string(AIRS_coin_time_max_minutes,format = '(i2.2)')+'Z'
	
				filename = 'AIRS_T_pt_+CIPS_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
					+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
					+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_4mu.png'
			endif
			IF cipsplot eq 0L then begin
				AIRS_plot_data = reform(BT_4MU_pt[data])
	
				plot_title = 'AIRS 4mu High BT Pert!C'$
				+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+$
				'Z, AIRS coin time: '+string(airs_coin_time_min,format='(i2.2)')+':'+string(AIRS_coin_time_min_minutes,format = '(i2.2)')+'-'+$
				string(airs_coin_time_max,format='(i2.2)')+':'+string(AIRS_coin_time_max_minutes,format = '(i2.2)')+'Z'
	
				filename = 'AIRS_T_pt_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_4mu.png'
			endif
				
			IF airsplot eq 0 then begin
				AIRS_plot_data = reform(BT_4MU_pt[data])
				plot_title = 'CIPS Albedo Pert!C'$
				+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+'Z'
	
				filename = 'CIPS_pt_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'.png'
			endif
			
	
	
			;-------------PLOT CODE--------------
			erase
			!type=2^2+2^3
			plot, [0,0],[0,0],xstyle = 4, ystyle =  4,$
			position = [.01,.01,.99,.96], charsize = 1.125,thick = 4 ; style= 4 supresses axis
			xyouts, .5,.91,plot_title,align=.5,charsize = 1.6
			loadct,0
	
			limit = [cips_lat_mean-20,cips_lon_mean-30,cips_lat_mean+20,cips_lon_mean+30]; Set plot limits based on the mean CIPS lat/lon
	
			Map_Set, cips_lat_mean, cips_lon_mean, /Satellite,  /NoErase, position = [.1,.1,.9,.9],$
			/ISOTROPIC, color=0, limit = limit, /horizon,$
			/CONTINENTS
	
			;Set AIRS levels
			level1 = (findgen(11)*.2-1.)*3.
			if orbitlist[iorbit] eq '49885' then level1 = level1/10.
			if orbitlist[iorbit] eq '49883' then level1 = level1*1.5
			if orbitlist[iorbit] eq '49858' or orbitlist[iorbit] eq '49873' or orbitlist[iorbit] eq '49883' or orbitlist[iorbit] eq '49958' then level1 = level1/2.
			if orbitlist[iorbit] eq '49928' then level1 = (findgen(11)*.2-1.)*5.
			nlvls  = n_elements(level1)
			col1 = [250,210,190,170,125,125,80,60,40,20,0]
			
			loadct,68
			IF airsplot eq 1 then begin
				;plot individual measurements within the lat/lon limits
				for i = 0, n_elements(AIRS_plot_data) - 1L do begin
					if lats[i] gt limit[0] and lats[i] lt limit[2] and ((lons[i] gt limit[1] and lons[i] lt (limit[3] +10)) $
						or (lons[i]+360. gt limit[1] and lons[i]+360. lt (limit[3]+10)) $
						or (lons[i]-360. gt limit[1] and lons[i]-360. lt (limit[3]+10))) then begin
							x = where(AIRS_plot_data[i]-level1 gt 0L,nx)
							if nx ge 1 then color = col1[x[-1]]
							;xyouts,lons[i],lats[i], '.',align = .5,color = color
							oplot,[lons[i],lons[i]],[lats[i],lats[i]], psym=8,color = color,symsize = .2
					endif
				endfor
			endif
	
			loadct,70
			;Set CIPS levels
			level2=[-5,-4,-3,-2,-1,0,1,2,3,4,5]
			nlvls2  = n_elements(level2)
			col2 = [250,220,190,170,125,125,100,80,60,30,0]
			if orbitlist[iorbit] eq '49885' then levels=levels/5.
			if orbitlist[iorbit] eq '49928' then levels=levels*2.
	
			if cipsplot eq 1 then begin
				contour, smooth(alb_pert_cips,1,/nan), lon_cips,lat_cips, /overplot, $
				levels = levels2,c_color = col2,/noerase, /close,/cell_fill,  color = 0,c_thick = 1
			endif
	
			;Plot outline of CIPS scene
			x = where(finite(alb_pert_cips))
			datalocations = lat_cips*0.
			datalocations[x] =10.
			loadct,0
			contour, datalocations,lon_cips,lat_cips,level=[.1],color = 250, /overplot,/noerase,/close,c_color = 0, c_thick = 4
	
			if orbitlist[iorbit] eq '49885' then begin
				Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
				/ISOTROPIC, color=0, $
				/CONTINENTS,/horizon,  limit = limit,/USA	
			endif
			Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
			/ISOTROPIC, color=0, $
			/CONTINENTS,/horizon,  limit = limit
			MAP_GRID, /LABEL, LATLAB=floor(cips_lon_mean/15.)*15.-30., LONLAB=floor(cips_lat_mean/10.)*10+17, LATDEL=5, LONDEL=10
	
			
	
			if CIPSplot eq 1 then begin
	
				; -----------------plot the CIPS color bar-----------------------
				
				level = level2
				!type=2^2+2^3+2^6			; no y title or ticsks
				imin=min(level)
				imax=max(level)
				slab=' '+strarr(n_elements(level))
				
				!p.title = ' '
				if airsplot eq 0 then position = [.3,0.09, .7,.17]
				if airsplot eq 1 then position = [.52,0.09, .9,.17]
				plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level2)-1L,$
					position = position,xstyle=1,xtickname=slab,xtitle= 'Albedo Perturbation (%)'
				loadct, 70
				
				ybox=[0,10,10,0,0]
				
				x2=0
				for j=1,n_elements(col2)-1 do begin
					dx= 1./(n_elements(level)-1.)
					xbox=[x2,x2,x2+dx,x2+dx,x2]
					polyfill,xbox,ybox,color=col2[j-1]
					x2=x2+dx
				endfor
				
				loadct,0
				slab=strcompress(string(format='(f8.3)',level),/remove_all)
				slabcolor = fltarr(n_elements(level))*0.
				slabcolor[0:2] = 255
				x1=dx/2
				
				for i=0L,n_elements(slab)-2L do begin
					slab0=slab[i]
					flab0=float(slab[i])
					slab0=strcompress(string(format='(f7.2)',flab0),/remove_all)
					xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5
					x1=x1+dx
				endfor
		
			endif
			if AIRSplot eq 1 then begin
			
				; -----------------plot the AIRS color bar-----------------------
				level = level1
				!type=2^2+2^3+2^6			; no y title or ticsks
				imin=min(level)
				imax=max(level)
				slab=' '+strarr(n_elements(level))
				
				!p.title = ' '
				if cipsplot eq 0 then position = [.3,0.09, .7,.17]
				if cipsplot eq 1 then position = [.1,0.09, .48,.17]
				plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level1)-1L,$
					position = position,xstyle=1,xtickname=slab,xtitle= 'BT Perturbation (K)'
				loadct, 68
				
				ybox=[0,10,10,0,0]
				
				x2=0
				for j=1,n_elements(col1)-1 do begin
					dx= 1./(n_elements(level)-1.)
					xbox=[x2,x2,x2+dx,x2+dx,x2]
					polyfill,xbox,ybox,color=col1[j-1]
					x2=x2+dx
				endfor
				
				loadct,0
				slab=strcompress(string(format='(f8.3)',level),/remove_all)
				slabcolor = fltarr(n_elements(level))*0.
				slabcolor[0:2] = 255
				x1=dx/2
				
				for i=0L,n_elements(slab)-2L do begin
					slab0=slab[i]
					flab0=float(slab[i])
					slab0=strcompress(string(format='(f7.2)',flab0),/remove_all)
					xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5
					x1=x1+dx
				endfor
				
			endif
			
			if plottype eq 1 then begin
				device, /close
				spawn, $
					'gs -dBATCH -sDEVICE=png16m -r300 -dNOPAUSE -sOutputFile=/Users/franceja/CIPS_GW_paper/Figures/'+filename+' idl.ps'
			endif
		endfor
	endfor
endfor
end