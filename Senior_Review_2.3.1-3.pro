@/Users/franceja/IDL_files/standard_scripts
@/Users/franceja/IDL_files/wavelet/wave_signif
@/Users/franceja/IDL_files/date2doy
@drawvectors

plottype=1
pre_plot,plottype
device,decompose=0

x1=[.1,.4,.7]
x2=[.35,.65,.95]
y1=[.53,.2]
y2=[.8,.47]

mdir='/atmos/harvey/MERRA2_data/Datfiles/MERRA2-on-WACCM_press_'
stime=['00','06','12','18']
icount=0L

orbitlist=[$
;'49885',$	; Squall
;'49899',$	; Non-oro
'49883'$	; Oro
;'49877'$	; Island
]
iscene=[$
;'7',$
;'13',$
'14'$
;'13'$
]

help, iscene, orbitlist
for cipsplot = 0,1 do begin
for airsplot = 0,1 do begin
if cipsplot eq 0 and airsplot eq 0 then continue

;----Lynn Code for MERRA wind------------	

	
		;-------------------------------------READ IN CIPS DATA-------------------------------------------
		for iorbit = 0, n_elements(orbitlist) - 1L do begin
			if orbitlist[iorbit] eq '49884' then continue
		file=FILE_SEARCH('/Users/franceja/CIPS_GW_paper/Data/ray_gw_dat/cips_ray_gw_north_2016_'+orbitlist[iorbit]+'*.sav',COUNT=nfiles)
			;EXTRACT DATE, ORBIT, LAT/LON INFORMATION FROM FILENAME
                        dum=strsplit(file,'_',/extract)
                        syear=dum(-4)
                        dum2=strsplit(dum(-2),'.',/extract)
                        monday=dum2(0)
			Date2DOY, syear+monday, CIPS_doy
			orbitnum = dum(-3)	;strmid(file,strlen(dir)+34,5)
			restore, file
			CIPS_frac_day = float(dum2(1))/100.	;float(strmid(file,strlen(dir)+44,3))
			CIPS_time = 24.*CIPS_frac_day		;float(strmid(file,strlen(dir)+44,3))

orbitstarttimestr=strmid(scene.ORBIT_START_TIME_UT,9,8)                                                                                                         ; extract HH:MM:SS
orbitstartsecs=float(strmid(orbitstarttimestr,0,2))*60.*60. + float(strmid(orbitstarttimestr,3,2))*60. + float(strmid(orbitstarttimestr,6,2))                   ; convert orbit start to seconds

timesinceorbitstart=(scene.time - scene.orbit_start_time )/1000000.                                                                                             ; seconds since orbit start
scenesecs=timesinceorbitstart + orbitstartsecs                                                                                                                  ; add seconds since to orbit start

scenetime=(scenesecs(iscene[iorbit])/86400.)*24.                                                                                                                                ; scene time in frac hours
scenemins=long((scenetime-long(scenetime))*60.)
scenetimestr=string(format = '(I2.2)',long(scenetime))+':'+string(format = '(I2.2)', scenemins)

			; look at AIRS data from 0-12Z or 12-24Z
			if scenetime lt 12. then ut_airs=.25
			if scenetime ge 12. then ut_airs=.75

; read MERRA2 at 2 bounding times and interpolate to scenetime
sdate=syear+monday
fracday=scenetime/24.
iyr=long(syear)
imn=long(strmid(monday,0,2))
idy=long(strmid(monday,2,2))
doy=julday(imn,idy,iyr)-julday(1,1,iyr)+1
jday=doy+julday(1,1,iyr)-1
if fracday lt 0.25 then begin
   jday0=jday-1.0
   jday1=jday
   stime0='18'
   stime1='00'
   tscale=(fracday-0.0)/0.25
endif
if fracday ge 0.25 and fracday lt 0.5 then begin
   jday0=jday
   jday1=jday
   stime0='06'
   stime1='12'
   tscale=(fracday-0.25)/0.25
endif
if fracday ge 0.5 and fracday lt 0.75 then begin
   jday0=jday
   jday1=jday
   stime0='12'
   stime1='18'
   tscale=(fracday-0.5)/0.25
endif
if fracday ge 0.75 then begin
   jday0=jday
   jday1=jday+1.0
   stime0='18'
   stime1='00'
   tscale=(fracday-0.75)/0.25
endif
    caldat,jday0,imn0,idy0,iyr0         ; mon, day, year corresponding to jday0
    caldat,jday1,imn1,idy1,iyr1         ; mon, day, year corresponding to jday1

    ifile0=string(FORMAT='(i4.4,i2.2,i2.2)',iyr0,imn0,idy0)+stime0+'.sav'
    restore,mdir+ifile0
alon=LONGITUDE_WACCM
nc=n_elements(alon)
alat=LATITUDE_WACCM
nr=n_elements(alat)
psold=PSGRD	;            FLOAT     = Array[144, 96]
told=TGRD	;            FLOAT     = Array[144, 96, 41]
uold=UGRD	;            FLOAT     = Array[144, 96, 41]
vold=VGRD	;            FLOAT     = Array[144, 96, 41]
zold=ZGRD	;            FLOAT     = Array[144, 96, 41]
    
    ifile1=string(FORMAT='(i4.4,i2.2,i2.2)',iyr1,imn1,idy1)+stime1+'.sav'
    restore,mdir+ifile1
psnew=PSGRD       ;            FLOAT     = Array[144, 96]
tnew=TGRD       ;            FLOAT     = Array[144, 96, 41]
unew=UGRD       ;            FLOAT     = Array[144, 96, 41]
vnew=VGRD       ;            FLOAT     = Array[144, 96, 41]
znew=ZGRD       ;            FLOAT     = Array[144, 96, 41]

print,monday,' ',scenetime
print,ifile0
print,ifile1

;
; perform time interpolation
;
    ps2=psold+TSCALE*(psnew-psold)
    u2=uold+TSCALE*(unew-uold)
    v2=vold+TSCALE*(vnew-vold)

      if icount eq 0 then begin
         press=100
;        print,pressure
;        read,'Enter pressure level ',press
         index=where(press eq pressure)
         if index(0) eq -1 then stop,'Invalid theta level '
		ipress=-1;index(0)
		icount=1
      endif
      press=pressure(ipress)
      spress=strcompress(string(press),/remove_all)
      u1=reform(u2(*,*,ipress))
      v1=reform(v2(*,*,ipress))
      sp1=sqrt(u1^2.+v1^2.)
      ps=0.*fltarr(nc+1,nr)
      ps(0:nc-1,0:nr-1)=ps2(0:nc-1,0:nr-1)
      ps(nc,*)=ps(0,*)
      u=0.*fltarr(nc+1,nr)
      u(0:nc-1,0:nr-1)=u1(0:nc-1,0:nr-1)
      u(nc,*)=u(0,*)
      v=0.*fltarr(nc+1,nr)
      v(0:nc-1,0:nr-1)=v1(0:nc-1,0:nr-1)
      v(nc,*)=v(0,*)
      sp=0.*fltarr(nc+1,nr)
      sp(0:nc-1,0:nr-1)=sp1(0:nc-1,0:nr-1)
      sp(nc,*)=sp(0,*)
      alon2=fltarr(nc+1)
      alon2(0:nc-1)=alon
      alon2(nc)=alon2(0)+360.

;----------------------------------------------------------------			





;-------------------------------------READ IN CIPS DATA-------------------------------------------
	if orbitlist[iorbit] eq '49884' then continue
	dir = '/Users/franceja/CIPS_GW_paper/Data/'
	file=FILE_SEARCH('/Users/franceja/CIPS_GW_paper/Data/ray_gw_dat/cips_ray_gw_north_2016_'+orbitlist[iorbit]+'*.sav',COUNT=nfiles)
	;EXTRACT DATE, ORBIT, LAT/LON INFORMATION FROM FILENAME
	Date2DOY, strmid(file,strlen(dir)+29,4)+strmid(file,strlen(dir)+40,4), CIPS_doy
	orbitnum = strmid(file,strlen(dir)+34,5)
	restore, file
			
			timesinceorbitstart = (scene.time[iscene[iorbit]] - scene.ORBIT_START_TIME)/1000000.D
			firstscenetimestr = strmid(scene.ORBIT_START_TIME_UT,9,5)
			firstscenesec = float(strmid(firstscenetimestr,0,2))*3600. + float(strmid(firstscenetimestr,3,2))*60.+ float(strmid(firstscenetimestr,6,2))
			timesec = timesinceorbitstart + firstscenesec
			timehour = string(timesec/3600.,format = '(i2.2)')
			timemin = string((timesec/3600. - floor(timesec/3600.)) * 60.,format = '(i2.2)')
		
			
		print, timehour+':'+timemin


			scenetime = float(timehour)
			scenetimehourStr = strmid(strtrim(string(scenetime,format = '(f7.2)'),2),0,2)
			scenetimeminStr = string((scenetime - floor(scenetime)) * 60., format = '(i2.2)')
			scenetimestr = timehour + ':' + timemin

			
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

	; Loop over each scene
	scenestr = string(iscene[iorbit],format = '(I2.2)')
	alb_pert_cips = scene.alb[*,*,iscene[iorbit]]
	lon_cips = scene.lon[*,*,iscene[iorbit]]
	lat_cips = scene.lat[*,*,iscene[iorbit]]
	sza_cips = scene.sza[*,*,iscene[iorbit]]
	x = where(sza_cips/!dtor ge 90.,nx)
	if nx gt 0 then alb_pert_cips[x] = -99.

	;UNIT CIRCLE TRIGONOMETRY TO GET AVERAGE LONGITUDE IN CASE VALUES WRAP AROUND
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
		data = where(abs(airs_frac_day_all - ut_airs) le .25 and abs(lat-cips_lat_mean) lt 40. $
		and (abs(lon - cips_lon_mean) lt 50. or $
		abs(lon+360. - cips_lon_mean) lt 50. or $
		abs(lon-360. - cips_lon_mean) lt 50.),nx) ; Flag measurements within 6 hours of CIPS observations
		lats = reform(lat[data])
		lons = reform(lon[data])
		times = reform(AIRS_frac_day_all[data])
		x=where(finite(times) eq 0. or times lt 0.,nx)
		if nx gt 0L then times[x] = !values.f_nan
		AIRS_frac_day = mean(times,/nan); Julday to fraction of day


		AIRS_coin = where(abs(lats-cips_lat_mean) lt 10. and abs(lons-cips_lon_mean) lt 5.,nx)
		if nx eq 0L then stop
		AIRS_coin_time_min =24.*min(times[airs_coin])
		AIRS_coin_time_max =24.*max(times[airs_coin])
		AIRS_coin_time_min_minutes = 60.*(AIRS_coin_time_min - floor(AIRS_coin_time_min))
		AIRS_coin_time_max_minutes = 60.*(AIRS_coin_time_max - floor(AIRS_coin_time_max))


		print, 24.* mean(times[airs_coin]),scenetime
		
		for iloop = 2, 2 do begin
		
		if cipsplot eq 1 and airsplot eq 1 then begin
		if iloop eq 0 then begin
			AIRS_plot = reform(BT_15MU_low_pt[data])
			plot_title = 'AIRS 15mu Low BT Pert + CIPS Albedo Anomaly !C'$
			+', DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+string(CIPS_time,format='(i2.2)')+'Z, AIRS coin time: '+string(airs_coin_time,format='(i2.2)')+'Z'
			filename = 'AIRS_BT_perturbation_+CIPS_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_15mu_low_wind.png'
		;	level1 =  [-5.,0.,5.]
		endif
		if iloop eq 1 then begin
			AIRS_plot = reform(BT_15MU_high_pt[data])
			plot_title = 'AIRS 15mu High BT Pert + CIPS Albedo Anomaly !C'$
			+', DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+string(CIPS_time,format='(i2.2)')+'Z, AIRS coin time: '+string(airs_coin_time,format='(i2.2)')+'Z'
			filename = 'AIRS_T_perturbation_CIPS_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_15mu_high_wind.png'
			level1 =  [-1.5,0.,1.5]
		endif
		if iloop eq 2 then begin
			AIRS_plot = reform(BT_4MU_pt[data])
			level1 = (findgen(11)*.2-1.)
			if orbitlist[iorbit] eq '49885' then level1 = level1/10.
			if orbitlist[iorbit] eq '49883' then level1 = level1*2.
			if orbitlist[iorbit] eq '49858' or orbitlist[iorbit] eq '49873' or orbitlist[iorbit] eq '49883' or orbitlist[iorbit] eq '49958' then level1 = level1/2.
			if orbitlist[iorbit] eq '49877' then level1 = level1/5.
			if orbitlist[iorbit] eq '49928' then level1 = (findgen(11)*.2-1.)*5.

;			level1 = findgen(21)*.1 - 1.
			plot_title = 'AIRS 4mu High BT Pert, CIPS Albedo Anomaly!C'$
			+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+'Z, AIRS coin time: '+string(airs_coin_time_min,format='(i2.2)')+':'+string(AIRS_coin_time_min_minutes,format = '(i2.2)')+'-'+$
			string(airs_coin_time_max,format='(i2.2)')+':'+string(AIRS_coin_time_max_minutes,format = '(i2.2)')+'Z'
			filename = 'AIRS_T_perturbation_+CIPS_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_4mu_wind.png'
				endif
		endif
		
		
		IF cipsplot eq 0L then begin
			if iloop eq 2 then begin
			AIRS_plot = reform(BT_4MU_pt[data])
			level1 = (findgen(11)*.2-1.)
			if orbitlist[iorbit] eq '49885' or orbitlist[iorbit] eq '49892' then level1 = level1/5.
			if orbitlist[iorbit] eq '49877' then level1 = level1/5.
 			if orbitlist[iorbit] eq '49883' then level1 = level1*1.5
			if orbitlist[iorbit] eq '49858' or orbitlist[iorbit] eq '49873' or orbitlist[iorbit] eq '49883' or orbitlist[iorbit] eq '49958' then level1 = level1/2.
			if orbitlist[iorbit] eq '49928' then level1 = (findgen(11)*.2-1.)*5.
;				level1 = (findgen(21)*.2-2.)/10.
				plot_title = 'AIRS 4mu High BT Pert!C'$
			+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+'Z, AIRS coin time: '+string(airs_coin_time_min,format='(i2.2)')+':'+string(AIRS_coin_time_min_minutes,format = '(i2.2)')+'-'+$
			string(airs_coin_time_max,format='(i2.2)')+':'+string(AIRS_coin_time_max_minutes,format = '(i2.2)')+'Z'
			filename = 'AIRS_T_perturbation_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_4mu_wind.png'
			endif
		endif
			
		IF airsplot eq 0 then begin
			if iloop eq 2 then begin
				AIRS_plot = reform(BT_4MU_pt[data])
				plot_title = 'CIPS Albedo Anomaly!C'$
			+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+'Z'
			filename = 'CIPS_anomaly_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_wind.png'
			endif
		endif
		
		help, AIRS_plot
print, filename

		erase
		!type=2^2+2^3
		
		x = where(finite(alb_pert_cips))
		datalocations = lat_cips*0.
		datalocations[x] =10.
		dateline = 0
		limit = [cips_lat_mean-10,cips_lon_mean-15,cips_lat_mean+10,cips_lon_mean+15]
		
		plot, [0,0],[0,0],xstyle = 4, ystyle =  4,$
		position = [.01,.01,.99,.96], charsize = 1.125,thick = 4 ; style= 4 supresses axis
		xyouts, .5,.91,plot_title,align=.5,charsize = 1.6
;		xyouts, .5,.15,'AIRS BT Anomaly > '+string(level1[2],format = '(f3.1)')+'K'+$
;			', CIPS Albedo Anomaly > 1%' ,align=.5,charsize = 1.6
		loadct,0
		Map_Set, cips_lat_mean, cips_lon_mean, /Satellite,  /NoErase, position = [.1,.1,.9,.9],$
			/ISOTROPIC, color=0, limit = limit, /horizon,$
			/CONTINENTS, MLINETHICK = 4 
;MAP_CONTINENTS [, /COASTS] [, COLOR=index] [, /CONTINENTS] [, /COUNTRIES] $
;	[ ,FILL_CONTINENTS={1 | 2}[, ORIENTATION=value]] [, /HIRES] [, LIMIT=vector]$
;	 [, MAP_STRUCTURE=structure] [, MLINESTYLE={0 | 1 | 2 | 3 | 4 | 5}] [, MLINETHICK=value] [, /RIVERS] [, SPACING=centimeters] [, /USA]

if orbitlist[iorbit] ne '49885' then MAP_CONTINENTS, fill_continents=1,color = 150

		;	
		;Center of projection = 41.5N latitude, –74W longitude
		;P (altitude) = 1.025 = (1.0 + 160 / 6371km)
		;Gamma (rotation of projection plane) = 150 degrees
		;Omega (tilt of projection plane) = 0 degrees
		;The eight element LIMIT keyword array specifies the latitude/longitude locations of points at the bottom, left, top, and right of the map respectively.
		;The HORIZON keyword draws a horizon line.
		
		nlvls  = n_elements(level1)
		col1 = reverse(1 + indgen(nlvls)) * 250. / nlvls	; define colors
;		x = where(level1 lt 0.)
		col1 = [250,210,190,170,125,125,80,60,40,20,0]
		
		
		;contour, dataplot, lons,lats, /overplot, levels=level1, /cell_fill, c_color = col1,/noerase,$;
		;	/follow, /close, color = 0
		loadct,68

		IF airsplot eq 1 then begin
			;plot individual measurements within the lat/lon limits
			for i = 0, n_elements(AIRS_plot) - 1L do begin
				if lats[i] gt limit[0] and lats[i] lt limit[2] and ((lons[i] gt limit[1] and lons[i] lt (limit[3] +10)) $
					or (lons[i]+360. gt limit[1] and lons[i]+360. lt (limit[3]+10)) $
					or (lons[i]-360. gt limit[1] and lons[i]-360. lt (limit[3]+10))) then begin
						x = where(airs_plot[i]-level1 gt 0L,nx)
						if nx ge 1 then color = col1[x[-1]]
						;xyouts,lons[i],lats[i], '.',align = .5,color = color
						oplot,[lons[i],lons[i]],[lats[i],lats[i]], psym=8,color = color,symsize = .3
				endif
			endfor
		endif
		loadct,70

		levels2=[-12,-8,-4,-3,-2,-1,0,1,2,3,4,8,12,13];
		col2 = [250,235,220,190,170,125,125,100,80,60,30,15,10,0]
;		levels2 = findgen(21)*1.5 - 15.
;		nlvls2  = n_elements(level2)
;		col1 = reverse(1 + indgen(nlvls2)) * 250. / nlvls	; define colors
		if orbitlist[iorbit] eq '49885' then levels2=[-10,-8,-6,-4,-3,-2,-1,0,1,2,3,4,6,8]/5.
		if orbitlist[iorbit] eq '49928' then levels2=[-10,-8,-6,-4,-3,-2,-1,0,1,2,3,4,6,8]*2.
		if orbitlist[iorbit] eq '49892' then levels2=[-10,-8,-6,-4,-3,-2,-1,0,1,2,3,4,6,8]


		if cipsplot eq 1 then begin
			contour, smooth(alb_pert_cips,1,/nan), lon_cips,lat_cips, /overplot, $
				levels = levels2,c_color = col2,/noerase, /close,/cell_fill,  color = 0,c_thick = 1
		endif
		loadct,0

		;Plot outline of CIPS scene
		contour, datalocations,lon_cips,lat_cips,level=[.1],color = 250, /overplot,/noerase,/close,c_color = 0, c_thick = 8

if orbitlist[iorbit] eq '49885' then begin
	Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
		/ISOTROPIC, color=0, $
		/CONTINENTS,/horizon,  limit = limit,/USA, MLINETHICK = 4
endif
Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
		/ISOTROPIC, color=0, $
		/CONTINENTS,/horizon,  limit = limit, MLINETHICK = 4
		MAP_GRID, /LABEL, LATLAB=-88., LONLAB=-44., LATDEL=5, LONDEL=10, charsize = 2

		
if orbitlist[iorbit] eq '49877' then MAP_CONTINENTS, fill_continents=1,color = 150

;drawvectors,nc+1,nr,alon2,alat,u,v,20,1

lonarr = findgen(33)*.25-75.
latarr = findgen(33)*((53-47.)/32.)-53.
if cipsplot eq 1 and airsplot eq 1 then oplot, lonarr, latarr, color = 0, thick = 14
npts = 5

		if CIPSplot eq 1 then begin

			; -----------------plot the color bar-----------------------
			if orbitlist[iorbit] eq '49885' then levels2 = levels2/5.
			if orbitlist[iorbit] eq '49928' then levels2 = levels2*2.

			;COL2 = [250,220,190,170,125,125,100,80,60,30,0]
			
			
			level = levels2
			!type=2^2+2^3+2^6			; no y title or ticsks
			imin=min(level)
			imax=max(level)
			slab=' '+strarr(n_elements(level))
			
			!p.title = ' '
			if airsplot eq 0 then position = [.1,0.09, .9,.17]
			if airsplot eq 1 then position = [.52,0.09, .9,.17]
			plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level)-1L,$
				position = position,xstyle=1,xtickname=slab
				
				xyouts, .5, position[1]-5.,'CIPS Albedo Anomaly (%)',charsize = 1.3,charthick = 2,align=.5
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
		
			; -----------------plot the color bar-----------------------
			level = level1
			!type=2^2+2^3+2^6			; no y title or ticsks
			imin=min(level)
			imax=max(level)
			slab=' '+strarr(n_elements(level))
			
			!p.title = ' '
			if cipsplot eq 0 then position = [.1,0.09, .9,.17]
			if cipsplot eq 1 then position = [.1,0.09, .48,.17]
			plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level)-1L,$
				position = position,xstyle=1,xtickname=slab

				xyouts, .5, position[1]-5.,'AIRS BT Perturbation (K)',charsize = 1.3,charthick = 2,align=.5

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
				'gs -dBATCH -sDEVICE=png16m -r300 -dNOPAUSE -sOutputFile=/Users/franceja/CIPS_GW_paper/Figures/Figure-2.3.1-2.png idl.ps'
		endif
		
	endfor; loop over AIRS products
endfor
endfor
endfor
end