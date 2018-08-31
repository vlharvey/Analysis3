;
; read CIPS and AIRS and plot coincidences
;
cips_dir='/atmos/harvey/CIPS_data/Datfiles/RAA/'
airs_dir='/atmos/aura3/data/AIRS_data/Datfiles/data/'
icount=0L

a=findgen(8)*(2*!pi/8.)
usersym,1.5*cos(a),1.5*sin(a),/fill
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
xorig=[0.15]
yorig=[0.15]
xlen=0.8
ylen=0.8
cbaryoff=0.06
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

orbitlist=['50504']
iscene=['4']

cipsplot=1
airsplot=1
;
; Loop over CIPS orbits listed in "orbitlist"
;
for iorbit = 0, n_elements(orbitlist) - 1L do begin
;
;-------------------------------------READ IN CIPS DATA (cat, alb, ang)-------------------------------------------
;
    cipsfiles=FILE_SEARCH(cips_dir+'cips_raa_2a_orbit_'+orbitlist[iorbit]+'*.nc',COUNT=nfiles)
    for ifile=0L,nfiles-1L do begin
        file=cipsfiles(ifile)
        dum=strsplit(file,'_',/extract)
        syear=strmid(dum(-4),0,4)
        CIPS_doy=strmid(dum(-4),5,3)
        orbitnum = dum(-5)
        ncid=ncdf_open(file)
        result=ncdf_inquire(ncid)   ;Inquire about the data
        nvars=result.nvars        ;# variables in the file
        for ivar=0,nvars-1 do begin
            result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
            ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
            if ( ( size( data, /n_dimensions )  EQ 1 ) && $
                 ( size( data, /type ) EQ 1 ) ) then $
                   data = string( data )
            if Execute(result.name + ' = data') eq 0 then $
               Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
        endfor
        NCDF_CLOSE, ncid
    endfor
;-------------------------------------END: READ IN CIPS DATA-------------------------------------------

    caldat,CIPS_doy+julday(1,1,long(syear))-1,m,d,y									; convert doy and year to month,day,year
    sdate=strcompress(y,/r)+string(format='(i2.2)',m)+string(format='(i2.2)',d)

    scenetime=UT_TIME(iscene[iorbit])                                                                               ; scene time in frac hours
    scenemins=long((scenetime-long(scenetime))*60.)                                                                 ; minutes for label
    scenetimestr=string(format = '(I2.2)',long(scenetime))+':'+string(format = '(I2.2)', scenemins)
    print,'scene ',iscene[iorbit],UT_TIME(iscene[iorbit]),' ',scenetimestr
    CIPS_frac_day = scenetime/24.
    if scenetime lt 12. then ut_airs=.25
    if scenetime ge 12. then ut_airs=.75
;
;-------------------------------------READ IN AIRS DATA on same day as CIPS-------------------------------------------
;
    ncfile=airs_dir+'pert_airs_l1_'+syear+'_'+CIPS_doy+'.nc'
    ncid=ncdf_open(ncfile)
    result=ncdf_inquire(ncid)   ;Inquire about the data
    nvars=result.nvars        ;# variables in the file
    for ivar=0,nvars-1 do begin
        result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if ( ( size( data, /n_dimensions )  EQ 1 ) && $
             ( size( data, /type ) EQ 1 ) ) then $
               data = string( data )
        if Execute(result.name + ' = data') eq 0 then $
           Print, ' "Execute" command failed -- are you in Virtual Machine mode?'
    endfor
    NCDF_CLOSE, ncid
;
;   BT_15MU_HIGH    FLOAT     = Array[90, 32400]
;   BT_15MU_HIGH_PT FLOAT     = Array[90, 32400]
;   BT_15MU_HIGH_VAR FLOAT     = Array[90, 32400]
;   BT_15MU_LOW     FLOAT     = Array[90, 32400]
;   BT_15MU_LOW_PT  FLOAT     = Array[90, 32400]
;   BT_15MU_LOW_VAR FLOAT     = Array[90, 32400]
;   BT_4MU          FLOAT     = Array[90, 32400]
;   BT_4MU_PT       FLOAT     = Array[90, 32400]
;   BT_4MU_VAR      FLOAT     = Array[90, 32400]
;   BT_8MU          FLOAT     = Array[90, 32400]
;   DATA            FLOAT     = Array[90, 32400]
;   LAT             DOUBLE    = Array[90, 32400]
;   LON             DOUBLE    = Array[90, 32400]
;   TIME            DOUBLE    = Array[90, 32400]
;
;-------------------------------------END: READ IN AIRS DATA-------------------------------------------
;
; strip out CIPS scene data
;
    scenestr = string(iscene[iorbit],format = '(I2.2)')
    alb_pert_cips = RAYLEIGH_ALBEDO_ANOMALY[*,*,iscene[iorbit]]
    sza_cips = ZENITH_ANGLE[*,*,iscene[iorbit]]
    bad=where(sza_cips gt 90.)
    alb_pert_cips2=alb_pert_cips
    if bad(0) ne -1L then alb_pert_cips2(bad)=0./0.			; set to nan all data in the dark
    lon_cips = longitude[*,*,iscene[iorbit]]
    lat_cips = latitude[*,*,iscene[iorbit]]
;
;UNIT CIRCLE TRIGONOMETRY TO GET AVERAGE LONGITUDE
;
    coslon = cos(!dtor*(lon_cips))
    sinlon = sin(!dtor*(lon_cips))
;
;x and y of unit cirle:
;
    x= mean(coslon,/nan)
    y= mean(sinlon,/nan)
;
;x value indicates absolute value of longitude between 0 and 180, sign of y indicates what hemisphere
;
    cips_lon_mean = acos(x)/!dtor * (y/abs(y)) ; determine x component on unit circle and y direction
    cips_lat_mean = mean(lat_cips,/nan)
;
;Reform AIRS data
;
;    AIRS_frac_day_all = (time-mean(time) + 43200.)/86400.		; Jeff's way to convert seconds to fractional day
     dum=time/86400.							; Lynn's way: convert seconds since 2000010100 to days since
     AIRS_frac_day_all=dum-min(dum)					; then subtract off day value of yesterday to range from 0-1
;
; Flag measurements within 6 hours of CIPS observations and near the CIPS scene
;
;   data = where(abs(airs_frac_day_all - ut_airs) le .25 and abs(lat-cips_lat_mean) lt 40. and $		; winner
    data = where(abs(airs_frac_day_all - ut_airs) le .0415 and abs(lat-cips_lat_mean) lt 40. and $		; this value doesn't obscure AIRS orbits
                (abs(lon - cips_lon_mean) lt 30. or $
                 abs(lon+360. - cips_lon_mean) lt 50. or $
                 abs(lon-360. - cips_lon_mean) lt 50.),nx)
    lats = reform(lat[data])
    lons = reform(lon[data])
    times = reform(AIRS_frac_day_all[data])
    x=where(finite(times) eq 0. or times lt 0.,nx)
    if nx gt 0L then times[x] = !values.f_nan
    AIRS_frac_day = mean(times,/nan)					; Julday to fraction of day
;
; Determine range of conincidence times from AIRS
;
;   AIRS_coin = where(abs(lats-cips_lat_mean) lt 10. and abs(lons-cips_lon_mean) lt 5.,nx)
    AIRS_coin = where(abs(lats-cips_lat_mean) lt 40. and abs(lons-cips_lon_mean) lt 30.,nx)	; same numbers as above
    if nx eq 0L then stop
    AIRS_coin_time_min =24.*min(times[airs_coin])
    AIRS_coin_time_max =24.*max(times[airs_coin])
    AIRS_coin_time_min_minutes = 60.*(AIRS_coin_time_min - floor(AIRS_coin_time_min))
    AIRS_coin_time_max_minutes = 60.*(AIRS_coin_time_max - floor(AIRS_coin_time_max))
;
; Set file and plot titles				
;
    if cipsplot eq 1 and airsplot eq 1 then begin
       AIRS_plot_data = reform(BT_4MU_pt[data])

       plot_title = sdate+' (DOY '+ string(CIPS_doy,format='(i4.2)')+') CIPS Orbit '+orbitnum+', Scene '+scenestr+'!CCIPS time: '+scenetimestr+$
                    'Z, AIRS coin time: '+string(airs_coin_time_min,format='(i2.2)')+':'+string(AIRS_coin_time_min_minutes,format = '(i2.2)')+'-'+$
                     string(airs_coin_time_max,format='(i2.2)')+':'+string(AIRS_coin_time_max_minutes,format = '(i2.2)')+'Z'

       filename = 'AIRS_T_pt_+CIPS_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
                 +'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
                 +strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'_4mu'
    endif

    if setplot eq 'ps' then begin
       lc=0
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename=filename+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif

;-------------PLOT CODE--------------
	erase
	!type=2^2+2^3
	plot, [0,0],[0,0],xstyle = 4, ystyle =  4,$
	position = [.01,.01,.99,.99], charsize = 1.125,thick = 4 ; style= 4 supresses axis
	xyouts, .5,.95,plot_title,align=.5,charsize = 1.6,color=0,charthick=2
	loadct,0

limit = [cips_lat_mean-15,cips_lon_mean-15,cips_lat_mean+20,cips_lon_mean+30]; Set plot limits based on the mean CIPS lat/lon		; winner
;limit = [cips_lat_mean-20,cips_lon_mean-40,cips_lat_mean+40,cips_lon_mean+40]; Set plot limits based on the mean CIPS lat/lon		; expand plot
;limit = [cips_lat_mean-10,cips_lon_mean-15,cips_lat_mean+10,cips_lon_mean+15]; Set plot limits based on the mean CIPS lat/lon		; contract plot
;limit = [-90,0,0,360]															; plot whole globe

	Map_Set, cips_lat_mean, cips_lon_mean, /Satellite,  /NoErase, position = [.1,.1,.9,.9],$
		/ISOTROPIC, color=0, limit = limit, /horizon,/CONTINENTS
        MAP_CONTINENTS, fill_continents=1,color = 150

;Set AIRS levels (trial and error)
        level1 = (findgen(11)*.2-1.)/3.
        nlvls  = n_elements(level1)
        col1 = [250,210,190,170,125,125,80,60,40,20,0]
;
;plot individual AIRS measurements within the lat/lon limits
;
        loadct,68
        IF airsplot eq 1 then begin
           for i = 0, n_elements(AIRS_plot_data) - 1L do begin
		if lats[i] gt limit[0] and lats[i] lt limit[2] and ((lons[i] gt limit[1] and lons[i] lt (limit[3] +10)) $
		or (lons[i]+360. gt limit[1] and lons[i]+360. lt (limit[3]+10)) $
		or (lons[i]-360. gt limit[1] and lons[i]-360. lt (limit[3]+10))) then begin
			x = where(AIRS_plot_data[i]-level1 gt 0L,nx)
			if nx ge 1 then color = col1[x[-1]]
			oplot,[lons[i],lons[i]],[lats[i],lats[i]], psym=8,color = color,symsize = .5
		endif
           endfor
        endif
;
;Set CIPS levels (trial and error)
;
	loadct,70
	level2=[-5,-4,-3,-2,-1,0,1,2,3,4,5]
	nlvls2  = n_elements(level2)
        col2 = [250,235,220,190,170,125,110,100,80,60,30,15]
	level2=level2/5.
;
;plot individual CIPS measurements within the lat/lon limits
;
	if cipsplot eq 1 then begin
           index=where(finite(alb_pert_cips2),npts)
           xc=reform(lon_cips(index))
           yc=reform(lat_cips(index))
           ac=reform(alb_pert_cips2(index))
           index=where(ac le level2(0))
           if index(0) ne -1L then oplot,xc(index),yc(index),color=col2(0),psym=8,symsize=0.25
           for nn=0L,n_elements(level2)-2L do begin
               index=where(ac gt level2(nn) and ac le level2(nn+1))
               if index(0) ne -1L then oplot,xc(index),yc(index),color=col2(nn+1),psym=8,symsize=0.25
           endfor
           index=where(ac gt level2(-1))
           if index(0) ne -1L then oplot,xc(index),yc(index),color=col2(-1),psym=8,symsize=0.25
        endif
;
;Plot outline of CIPS scene
;
        x = where(finite(alb_pert_cips))
        datalocations = lat_cips*0.
        datalocations[x] =10.
        loadct,0
        contour, datalocations,lon_cips,lat_cips,level=[.1],color = 250, /overplot,/noerase,/close,c_color = 0, c_thick = 4
;
; plot USA states and grid on top of data
;
        Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],/ISOTROPIC, color=0, $
				/CONTINENTS,/horizon,  limit = limit,/USA	
        MAP_GRID, /LABEL, LATLAB=floor(cips_lon_mean/15.)*15.-10., LONLAB=floor(cips_lat_mean/10.)*10-10, LATDEL=5, LONDEL=10, charsize=2.5,color=0,charthick=2
;
; -----------------plot the CIPS color bar-----------------------
;
	if CIPSplot eq 1 then begin
           loadct,0
           level = level2
           !type=2^2+2^3+2^6			; no y title or ticsks
           imin=min(level)
           imax=max(level)
           slab=' '+strarr(n_elements(level))
           !p.title = ' '
           if airsplot eq 1 then position = [.52,0.08,.9,.14]
           plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level2)-1L,charsize=1.25,$
                position = position,xstyle=1,xtickname=slab,xtitle= 'CIPS Albedo Anomaly (%)',color=0,charthick=2
           loadct, 70
           ybox=[0,10,10,0,0]
           x2=0
           for j=1,n_elements(col2)-1 do begin
               dx= 1./(n_elements(level))
               xbox=[x2,x2,x2+dx,x2+dx,x2]
               polyfill,xbox,ybox,color=col2[j-1]
               x2=x2+dx
           endfor
           loadct,0
           slab=strcompress(string(format='(f8.3)',level),/remove_all)
           slabcolor = fltarr(n_elements(level))*0.
           slabcolor[0:2] = 255
           x1=dx/2
           for i=0L,n_elements(slab)-1L do begin
               slab0=slab[i]
               flab0=float(slab[i])
               slab0=strcompress(string(format='(f7.2)',flab0),/remove_all)
               xyouts,x1-dx/2.,5,slab0,charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5
               x1=x1+dx
           endfor
       endif
;
; -----------------plot the AIRS color bar-----------------------
;
       if AIRSplot eq 1 then begin
          level = level1
          !type=2^2+2^3+2^6			; no y title or ticsks
          imin=min(level)
          imax=max(level)
          slab=' '+strarr(n_elements(level))
          !p.title = ' '
          if cipsplot eq 1 then position = [.1,0.08,.48,.14]
          plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level1)-1L,charsize=1.25,$
               position = position,xstyle=1,xtickname=slab,xtitle= 'AIRS BT Perturbation (K)',color=0,charthick=2
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
;
; stop if plotted to the screen, convert ps to png if plotted to a file
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim '+filename+'.ps -rotate -90 '+filename+'.png'
    endif
endfor		; end of loop over CIPS orbits
end
