@standard_scripts
@wave_signif
@date2doy
@rd_merra2_nc3

dir='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/'

mdir='/atmos/harvey/MERRA2_data/Datfiles/MERRA2-on-WACCM_theta_'
pdir='/atmos/harvey/MERRA2_data/Datfiles/MERRA2-on-WACCM_press_'
stime=['00','06','12','18']
icount=0L

plottype=0
pre_plot,plottype
device,decompose=0

x1=[.1,.4,.7]
x2=[.35,.65,.95]
y1=[.53,.2]
y2=[.8,.47]
orbitlist=[$
'50416'$       ; vortex edge
]
iscene=[$
'14'$
]

for cipsplot = 1,1 do begin
		
		;-------------------------------------READ IN CIPS DATA-------------------------------------------
		for iorbit = 0, n_elements(orbitlist) - 1L do begin
			if orbitlist[iorbit] eq '49884' then continue
			file=FILE_SEARCH(dir+'cips_ray_gw_north_2016_'+orbitlist[iorbit]+'*.sav',COUNT=nfiles)
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

orbitstarttimestr=strmid(scene.ORBIT_START_TIME_UT,9,8)														; extract HH:MM:SS
orbitstartsecs=float(strmid(orbitstarttimestr,0,2))*60.*60. + float(strmid(orbitstarttimestr,3,2))*60. + float(strmid(orbitstarttimestr,6,2))			; convert orbit start to seconds

timesinceorbitstart=(scene.time - scene.orbit_start_time )/1000000.												; seconds since orbit start
scenesecs=timesinceorbitstart + orbitstartsecs															; add seconds since to orbit start

scenetime=(scenesecs(iscene[iorbit])/86400.)*24.																; scene time in frac hours
scenemins=long((scenetime-long(scenetime))*60.)
scenetimestr=string(format = '(I2.2)',long(scenetime))+':'+string(format = '(I2.2)', scenemins)
print,scenetimestr

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
;
; read MERRA2 pressure data
;
    ifile0=string(FORMAT='(i4.4,i2.2,i2.2)',iyr0,imn0,idy0)+stime0+'.sav'
    restore,pdir+ifile0
alon=LONGITUDE_WACCM
nc=n_elements(alon)
alat=LATITUDE_WACCM
nr=n_elements(alat)
psold=PSGRD     ;            FLOAT     = Array[144, 96]
told=TGRD       ;            FLOAT     = Array[144, 96, 41]
uold=UGRD       ;            FLOAT     = Array[144, 96, 41]
vold=VGRD       ;            FLOAT     = Array[144, 96, 41]
zold=ZGRD       ;            FLOAT     = Array[144, 96, 41]

    ifile1=string(FORMAT='(i4.4,i2.2,i2.2)',iyr1,imn1,idy1)+stime1+'.sav'
    restore,pdir+ifile1
psnew=PSGRD       ;            FLOAT     = Array[144, 96]
tnew=TGRD       ;            FLOAT     = Array[144, 96, 41]
unew=UGRD       ;            FLOAT     = Array[144, 96, 41]
vnew=VGRD       ;            FLOAT     = Array[144, 96, 41]
znew=ZGRD       ;            FLOAT     = Array[144, 96, 41]

print,monday,' ',scenetime
print,pdir+ifile0
print,pdir+ifile1
;
; perform time interpolation
;
    ps2=psold+TSCALE*(psnew-psold)
    u2=uold+TSCALE*(unew-uold)
    v2=vold+TSCALE*(vnew-vold)
;
      if icount eq 0 then begin
         press=1
;        print,pressure
;        read,'Enter pressure level ',press
         index=where(press eq pressure)
         if index(0) eq -1 then stop,'Invalid pressure level '
         ipress=index(0)
      endif
      press=pressure(ipress)
      spress=strcompress(string(press),/remove_all)
      u1=reform(u2(*,*,ipress))
      v1=reform(v2(*,*,ipress))
      u=0.*fltarr(nc+1,nr)
      u(0:nc-1,0:nr-1)=u1(0:nc-1,0:nr-1)
      u(nc,*)=u(0,*)
      v=0.*fltarr(nc+1,nr)
      v(0:nc-1,0:nr-1)=v1(0:nc-1,0:nr-1)
      v(nc,*)=v(0,*)
;
; read MERRA2 theta data
;
    ifile0=string(FORMAT='(i4.4,i2.2,i2.2)',iyr0,imn0,idy0)+stime0+'.nc3'
    rd_merra2_nc3,mdir+ifile0,nc,nr,nth,alon,alat,th,pvold,pold,$
                   uold,vold,qdfold,markold,qvold,gold,sfold,qold,o3old,iflag
    ifile1=string(FORMAT='(i4.4,i2.2,i2.2)',iyr1,imn1,idy1)+stime1+'.nc3'
    rd_merra2_nc3,mdir+ifile1,ncw,nrw,nthw,alon,alat,th,pvnew,pnew,$
                   unew,vnew,qdfnew,marknew,qvnew,gnew,sfnew,qnew,o3new,iflag
index=where(markold lt 0.)
if index(0) ne -1L then markold(index)=-1.
index=where(marknew lt 0.)
if index(0) ne -1L then marknew(index)=-1.
;
; perform time interpolation
;
    u2=uold+TSCALE*(unew-uold)
    v2=vold+TSCALE*(vnew-vold)
    sf2=sfold+TSCALE*(sfnew-sfold)
    mark2=markold+TSCALE*(marknew-markold)
    z2=gold+TSCALE*(gnew-gold)
    z50s=reform(z2(20,*,*))	; 52S
    zprof=mean(z50s,dim=1)

      if icount eq 0 then begin
         theta=2000.
;        print,th
;        read,'Enter theta ',theta
         index=where(theta eq th)
         if index(0) eq -1 then stop,'Invalid theta level '
         thlev=index(0)
         icount=1
      endif
      theta=th(thlev)
      stheta=strcompress(string(fix(theta)),/remove_all)
;     u1=transpose(u2(*,*,-1))
;     v1=transpose(v2(*,*,-1))  ; wind speed at 350 K
      sf1=transpose(sf2(*,*,thlev))
      mark1=transpose(mark2(*,*,thlev))
      sf=0.*fltarr(nc+1,nr)
      sf(0:nc-1,0:nr-1)=sf1(0:nc-1,0:nr-1)
      sf(nc,*)=sf(0,*)
      mark=0.*fltarr(nc+1,nr)
      mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
      mark(nc,*)=mark(0,*)
      alon2=fltarr(nc+1)
      alon2(0:nc-1)=alon
      alon2(nc)=alon2(0)+360.
      lon=0.*sf
      lat=0.*sf
      for i=0,nc   do lat(i,*)=alat
      for j=0,nr-1 do lon(*,j)=alon2

;set_plot,'x'
;window
;!p.font=1
;!type=2^2+2^3
;loadct,39
;uyz=reform(u2(*,116,*))	; 70W
;markyz=reform(mark2(*,116,*))
;contour,uyz,alat,zprof,xrange=[-90,-30],levels=10+10*findgen(15),/foll,xtitle='Latitude',ytitle='Altitude',c_color=(findgen(15)/14.)*255,thick=3,title='MERRA2 Zonal Wind at 70W',charsize=2,charthick=2
;contour,markyz,alat,zprof,levels=[0.1],thick=15,/overplot 
;stop

			
			; strip out CIPS scene data
			scenestr = string(iscene[iorbit],format = '(I2.2)')
			alb_pert_cips = scene.alb[*,*,iscene[iorbit]]
			sza_cips = scene.sza[*,*,iscene[iorbit]]*180./!pi
bad=where(sza_cips gt 90.)
alb_pert_cips2=alb_pert_cips
if bad(0) ne -1L then alb_pert_cips2(bad)=0./0.
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
		
			; Set file and plot titles				
				plot_title = 'CIPS Albedo Pert!C'$
				+'DOY:'+ string(CIPS_doy,format='(i4.2)')+', Orbit:'+orbitnum+', Scene:'+scenestr+'!CCIPS time: '+scenetimestr+'Z'
	
				filename = 'CIPS_pt_DOY'+strtrim(string(CIPS_doy,format='(i3.3)'),2)+strmid(string(cips_frac_day,format='(f5.3)'),1,3)$
				+'_orbit'+orbitnum+'_scene'+scenestr+'_lat'+strtrim(string(cips_lat_mean,format = '(i4.2)'),2)+'_lon'$
				+strtrim(string(cips_lon_mean,format = '(i5.3)'),2)+'.png'
	
			;-------------PLOT CODE--------------
			erase
			!type=2^2+2^3
			plot, [0,0],[0,0],xstyle = 4, ystyle =  4,$
			position = [.01,.01,.99,.99], charsize = 1.125,thick = 4 ; style= 4 supresses axis
			xyouts, .5,.97,plot_title,align=.5,charsize = 1.6
			loadct,0
	
;limit = [cips_lat_mean-20,cips_lon_mean-30,cips_lat_mean+20,cips_lon_mean+30]; Set plot limits based on the mean CIPS lat/lon
limit = [cips_lat_mean-10,cips_lon_mean-15,cips_lat_mean+10,cips_lon_mean+5]; Set plot limits based on the mean CIPS lat/lon
;limit=[-90,0,0,360]
	
			Map_Set, cips_lat_mean, cips_lon_mean, /Satellite,  /NoErase, position = [.1,.1,.9,.9],$
			/ISOTROPIC, color=0, limit = limit, /horizon,$
			/CONTINENTS
                        MAP_CONTINENTS, fill_continents=1,color = 150

			loadct,70
			;Set CIPS levels
			level2=[-5,-4,-3,-2,-1,0,1,2,3,4,5]
			nlvls2  = n_elements(level2)
;		col2 = [250,220,190,170,125,125,100,80,60,30,0]
                col2 = [250,235,220,190,170,125,125,100,80,60,30,15,10,0]

			if orbitlist[iorbit] eq '50416' then level2=level2*2
	
			if cipsplot eq 1 then begin
;			contour, smooth(alb_pert_cips2,1,/nan), lon_cips,lat_cips, /overplot, $
;				levels = level2,c_color = col2,/noerase, /close,/cell_fill,  color = 0,c_thick = 1
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

			endif
	
			;Plot outline of CIPS scene
			x = where(finite(alb_pert_cips))
			datalocations = lat_cips*0.
			datalocations[x] =10.
			loadct,0
			contour, datalocations,lon_cips,lat_cips,level=[.1],color = 250, /overplot,/noerase,/close,c_color = 0, c_thick = 4
	
			if orbitlist[iorbit] eq '49883' then begin
				Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
				/ISOTROPIC, color=0, $
				/CONTINENTS,/horizon,  limit = limit,/USA	
			endif
			Map_Set, cips_lat_mean, cips_lon_mean, /Satellite, /NoErase, position = [.1,.1,.9,.9],$
			/ISOTROPIC, color=0, $
			/CONTINENTS,/horizon,  limit = limit
;        		MAP_GRID, /LABEL, LATLAB=floor(cips_lon_mean/15.)*15.-20., LONLAB=floor(cips_lat_mean/10.)*10+17, LATDEL=5, LONDEL=10
 		        MAP_GRID, /LABEL, LATLAB=floor(cips_lon_mean/15.)*15.-2., LONLAB=floor(cips_lat_mean/10.)*10+12, LATDEL=5, LONDEL=10, charsize=2.5
;
; superimpose MERRA2 isotachs and vortex edge
;
      mcolor=255
;     contour,sf,alon2,alat,/overplot,nlevels=20,thick=5,color=0
;     contour,sp,alon2,alat,/overplot,levels=[50.],thick=5,color=.65*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[60.],thick=5,color=.55*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[70.],thick=5,color=.45*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[80.],thick=5,color=.35*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[90.],thick=5,color=.25*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[100.],thick=5,color=.15*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[110.],thick=5,color=.15*mcolor,c_labels=[1]
;     contour,sp,alon2,alat,/overplot,levels=[120.],thick=5,color=.15*mcolor,c_labels=[1]
loadct,0

      drawvectors,nc+1,nr,alon2,alat,u,v,10,1

mark=smooth(mark,3,/edge_truncate)
for k=thlev,thlev do begin
    print,zprof(k) 
      mark1=transpose(mark2(*,*,k))
      mark=0.*fltarr(nc+1,nr)
      mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
      mark(nc,*)=mark(0,*)
      contour,mark,alon2,alat,/overplot,levels=[0.5],thick=10,color=0

vortex_lat=fltarr(nc)
for i=0L,nc-1L do begin
    markslice=reform(mark(i,*))
    index=where(alat lt 0. and markslice gt 0.)
    vortex_lat(i)=max(alat(index))
endfor
endfor
;oplot,alon,vortex_lat,psym=1,color=0,symsize=3

lonarr = findgen(33)*.25-75.
latarr = findgen(33)*((53-47.)/32.)-53.
if cipsplot eq 1 then oplot, lonarr, latarr, color = 0, thick = 14
loadct,70


			if CIPSplot eq 1 then begin
	
				; -----------------plot the CIPS color bar-----------------------
				
loadct,0
				level = level2
				!type=2^2+2^3+2^6			; no y title or ticsks
				imin=min(level)
				imax=max(level)
				slab=' '+strarr(n_elements(level))
				
				!p.title = ' '
				position = [.3,0.04, .7,.1]
				plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=n_elements(level2)-1L,$
					position = position,xstyle=1,xtickname=slab,xtitle= 'Albedo Anomaly (%)',color=0
				loadct, 70
				
				ybox=[0,10,10,0,0]
				
				x2=0
				for j=1,n_elements(col2)-3 do begin
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
stop
;		if plottype eq 1 then begin
;			device, /close
;			spawn, 'gs -dBATCH -sDEVICE=png16m -r300 -dNOPAUSE -sOutputFile=filename+' idl.ps'
;		endif
		endfor
endfor
end
