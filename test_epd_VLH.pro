@open_eps.pro
@close_eps.pro
@colorbar.pro
@epd_calculator_4Cora.pro

spawn,'date'
skip_read = 0
;read,"Skip reading (n=0/y=1)",skip_read

if skip_read eq 1 then goto, jump_read

;constants
p0=1e3
hscale=7000.
rgas=287.
cp=1004.
gravit=9.81
aearth=6.37e6

omega_earth = 2. * !pi / 86400.
beta_earth = 2. * omega_earth / aearth

; list of fields
field = [ 'U' , 'V', 'T' , 'OMEGA'  ]
nfields = n_elements(field)
;
; set dirw to directory in which h3 data files reside
;
;dirw = '/Volumes/PRIVATE/DATA/'
dirw = '/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/'

;cyear = '2009'
;
; set casename to be filename prefix
;
;casename = 'f_sd_waccmx_nogaps_2009'
casename='mee00fpl_FW3.cam2.h3.'

;nsamples_per_day = 1
;read,"Number of samples per day?",nsamples_per_day

;dayb = 1
;read,"Beginning day =",dayb

;daye = 90
;read,"Ending day =",daye

;ilat_step = 2
;read,"Latitude step (1,2..)",ilat_step

;ilon_step = 4
;read,"Longitude step (1,2..)",ilon_step

;itime_step = 1
;read,"Time step (1,2,3 ...)",itime_step

;date0 = 20090101
;read,"Starting date (YYYYMMDD) = ",date0
;
; filenames
;
;file_name = dirw + casename + '/atm/hist/' + casename + '.rd.h1.' + cyear + '.nc'
file_name = file_search(dirw+casename+'*.nc')
;
; loop over files
;
nfiles=n_elements(file_name)
for nn=0L,nfiles-1L do begin

print,file_name(nn)
ncid=ncdf_open(file_name(nn),/nowrite)

levid = ncdf_varid(ncid,'lev')
ncdf_varget,ncid,levid,lev
nlev = n_elements(lev)
nlev_org = nlev

zh = hscale * 1e-3 * alog(1e3/lev)

latid = ncdf_varid(ncid,'lat')
ncdf_varget,ncid,latid,lat
nlat = n_elements(lat)
;lat = lat[0:nlat-1:ilat_step]
;nlat_org = nlat
;nlat = n_elements(lat)

clat = cos( lat*!dtor )
slat = sin( lat*!dtor )

lonid = ncdf_varid(ncid,'lon')
ncdf_varget,ncid,lonid,lon
nlon = n_elements( lon )
;lon = lon[0:nlon-1:ilon_step]
;nlon_org = nlon
;nlon = n_elements(lon)

timid = ncdf_varid(ncid,'time')
ncdf_varget,ncid,timid,time
nt = n_elements(time)
;time = time[0:nt-1:itime_step]
;nt_org = nt
;nt = n_elements(time)

dateid = ncdf_varid(ncid,'date')
ncdf_varget,ncid,dateid,date
;date = date[0:nt_org-1:itime_step]

print, "Field list:",field

f_all_in = make_array(nlon,nlat,nlev,nt,nfields,value=0.)
for n=0,nfields-1 do begin

   print, "Reading "+field[n] + " ..."
   uid=ncdf_varid(ncid,field[n])
   if uid eq -1 then begin
      print," ... does not exist on file ... skipping"
      continue
   endif
   print," ... exists on file ... reading "

   ncdf_varget,ncid,uid,dummy   ;,stride=[ilon_step,ilat_step,1,itime_step]
   f_all_in[*,*,*,*,n] = dummy

endfor                          ; n

print,"Finished reading"

print, "No interpolation - Using hybrid levels"

vin = make_array(nlon,nlat,nlev,nt,nfields,value=!values.f_nan)
for l=0,nt-1 do vin[*,*,*,l,*] = f_all_in[*,*,*,l,*]

index = where( field eq 'U')
if index[0] ge 0 then u = vin[*,*,*,*,index]

index = where( field eq 'V')
if index[0] ge 0 then v = vin[*,*,*,*,index]

index = where( field eq 'T')
if index[0] ge 0 then t = vin[*,*,*,*,index]

index = where( field eq 'Z3')
if index[0] ge 0 then z3 = vin[*,*,*,*,index]

index = where( field eq 'OMEGA')
if index[0] ge 0 then omega = vin[*,*,*,*,index]

w = omega * rgas * t / gravit
for k=0,nlev-1 do w[*,*,k,*] = - w[*,*,k,*] / (1e2 * lev[k] )

; EPD/PV calcs
epd_calcs:

epd = make_array(nlat,nlev,nt,value=0.)
fy = make_array(nlat,nlev,nt,value=0.)
fz = make_array(nlat,nlev,nt,value=0.)
vres = make_array(nlat,nlev,nt,value=0.)
wres = make_array(nlat,nlev,nt,value=0.)

for l=0,nt-1 do begin

   print, "Processing EPD/PV ",100.*float(l+1)/float(nt),"%"

   dy_u = reform(u[*,*,*,l])

   dy_v = reform(v[*,*,*,l])

   dy_t = reform(t[*,*,*,l])

   dy_w = reform(w[*,*,*,l])

   result = epd_calculator (dy_u , dy_v , dy_w , dy_t , lat , lev , qg_approx=0)


   epd[*,*,l] = result[*,*,0]
   fy[*,*,l] = result[*,*,1]
   fz[*,*,l] = result[*,*,2]
   vres[*,*,l] = result[*,*,3]
   wres[*,*,l] = result[*,*,4]

endfor                          ;l

result = 0. & dummy_v = 0. & dummy_w = 0.

ubar = total( u , 1 ,/nan) / float(nlon)
vbar = total( v , 1 ,/nan) / float(nlon)
tbar = total( t , 1 ,/nan) / float(nlon)

jump_read:

a = open_eps()

ycoord = zh

xcoord = lat

yr = [ 20., 100.]

xr = [-80.,80.]

xtitle = 'Latitude'

ytitle = 'Height (km)'



for l=0,nt-1L do begin

; EPD
   ain =  reform( epd[*,*,l] ) * 86400.

   cyear=strcompress(date(l),/remove_all)
   title = cyear + ' EPD at Time =' + string(time[l],format='(f7.3)')

   loadct,0,/silent
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/nodata,position=[0.2,0.2,0.9,0.9]

   loadct,33,/silent
   ncint = 21 & cint = 5.
   clev = findgen(ncint) * cint - float(cint)*float(ncint+1)*0.5

   ain[ where( ain lt min(clev) ) ] = min(clev)
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/cell,/overplot,lev=clev

   colorbar,position=[0.15, 0.0, 0.95, 0.025],range=[min(clev),max(clev)],divisions=6,format='(f7.1)'

; T
   ain =  reform( tbar[*,*,l] )

   title = cyear + ' T at Time =' + string(time[l],format='(f7.3)')

   loadct,0,/silent
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/nodata,position=[0.2,0.2,0.9,0.9]

   loadct,33,/silent
   ncint = 31 & cint = 5.
   clev = findgen(ncint+1) * cint + 150.

   ain[ where( ain lt min(clev) ) ] = min(clev)
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/cell,/overplot,lev=clev

   colorbar,position=[0.15, 0.0, 0.95, 0.025],range=[min(clev),max(clev)],divisions=6,format='(f7.1)'

; U
   ain =  reform( ubar[*,*,l] )

   title = cyear + ' U at Time =' + string(time[l],format='(f7.3)')

   loadct,0,/silent
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/nodata,position=[0.2,0.2,0.9,0.9]

   loadct,33,/silent
   ncint = 20 & cint = 10.
   clev = findgen(ncint+1) * cint - float(cint)*float(ncint)*0.5

   ain[ where( ain lt min(clev) ) ] = min(clev)
   contour,ain,xcoord,ycoord,title=title,xr=xr,yr=yr,/ys,/xs,xtitle=xtitle,ytitle=ytitle,/cell,/overplot,lev=clev

   colorbar,position=[0.15, 0.0, 0.95, 0.025],range=[min(clev),max(clev)],divisions=6,format='(f7.1)'


endfor                          ; l

a = close_eps()
;
; save EP Flux diagnostics associated with this file
;
save,file=file_name(nn)+'_EPFLUX.sav',lat,lev,date,epd,fy,fz,vres,wres

endfor          ; loop over files
spawn,'date'
end

