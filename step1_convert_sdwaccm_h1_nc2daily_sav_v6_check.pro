;
; read SDWACCM h1 netcdf data files from Chuck Bardeen - July 19th 2018
; coarsen in space to old 2x2.5 resoltution
; output daily .sav files
;
; /atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_New/f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.*
;
; CO2, NO, NO2, H2O, N2O, O3, QRS_TOT, QRL_TOT, T, U, V, Z3
;
device,decompose=0
loadct,39
;
; read old WACCM grid
;
ncid=ncdf_open('/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_New/f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h3.2014-10-11-00000.nc')
result0=ncdf_inquire(ncid)
for idim=0,result0.ndims-1 do begin
    ncdf_diminq,ncid,idim,name,dim
    if name eq 'lon' then nc_old=dim
    if name eq 'lat' then nr_old=dim
endfor
for ivar=0,result0.nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)
    if result.name eq 'lat' or result.name eq 'lon' then begin
       ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
       if result.name eq 'lat' then lat_old=data
       if result.name eq 'lon' then lon_old=data
    endif
endfor
ncdf_close,ncid
;
; read new WACCM data
;
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_v6/'
spawn,'ls '+dir+'c_cesm2_fswd_2005_cntrl.cam.h1.2005*-00000.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=0L,nfiles-1L do begin
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'lon' then nc=dim
        if name eq 'lat' then nr=dim
        if name eq 'lev' then nl=dim
        if name eq 'time' then nt=dim
;       print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        if result.name eq 'lat' or result.name eq 'lon' or result.name eq 'lev' or result.name eq 'P0' or result.name eq 'hyai' or $
           result.name eq 'hybi' or result.name eq 'hyam' or result.name eq 'hybm' or result.name eq 'date' or  result.name eq 'T' or $
           result.name eq 'U' or result.name eq 'V' or result.name eq 'Z3' or result.name eq 'PS' or result.name eq 'CO' or $
           result.name eq 'NO' or result.name eq 'NO2' or result.name eq 'O3' then begin

           ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
           if result.name eq 'P0' then p0=data
           if result.name eq 'hyai' then hyai=data
           if result.name eq 'hybi' then hybi=data
           if result.name eq 'hyam' then hyam=data
           if result.name eq 'hybm' then hybm=data
           if result.name eq 'date' then date=data
           if result.name eq 'PS' then ps=data
           if result.name eq 'lat' then lat=data
           if result.name eq 'lon' then lon=data
           if result.name eq 'lev' then lev=data
           if result.name eq 'CO' then cogrd=data
           if result.name eq 'NO' then nogrd=data
           if result.name eq 'NO2' then no2grd=data
           if result.name eq 'O3' then o3grd=data
           if result.name eq 'T' then tgrd=data
           if result.name eq 'U' then ugrd=data
           if result.name eq 'V' then vgrd=data
           if result.name eq 'Z3' then zgrd=data
;          print,ivar,result.name,min(data),max(data)
        endif
    endfor
    ncdf_close,ncid
    sdate=strcompress(date,/remove_all)
;
; Calculate 3d Pressure: p(i,j,k,n) = A(k)*PO + B(k)*PS3d(i,j,n) in hPa
;
    pgrd=0.*zgrd
    Pzero=P0
    FOR ilon=0,nc-1 DO $
        FOR ilat=0,nr-1 DO $
            FOR ialt=0,nl-1 DO $
                pgrd(ilon,ilat,ialt)=(hyam(ialt)*Pzero + hybm(ialt)*PS(ilon,ilat,*)) / 100.
;erase
;plev=10.
;zlev=fltarr(nc,nr)
;for j=0L,nr-1L do begin
;for i=0L,nc-1L do begin
;    zprof=reform(zgrd(i,j,*))
;    pprof=reform(pgrd(i,j,*))
;    zlev(i,j)=interpol(zprof,alog(pprof),alog(plev))
;endfor
;endfor
;
;mcolor=255
;map_set,0,0,0,/contin,/grid,/noerase,color=0,charsize=1.5,title='original SD-WACCM at '+strcompress(plev,/r)+' hPa on '+strcompress(date,/r)
;;dum=reform(zgrd(*,*,50))
;dum=zlev
;imin=min(dum)
;imax=max(dum)
;nlvls=26L
;level=imin+((imax-imin)/float(nlvls-1L))*findgen(nlvls)
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;contour,dum,lon,lat,levels=level,/noeras,c_color=col1,/overplot
;stop

;
; interpolate new v6 run to old grid
;
    cogrd_old=fltarr(nc_old,nr_old,nl)
    nogrd_old=fltarr(nc_old,nr_old,nl)
    no2grd_old=fltarr(nc_old,nr_old,nl)
    o3grd_old=fltarr(nc_old,nr_old,nl)
    tgrd_old=fltarr(nc_old,nr_old,nl)
    ugrd_old=fltarr(nc_old,nr_old,nl)
    vgrd_old=fltarr(nc_old,nr_old,nl)
    zgrd_old=fltarr(nc_old,nr_old,nl)
    pgrd_old=fltarr(nc_old,nr_old,nl)
    for k=0L,n_elements(lev)-1L do begin
    for i=0L,nc_old-1L do begin
        cogrd_old(i,*,k)=interpol(reform(cogrd(i*2,*,k)),lat,lat_old)
        nogrd_old(i,*,k)=interpol(reform(nogrd(i*2,*,k)),lat,lat_old)
        no2grd_old(i,*,k)=interpol(reform(no2grd(i*2,*,k)),lat,lat_old)
        o3grd_old(i,*,k)=interpol(reform(o3grd(i*2,*,k)),lat,lat_old)
        tgrd_old(i,*,k)=interpol(reform(tgrd(i*2,*,k)),lat,lat_old)
        ugrd_old(i,*,k)=interpol(reform(ugrd(i*2,*,k)),lat,lat_old)
        vgrd_old(i,*,k)=interpol(reform(vgrd(i*2,*,k)),lat,lat_old)
        zgrd_old(i,*,k)=interpol(reform(zgrd(i*2,*,k)),lat,lat_old)
        pgrd_old(i,*,k)=interpol(reform(pgrd(i*2,*,k)),lat,lat_old)
    endfor
    endfor
;
; rename
;
    lat=lat_old
    lon=lon_old
    cogrd=cogrd_old
    nogrd=nogrd_old
    no2grd=no2grd_old
    o3grd=o3grd_old
    tgrd=tgrd_old
    ugrd=ugrd_old
    vgrd=vgrd_old
    zgrd=zgrd_old
    pgrd=pgrd_old
;
; save daily file
;
;   ofile=dir+'c_cesm2_fswd_2005_cntrl.cam.h1.'+sdate+'.sav'
;   save,file=ofile,LAT,LON,LEV,PGRD,TGRD,UGRD,VGRD,ZGRD,COGRD,NOGRD,NO2GRD,O3GRD
;
; quick check
;
    erase
;   dum=mean(ugrd,dim=1)                                          
;   contour,dum,lat,lev,/ylog,yrange=[1000,min(lev)],levels=10*findgen(20),title=sdate,/noeras
;   contour,dum,lat,lev,/ylog,levels=-200+10*findgen(20),/overplot,c_linestyle=5,/noeras
;   dum=mean(tgrd,dim=1)                                                                                  
;   contour,dum,lat,lev,/ylog,levels=100+10*findgen(5),/overplot,color=100,thick=5,/noeras
;   contour,dum,lat,lev,/ylog,levels=160+20*findgen(10),/overplot,c_color=(indgen(10)/10.)*255.,thick=2,/noeras
;plev=10.
;zlev=fltarr(nc_old,nr_old)
;for j=0L,nr_old-1L do begin
;for i=0L,nc_old-1L do begin
;    zprof=reform(zgrd(i,j,*))
;    pprof=reform(pgrd(i,j,*))
;    zlev(i,j)=interpol(zprof,alog(pprof),alog(plev))
;endfor
;endfor
;
;mcolor=255
;map_set,0,0,0,/contin,/grid,/noerase,color=0,charsize=1.5,title='original SD-WACCM'
;;dum=reform(zgrd(*,*,50))
;dum=zlev
;imin=min(dum)
;imax=max(dum)
;nlvls=26L
;level=imin+((imax-imin)/float(nlvls-1L))*findgen(nlvls)
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;contour,dum,lon,lat,levels=level,/noeras,c_color=col1,/overplot
;stop
endfor			; loop over daily files
end
