;
; read WACCM4 netcdf data from Ethan Peck. h3 file and save 1 day of 3d QSUM
; /atmos/pecked/forCora
;
dir='/atmos/pecked/forCora/'
;
; SmidEmax run
;
odir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/QSUM_CO2x1SmidEmax_yBWCN.'
;ncfile0=dir+'CO2x1SmidEmax_yBWCN.cam.h1.0297-01-01-00000.nc'
ncfile0=dir+'CO2x1SmidEmax_yBWCN.cam.h1.0297-06-30-00000.nc'
print,ncfile0
ncid=ncdf_open(ncfile0)
result0=ncdf_inquire(ncid)
for idim=0,result0.ndims-1 do begin
    ncdf_diminq,ncid,idim,name,dim
    if name eq 'lon' then nc=dim
    if name eq 'lat' then nr=dim
    if name eq 'lev' then nl=dim
    if name eq 'time' then nt=dim
    print,'read ',name,' dimension ',dim
endfor
for ivar=0,result0.nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)
    if result.name eq 'P0' or result.name eq 'hyai' or result.name eq 'hybi' or result.name eq 'hyam' or result.name eq 'hybm' or result.name eq 'PS' or result.name eq 'lat' or result.name eq 'lon' or result.name eq 'lev' or result.name eq 'time' or result.name eq 'date' or result.name eq 'Z3' or result.name eq 'QSUM' then ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    if result.name eq 'P0' then p0=data
    if result.name eq 'hyai' then hyai=data
    if result.name eq 'hybi' then hybi=data
    if result.name eq 'hyam' then hyam=data
    if result.name eq 'hybm' then hybm=data
    if result.name eq 'PS' then ps=float(data)
    if result.name eq 'lat' then alat=float(data)
    if result.name eq 'lon' then alon=float(data)
    if result.name eq 'lev' then lev=float(data)
    if result.name eq 'time' then time=float(data)
    if result.name eq 'date' then date=data
    if result.name eq 'Z3' then g4d=float(data)/1000.		; Geopotential Height (m)
    if result.name eq 'QSUM' then qsum4d=float(data)		; total ion production (s-1)
    print,ivar,result.name,min(data),max(data)
endfor
ncdf_close,ncid
sdate=strcompress(date,/remove_all)
smmdd=strmid(sdate,3,4)	; first 3 digits are the year
;
; Calculate 3d Pressure: p(i,j,k,n) = A(k)*PO + B(k)*PS(i,j,n) in Pascals
;
p4d=fltarr(nc,nr,nl,nt)
Pzero=P0
FOR ilon=0,nc-1 DO $
    FOR ilat=0,nr-1 DO $
        FOR ialt=0,nl-1 DO $
            p4d(ilon,ilat,ialt,*)=(hyam(ialt)*Pzero + hybm(ialt)*PS(ilon,ilat,*)) / 100.
;
; IDL save file for each day
;
for iday=0L,nt-1L do begin
    ofile=odir+smmdd(iday)+'.sav'
    print,ofile
    p=reform(p4d(*,*,*,iday))
    z=reform(g4d(*,*,*,iday))
    qsum=reform(qsum4d(*,*,*,iday))
    save,file=ofile,alon,alat,lev,p,z,qsum
endfor      ; loop over days
;
; SmidEmin run
;
odir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmin_yBWCN/QSUM_CO2x1SmidEmin_yBWCN.'
;ncfile0=dir+'CO2x1SmidEmin_yBWCN.cam.h1.0297-01-01-00000.nc'
ncfile0=dir+'CO2x1SmidEmin_yBWCN.cam.h1.0297-06-30-00000.nc'
print,ncfile0
ncid=ncdf_open(ncfile0)
result0=ncdf_inquire(ncid)
for idim=0,result0.ndims-1 do begin
    ncdf_diminq,ncid,idim,name,dim
    if name eq 'lon' then nc=dim
    if name eq 'lat' then nr=dim
    if name eq 'lev' then nl=dim
    if name eq 'time' then nt=dim
    print,'read ',name,' dimension ',dim
endfor
for ivar=0,result0.nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)
    if result.name eq 'P0' or result.name eq 'hyai' or result.name eq 'hybi' or result.name eq 'hyam' or result.name eq 'hybm' or result.name eq 'PS' or result.name eq 'lat' or result.name eq 'lon' or result.name eq 'lev' or result.name eq 'time' or result.name eq 'date' or result.name eq 'Z3' or result.name eq 'QSUM' then ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    if result.name eq 'P0' then p0=data
    if result.name eq 'hyai' then hyai=data
    if result.name eq 'hybi' then hybi=data
    if result.name eq 'hyam' then hyam=data
    if result.name eq 'hybm' then hybm=data
    if result.name eq 'PS' then ps=float(data)
    if result.name eq 'lat' then alat=float(data)
    if result.name eq 'lon' then alon=float(data)
    if result.name eq 'lev' then lev=float(data)
    if result.name eq 'time' then time=float(data)
    if result.name eq 'date' then date=data
    if result.name eq 'Z3' then g4d=float(data)/1000.          ; Geopotential Height (m)
    if result.name eq 'QSUM' then qsum4d=float(data)           ; total ion production (s-1)
    print,ivar,result.name,min(data),max(data)
endfor
ncdf_close,ncid
sdate=strcompress(date,/remove_all)
smmdd=strmid(sdate,3,4) ; first 3 digits are the year
;
; Calculate 3d Pressure: p(i,j,k,n) = A(k)*PO + B(k)*PS(i,j,n) in Pascals
;
p4d=fltarr(nc,nr,nl,nt)
Pzero=P0
FOR ilon=0,nc-1 DO $
    FOR ilat=0,nr-1 DO $
        FOR ialt=0,nl-1 DO $
            p4d(ilon,ilat,ialt,*)=(hyam(ialt)*Pzero + hybm(ialt)*PS(ilon,ilat,*)) / 100.
;
; IDL save file for each day
;
for iday=0L,nt-1L do begin
    ofile=odir+smmdd(iday)+'.sav'
    print,ofile
    p=reform(p4d(*,*,*,iday))
    z=reform(g4d(*,*,*,iday))
    qsum=reform(qsum4d(*,*,*,iday))
    save,file=ofile,alon,alat,lev,p,z,qsum
endfor      ; loop over days

end
