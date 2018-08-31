;
; save 3d daily data for the first 10 days of Year 151 of the SmidEmin case
;
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z33d_CO2x1SmidEmin_yBWCN_0151_vE.sav missing these days
;
ncfile0='/atmos/pecked/forLynn/CO2x1SmidEmin_yBWCN.cam.h1.0150-12-22-00000.nc'
print,ncfile0
ncid=ncdf_open(ncfile0)
result0=ncdf_inquire(ncid)
for idim=0,result0.ndims-1 do begin
    ncdf_diminq,ncid,idim,name,dim
    if name eq 'lon' then nc=dim
    if name eq 'lat' then nr=dim
    if name eq 'lev' then nl=dim
    if name eq 'ilev' then nl1=dim
    if name eq 'time' then nt=dim
    print,'read ',name,' dimension ',dim
endfor
for ivar=0,result0.nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)
    if result.name eq 'lat' or result.name eq 'lon' or result.name eq 'lev' or result.name eq 'date' or $
       result.name eq 'T' or result.name eq 'U' or result.name eq 'V' or result.name eq 'Z3' or result.name eq 'CO' then ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    if result.name eq 'lat' then lat=data
    if result.name eq 'lon' then lon=data
    if result.name eq 'lev' then lev=data
    if result.name eq 'date' then date=data
    if result.name eq 'T' then t4d=data
    if result.name eq 'U' then u4d=data
    if result.name eq 'V' then v4d=data
    if result.name eq 'Z3' then z4d=data
    if result.name eq 'CO' then co4d=data
    print,ivar,result.name,min(data),max(data)
endfor
ncdf_close,ncid

dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmin_yBWCN/'

lon=float(lon)
lat=float(lat)
lev=float(lev)
;
; loop over days and save
;
sdates=string(format='(i7.7)',date)
nday=n_elements(date)
for iday=0L,nday-1L do begin
     
     cogrd=reform(CO4D(*,*,*,iday))
     tgrd=reform(T4D(*,*,*,iday))
     ugrd=reform(U4D(*,*,*,iday))
     vgrd=reform(V4D(*,*,*,iday))
     zgrd=reform(Z4D(*,*,*,iday))

     print,'saved '+dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav'
     save,file=dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav',lon,lat,lev,cogrd,tgrd,ugrd,vgrd,zgrd
endfor	; loop over days
end
