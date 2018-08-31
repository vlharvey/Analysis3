pro rd_sdwaccm4_nc3,file1,nc,nr,nth,alon,alat,thlev,$
    ipv,prs,gph,u,v,q,qdf,marksf,sf,h2o,n2o,o3,iflg
iflg=0
dum1=findfile(file1)
if dum1(0) ne '' then begin
   ncid=ncdf_open(file1)
   goto,jump
endif
if dum1(0) eq '' then begin
   iflg=1
   return
endif
stop
jump:
;
; read daily file
;
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'number_of_latitudes' then nr=dim
        if name eq 'number_of_longitudes' then nc=dim
        if name eq 'number_of_levels' then nth=dim
;       print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'latitude' then alat=data             ; latitude  (deg)
        if result.name eq 'longitude' then alon=data    ; longitude (deg E)
        if result.name eq 'theta' then thlev=data           ; potential temperature (K)
        if result.name eq 'P' then prs=data              ; pressure  (hPa)
        if result.name eq 'IPV' then ipv=data           ; Ertel potential vorticity (K m^2 /s /kg)
        if result.name eq 'U' then u=data              ; zonal wind (m/s)
        if result.name eq 'V' then v=data              ; meridional wind (m/s)
        if result.name eq 'GPH' then gph=data            ; geopotential height (m)
        if result.name eq 'QDF' then qdf=data          ; Strain/Rotation Parameter (s-1)
        if result.name eq 'Q' then q=data              ; net diabatic heating rate (K/day)
        if result.name eq 'SF' then sf=data              ; stream function (m^2/s)
        if result.name eq 'MARK' then marksf=data              ; stream function (m^2/s)
        if result.name eq 'H2O' then h2o=data              ; water vapor
        if result.name eq 'N2O' then n2o=data              ; nitrous oxide
        if result.name eq 'O3' then o3=data              ; ozone
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
end
