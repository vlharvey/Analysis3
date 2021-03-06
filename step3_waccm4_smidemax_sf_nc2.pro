;
; read WACCM4 isentropic data and compute Streamfunction and save
; Input data:  IDL>restore,'/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_YYYMMDD.nc
; dimensions:
;        number_of_latitudes = 96 ;
;        number_of_longitudes = 144 ;
;        number_of_levels = 22 ;
;variables:
;        float longitude(number_of_longitudes) ;
;                longitude:longname = "longitude" ;
;                longitude:units = "deg E" ;
;        float latitude(number_of_latitudes) ;
;                latitude:longname = "latitude" ;
;                latitude:units = "deg" ;
;        float theta(number_of_levels) ;
;                theta:longname = "potential temperature" ;
;                theta:units = "K" ;
;        float IPV(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                IPV:longname = "Isentropic Potential Vorticity" ;
;                IPV:units = "K m^2 /s /kg" ;
;        float P(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                P:longname = "Pressure" ;
;                P:units = "hPa" ;
;        float U(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                U:longname = "Zonal Wind" ;
;                U:units = "m/s" ;
;        float V(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                V:longname = "Meridional Wind" ;
;                V:units = "m/s" ;
;        float QDF(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                QDF:longname = "Strain/Rotation Parameter" ;
;                QDF:units = "s-1" ;
;        float CO(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                CO:longname = "Carbon Monoxide" ;
;                CO:units = "mol/mol" ;
;        float GPH(number_of_levels, number_of_longitudes, number_of_latitudes) ;
;                GPH:longname = "Geopotential Height" ;
;                GPH:units = "m" ;
;loadct,39
;device,decompose=0
;mcolor=byte(!p.color)
;nlvls=30L
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
dirw='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/'
ifiles=file_search(dirw+'3d_CO2x1SmidEmax_yBWCN_???????.nc',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'/',/extract)
    result2=strsplit(result(-1),'_',/extract)
    result3=strsplit(result2(-1),'.',/extract)
    sdate=result3(0)
    print,sdate
;
; look for nc2 file and skip if it already exists
;
    ofile=dirw+'3d_CO2x1SmidEmax_yBWCN_'+sdate+'.nc2'
    result=file_search(ofile)
    if result(0) ne '' then goto,jumpstep
;
; read daily file
;
    ncfile0=ifiles(n)
    print,ncfile0
    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'number_of_latitudes' then nr=dim
        if name eq 'number_of_longitudes' then nc=dim
        if name eq 'number_of_levels' then nl=dim
;       print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'latitude' then alat=data
        if result.name eq 'longitude' then alon=data
        if result.name eq 'theta' then theta=data
        if result.name eq 'IPV' then pvgrd=data
        if result.name eq 'P' then pgrd=data
        if result.name eq 'U' then ugrd=data
        if result.name eq 'V' then vgrd=data
        if result.name eq 'QDF' then qdfgrd=data
        if result.name eq 'CO' then cogrd=data
        if result.name eq 'GPH' then zgrd=data
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; prepare variables for ncl script
;
    u=transpose(ugrd,[1,0,2])
    v=transpose(vgrd,[1,0,2])
;
; create temporary winds.nc file
;
    spawn,'rm -f winds.nc'
    ofile='winds.nc'
;   print,'writing ',ofile
    nocid = ncdf_create(ofile,/CLOBBER)
    londimid=ncdf_dimdef(nocid, 'lon', nc)
    latdimid=ncdf_dimdef(nocid, 'lat', nr)
    levdimid=ncdf_dimdef(nocid, 'lev', nl)
    vid = ncdf_vardef(nocid, 'lon',  londimid)
    vid = ncdf_vardef(nocid, 'lat',  latdimid)
    vid = ncdf_vardef(nocid, 'lev',  levdimid)
    vid  = ncdf_vardef(nocid, 'u' , [londimid,latdimid,levdimid])
    vid  = ncdf_vardef(nocid, 'v' , [londimid,latdimid,levdimid])
    ncdf_control,nocid,/ENDEF
    ncdf_varput, nocid, 'lon' , alon
    ncdf_varput, nocid, 'lat' , alat 
    ncdf_varput, nocid, 'lev' , theta
    ncdf_varput, nocid, 'u'   , u
    ncdf_varput, nocid, 'v'   , v
    ncdf_close,nocid
;
; compute SF in ncl script "step3_waccm4_sf_nc2.ncl"
;
    spawn,'rm -f vorticity.nc'
    spawn,'ncl step3_waccm4_sf_nc2.ncl'
;
; read SF from vorticity.nc created by ncl script
;
    ncid=ncdf_open('vorticity.nc')
    result0=ncdf_inquire(ncid)
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'sf' then sfgrd=data
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; check
;
;for k=nl-1L,0L,-1L do begin
;rlev=theta(k)	;2000.
;zmean=strcompress(long(mean(zgrd(*,*,k))/1000.),/r)
;print,theta
;;read,'Enter theta surface ',rlev
;index=where(theta eq rlev)
;ilev=index(0)
;slev=string(rlev)
;pp=transpose(reform(qdfgrd(*,*,ilev)))
;pp2=reform(sfgrd(*,*,ilev))
;level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
;level2=min(pp2)+((max(pp2)-min(pp2))/float(nlvls))*findgen(nlvls)
;!type=2^2+2^3
;erase
;contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
;        yrange=[-90.,90.],title=sdate+'  '+slev+' K ~ ('+zmean+'km)'
;contour,pp2,alon,alat,levels=level2,/follow,c_color=0,/noeras,/overplot,thick=3
;;contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
;;contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
;stop
;endfor
;
    sfgrd2=transpose(sfgrd,[1,0,2])
    sfgrd=sfgrd2
;
; write daily theta file
;
    ofile=dirw+'3d_CO2x1SmidEmax_yBWCN_'+sdate+'.nc2'
    print,'writing ',ofile
    nocid = ncdf_create(ofile,/CLOBBER)
    latdimid=ncdf_dimdef(nocid, 'number_of_latitudes' , nr)
    londimid=ncdf_dimdef(nocid, 'number_of_longitudes', nc)
    levdimid=ncdf_dimdef(nocid, 'number_of_levels'    , nl)
    lonsid = ncdf_vardef(nocid, 'longitude',  londimid)
    latsid = ncdf_vardef(nocid, 'latitude' ,  latdimid)
    levsid = ncdf_vardef(nocid, 'theta'    ,  levdimid)
    vid  = ncdf_vardef(nocid, 'IPV' , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'P'   , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'U'   , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'V'   , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'QDF' , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'CO'  , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'GPH' , [latdimid,londimid,levdimid])
    vid  = ncdf_vardef(nocid, 'SF'  , [latdimid,londimid,levdimid])
    ncdf_attput, nocid, 'longitude', 'longname', 'longitude' & ncdf_attput, nocid, 'longitude', 'units', 'deg E'
    ncdf_attput, nocid, 'latitude', 'longname', 'latitude' & ncdf_attput, nocid, 'latitude', 'units', 'deg'
    ncdf_attput, nocid, 'theta', 'longname', 'potential temperature' & ncdf_attput, nocid, 'theta', 'units', 'K'
    ncdf_attput, nocid, 'IPV', 'longname', 'Isentropic Potential Vorticity' & ncdf_attput, nocid, 'IPV', 'units', 'K m^2 /s /kg'
    ncdf_attput, nocid, 'P', 'longname', 'Pressure' & ncdf_attput, nocid, 'P', 'units', 'hPa'
    ncdf_attput, nocid, 'U', 'longname', 'Zonal Wind' & ncdf_attput, nocid, 'U', 'units', 'm/s'
    ncdf_attput, nocid, 'V', 'longname', 'Meridional Wind' & ncdf_attput, nocid, 'V', 'units', 'm/s'
    ncdf_attput, nocid, 'QDF', 'longname', 'Strain/Rotation Parameter' & ncdf_attput, nocid, 'QDF', 'units', 's-1'
    ncdf_attput, nocid, 'CO', 'longname', 'Carbon Monoxide' & ncdf_attput, nocid, 'CO', 'units', 'mol/mol'
    ncdf_attput, nocid, 'GPH', 'longname', 'Geopotential Height' & ncdf_attput, nocid, 'GPH', 'units', 'm'
    ncdf_attput, nocid, 'SF', 'longname', 'Streamfunction' & ncdf_attput, nocid, 'SF', 'units', 'm2/s'
    ncdf_control,nocid,/ENDEF
    ncdf_varput, nocid, 'longitude', alon  , COUNT=[nc]
    ncdf_varput, nocid, 'latitude' , alat  , COUNT=[nr]
    ncdf_varput, nocid, 'theta'    , theta , COUNT=[nl]
    ncdf_varput, nocid, 'IPV' , pvgrd      , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'P'   , pgrd       , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'U'   , ugrd       , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'V'   , vgrd       , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'QDF' , qdfgrd     , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'CO'  , cogrd      , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'GPH' , zgrd       , COUNT=[nr,nc,nl]
    ncdf_varput, nocid, 'SF'  , sfgrd      , COUNT=[nr,nc,nl]
    ncdf_close,nocid
;
; remove nc file
;
;spawn,'rm -f '+ncfile0
jumpstep:
ENDFOR		; LOOP OVER TIMESTEPS
end
