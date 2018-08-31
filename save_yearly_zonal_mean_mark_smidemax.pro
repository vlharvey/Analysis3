;
; save daily zonal mean mark in yearly files
; SmidEmax
;
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)
;
; build MMDD dates
;
year1files=file_search(dir+'001????.nc3')
ndays=n_elements(year1files)
mmdd=strarr(ndays)
for ii=0L,ndays-1L do begin
    dum=strsplit(year1files(ii),'.',/extract)
    dum2=strsplit(dum(0),'_',/extract)
    mmdd(ii)=strmid(dum2(-1),3,4)
endfor
;
; loop over years
;
for iyear=0L,300L do begin
    syear=string(format='(i3.3)',iyear)
    ofile=dir+syear+'_mark.sav'
    dum=file_search(ofile)
    if dum(0) ne '' then goto,skipyear
;
; loop over days of the year
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
    filename=file_search(dir+syear+mmdd(iday)+'.nc3')
;
    ncfile0=filename(0)
    ncid=ncdf_open(ncfile0)
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
        if result.name eq 'latitude' then alat=data
;       if result.name eq 'longitude' then alon=data
        if result.name eq 'theta' then th=data
;       if result.name eq 'IPV' then ipv=data
        if result.name eq 'P' then p=data
;       if result.name eq 'U' then u=data
;       if result.name eq 'V' then v=data
;       if result.name eq 'QDF' then qdf=data
;       if result.name eq 'CO' then co=data*1.e6
        if result.name eq 'GPH' then z=data/1000.
;       if result.name eq 'SF' then sf=data
        if result.name eq 'MARK' then mark=data
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
    index=where(mark lt 0.)
    if index(0) ne -1L then mark(index)=-1.
;
    if iday eq 0L then begin
       pzm=fltarr(ndays,nr,nth)
       zzm=fltarr(ndays,nr,nth)
       mzm=fltarr(ndays,nr,nth)
    endif
;
; retain all daily zonal mean marker fields in one yearly file
;
        pzm(iday,*,*)=mean(p,dim=2)
        zzm(iday,*,*)=mean(z,dim=2)
        mzm(iday,*,*)=mean(mark,dim=2)
    endfor	; loop over files
;
; save daily mean of all years
;
    print,'saving '+ofile
    lat=alat
    save,filename=ofile,nr,nth,lat,th,ndays,mmdd,pzm,zzm,mzm
skipyear:
endfor	; loop over years
end
