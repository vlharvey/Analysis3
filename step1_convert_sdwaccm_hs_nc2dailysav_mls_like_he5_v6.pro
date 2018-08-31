;
; read SDWACCM version 6 hs netcdf data files from Chuck Bardeen
; output daily .sav files at MLS locations 
; save CO on 37 MLS pressure levels
; save T and Z3 on 55 MLS pressure levels
;
; NOTE: MLS dates do not adhere to WACCM dates!
;
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_SD_v6/'
spawn,'ls '+dir+'c_cesm2_fswd_2005_cntrl.cam.hs.????-*.nc',ifiles
;spawn,'ls '+dir+'c_cesm2_fswd_2005_cntrl.cam.hs.2005-01-0*.nc',ifiles	; testing
nfiles=n_elements(ifiles)
;
; restore one day of MLS to get pressure grids
;
restore,'/atmos/aura6/data/MLS_data/Datfiles_Grid/MLS_grid5_ALL_U_V_v4.2_20040812.sav
np=n_elements(PMLS2)
npco=n_elements(PMLS)
zindex=0.*pmls2
for k = 0,n_elements(PMLS2)-1 do begin
    index = where(pmls eq pmls2(k))
    if index(0) ne -1 then zindex(k) = 1.0
endfor
common_levs=where(zindex eq 1.0)

icount=0L
for ifile=0L,nfiles-1L do begin	; 
;
; read WACCM data
;
    ncfile0=ifiles(ifile)
    print,ncfile0
    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'ncol' then ncol=dim
        if name eq 'lev' then nl=dim
;       print,'read ',name,' dimension ',dim
    endfor
    varid = ncdf_varid(ncid,'P0')
    ncdf_varget,ncid,varid,P0
    varid = ncdf_varid(ncid,'hyai')
    ncdf_varget,ncid,varid,hyai
    varid = ncdf_varid(ncid,'hybi')
    ncdf_varget,ncid,varid,hybi
    varid = ncdf_varid(ncid,'hyam')
    ncdf_varget,ncid,varid,hyam
    varid = ncdf_varid(ncid,'hybm')
    ncdf_varget,ncid,varid,hybm
    varid = ncdf_varid(ncid,'obs_date')		; current date (YYYYMMDD)
    ncdf_varget,ncid,varid,date
    varid = ncdf_varid(ncid,'lat')
    ncdf_varget,ncid,varid,lat
    varid = ncdf_varid(ncid,'lon')
    ncdf_varget,ncid,varid,lon
    varid = ncdf_varid(ncid,'lev')
    ncdf_varget,ncid,varid,lev
    varid = ncdf_varid(ncid,'obs_time')	; current seconds of current date
    ncdf_varget,ncid,varid,datesec
    varid = ncdf_varid(ncid,'PS')		; surface pressure (Pa)
    ncdf_varget,ncid,varid,ps
    varid = ncdf_varid(ncid,'instr_num')	; (int) MLS=1, ACE-FTS=2, HIRDLS=3, ... see global comment
    ncdf_varget,ncid,varid,instr_num
    varid = ncdf_varid(ncid,'CO')
    ncdf_varget,ncid,varid,codata
    varid = ncdf_varid(ncid,'O3')
    ncdf_varget,ncid,varid,o3data
    varid = ncdf_varid(ncid,'T')
    ncdf_varget,ncid,varid,tdata
    varid = ncdf_varid(ncid,'Z3')
    ncdf_varget,ncid,varid,gpdata
    ncdf_close,ncid

    time=24.*datesec/86400.

; Calculate Pressure: p(k,n) = A(k)*PO + B(k)*PS(n) in hPa
;
    pdata=fltarr(nl,ncol)
    FOR ialt=0,nl-1 DO pdata(ialt,*)=(hyam(ialt)*P0 + hybm(ialt)*PS) / 100.
;
; strip out data at measurement locations
;
    comment='instrument: 1=Aura-MLS
    atsosst=where(instr_num eq 1L,nprof)
    if atsosst(0) ne -1L then begin
       lat=reform(lat(atsosst))
       lon=reform(lon(atsosst))
       date=reform(date(atsosst))
       time=reform(time(atsosst))
       codata=transpose(reform(codata(*,atsosst)))
       tdata=transpose(reform(tdata(*,atsosst)))
       gpdata=transpose(reform(gpdata(*,atsosst)))
       pdata=transpose(reform(pdata(*,atsosst)))
       print,'at MLS ',min(date),max(date)
;
; retain data for later separation by day
;
       if icount eq 0L then begin
          lat_all=lat
          lon_all=lon
          date_all=date
          time_all=time
          codata_all=codata
          tdata_all=tdata
          gpdata_all=gpdata
          pdata_all=pdata
       endif
       if icount gt 0L then begin
          lat_all=[lat_all,lat]
          lon_all=[lon_all,lon]
          date_all=[date_all,date]
          time_all=[time_all,time]
          codata_all=[codata_all,codata]
          tdata_all=[tdata_all,tdata]
          gpdata_all=[gpdata_all,gpdata]
          pdata_all=[pdata_all,pdata]
       endif
       icount=icount+1L
    endif
endfor                  ; loop over files
;
; interpolate CO to 37 MLS pressure levels given by "pmls" and T & Z to 55 MLS pressure levels given by "pmls2"
;
nprof=n_elements(lat_all)
codata_all_mlsgrid=fltarr(nprof,npco)
tdata_all_mlsgrid=fltarr(nprof,np)
gpdata_all_mlsgrid=fltarr(nprof,np)
for iprof=0L,n_elements(lat_all)-1L do begin
    coprof=reform(codata_all(iprof,*))
    tprof=reform(tdata_all(iprof,*))
    gpprof=reform(gpdata_all(iprof,*))
    pprof=reform(pdata_all(iprof,*))

    codata_all_mlsgrid(iprof,*) = interpol(coprof,alog(pprof),alog(PMLS))
    tdata_all_mlsgrid(iprof,*) = interpol(tprof,alog(pprof),alog(PMLS2))
    gpdata_all_mlsgrid(iprof,*) = interpol(gpprof,alog(pprof),alog(PMLS2))
endfor
;
; add loop over days
;
sdate_all=strcompress(date_all,/r)
year=long(strmid(sdate_all,0,4))
mm=long(strmid(sdate_all,4,2))
dd=long(strmid(sdate_all,6,2))
jday=julday(mm,dd,year)
jday0=min(jday)
jday1=max(jday)
for iday=jday0,jday1 do begin
    caldat, iday, imn, idy, iyr
    smn=string(format='(i2.2)',imn)
    sdy=string(format='(i2.2)',idy)
    syr=string(format='(i4)',iyr)
    today=where(sdate_all eq syr+smn+sdy,nprof)
;
; save file if there is data today
;
    if today(0) ne -1L then begin
       LATITUDE=lat_all(today)
       LONGITUDE=lon_all(today)
       DATE=date_all(today)
       TIME=time_all(today)
       CO=codata_all_mlsgrid(today,*)
       T=tdata_all_mlsgrid(today,*)
       Z=gpdata_all_mlsgrid(today,*)
       ofile=dir+'c_cesm2_fswd_2005_cntrl.cam.hs.MLS.'+syr+smn+sdy+'.sav
       print,nprof,ofile
       SAVE,FILE=OFILE,LATITUDE,LONGITUDE,DATE,TIME,PMLS,PMLS2,CO,T,Z
    endif
endfor			; loop over days
end
