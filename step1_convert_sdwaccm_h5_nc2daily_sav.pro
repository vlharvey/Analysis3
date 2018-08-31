;
; read SDWACCM h5 netcdf data files from Doug Kinnison
; output daily .sav files
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.*
;
; H2O, N2O, O3, QRS_TOT, QRL_TOT, T, U, V, Z3
;
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
spawn,'ls '+dir+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.*-00000.nc',ifiles
nfiles=n_elements(ifiles)
for ifile=1083L,nfiles-1L do begin
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
        if result.name eq 'BRO' or result.name eq 'BROX' or result.name eq 'CL2O2' or result.name eq 'CLO' or result.name eq 'CLOX' or result.name eq 'DO3CHM' or result.name eq 'DO3CHM_LMS' or result.name eq 'DO3CHM_TRP' or result.name eq 'DTCOND' or result.name eq 'DTCOND_12_COS' or result.name eq 'DTCOND_12_SIN' or result.name eq 'DTCOND_24_COS' or result.name eq 'DTCOND_24_SIN' or result.name eq 'DTV' or result.name eq 'FSDS' or result.name eq 'FSDSC' or result.name eq 'HCL_GAS' or result.name eq 'HNO3_GAS' or result.name eq 'HORZ' or result.name eq 'MASS' or result.name eq 'O3_Loss' or result.name eq 'O3_Prod' or result.name eq 'PRECT' or result.name eq 'QRS_TOT_12_COS' or result.name eq 'QRS_TOT_12_SIN' or result.name eq 'QRS_TOT_24_COS' or result.name eq 'QRS_TOT_24_SIN' or result.name eq 'SAD_SULFC' or result.name eq 'SZA' or result.name eq 'T010' or result.name eq 'T1000' or result.name eq 'TROP_P' or result.name eq 'TROP_T' or result.name eq 'TROP_Z' or result.name eq 'T_12_COS' or result.name eq 'T_12_SIN' or result.name eq 'T_24_COS' or result.name eq 'T_24_SIN' or result.name eq 'U010' or result.name eq 'U850' or result.name eq 'U_12_COS' or result.name eq 'U_12_SIN' or result.name eq 'U_24_COS' or result.name eq 'U_24_SIN' or result.name eq 'V850' or result.name eq 'VERT' or result.name eq 'V_12_COS' or result.name eq 'V_12_SIN' or result.name eq 'V_24_COS' or result.name eq 'V_24_SIN' or result.name eq 'Z1000' or result.name eq 'Z500' then goto,jumpvar
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'P0' then p0=data		; Pa
        if result.name eq 'hyai' then hyai=data
        if result.name eq 'hybi' then hybi=data
        if result.name eq 'hyam' then hyam=data
        if result.name eq 'hybm' then hybm=data
        if result.name eq 'date' then begin
           date=data
           print,date(0)
           dum=findfile(dir+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.'+strcompress(date(0),/remove_all)+'.sav')
           if dum(0) ne '' then goto,jumpnc
        endif
        if result.name eq 'PS' then ps3d=data		; Pa
        if result.name eq 'TS' then ts3d=data
        if result.name eq 'lat' then lat=data
        if result.name eq 'lon' then lon=data
        if result.name eq 'lev' then lev=data
        if result.name eq 'time' then time=data
        if result.name eq 'date' then date=data
;       if result.name eq 'CO' then co4d=data
        if result.name eq 'H2O' then h2o4d=data
        if result.name eq 'N2O' then n2o4d=data
        if result.name eq 'O3' then o34d=data
        if result.name eq 'QRL_TOT' then qrl_tot4d=data
        if result.name eq 'QRS_TOT' then qrs_tot4d=data
        if result.name eq 'T' then t4d=data
        if result.name eq 'U' then u4d=data
        if result.name eq 'V' then v4d=data
        if result.name eq 'Z3' then z4d=data
        print,ivar,result.name,min(data),max(data)
jumpvar:
    endfor
    sdate=strcompress(date,/remove_all)
;
; Calculate 3d Pressure: p(i,j,k,n) = A(k)*PO + B(k)*PS3d(i,j,n) in hPa
;
    p4d=fltarr(nc,nr,nl,nt)
    Pzero=P0
    FOR ilon=0,nc-1 DO $
        FOR ilat=0,nr-1 DO $
            FOR ialt=0,nl-1 DO $
                p4d(ilon,ilat,ialt,*)=(hyam(ialt)*Pzero + hybm(ialt)*PS3d(ilon,ilat,*)) / 100.
;
; IDL save file for each day
;
    for iday=0L,nt-1L do begin 
        today=sdate(iday)
;
; strip out 3d fields on this day
;
        H2O=reform(h2o4d(*,*,*,iday))
        N2O=reform(n2o4d(*,*,*,iday))
        O3=reform(o34d(*,*,*,iday))
        QSUM=86400.*(reform(qrl_tot4d(*,*,*,iday))+reform(qrs_tot4d(*,*,*,iday)))	; K/s -> K/day
        P=reform(p4d(*,*,*,iday))
        T=reform(t4d(*,*,*,iday))
        U=reform(u4d(*,*,*,iday))
        V=reform(v4d(*,*,*,iday))
        Z=reform(z4d(*,*,*,iday))
;
; save daily file
;
        ofile=dir+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.'+today+'.sav'
        print,ofile
        save,file=ofile,LAT,LON,LEV,P,O3,H2O,N2O,QSUM,T,U,V,Z
    endfor		; loop over days
jumpnc:
    ncdf_close,ncid
endfor			; loop over 10-day files
end
