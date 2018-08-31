;
; read WACCM4 netcdf data from Ethan Peck
; convert 20-day files to daily files
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
; ALT             FLOAT     = Array[66]		; globally averaged Z3
; CLONO2NAFPL     FLOAT     = Array[96, 66]
; DATENAFPL       FLOAT     = Array[1]
; LAT             DOUBLE    = Array[96]
; NOXNAFPL        FLOAT     = Array[96, 66]
; NOYNAFPL        FLOAT     = Array[96, 66]
; O3NAFPL         FLOAT     = Array[96, 66]
; OMEGANAFPL      FLOAT     = Array[96, 66]
; P               FLOAT     = Array[66]		; globally averaged pressure
; QSUMNAFPL       FLOAT     = Array[96, 66]
; TNAFPL          FLOAT     = Array[96, 66]
; UNAFPL          FLOAT     = Array[96, 66]
; VNAFPL          FLOAT     = Array[96, 66]
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW/mee00fpl_FW.cam2.h3.'
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW/noaurfpl_FW.cam2.h3.'
spawn,'ls '+dir+'*00000.nc',ifiles
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
        print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        if result.name eq 'CH4' or result.name eq 'CO' or result.name eq 'MSKtem' or result.name eq 'N2O' or $
           result.name eq 'NO' or result.name eq 'TH' or result.name eq 'TTGW' or result.name eq 'UV3d' or $
           result.name eq 'UW3d' or result.name eq 'VTH3d' then goto,jumpvar
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'P0' then p0=data		; Pa
        if result.name eq 'hyai' then hyai=data
        if result.name eq 'hybi' then hybi=data
        if result.name eq 'hyam' then hyam=data
        if result.name eq 'hybm' then hybm=data
        if result.name eq 'PS' then ps3d=data		; Pa
        if result.name eq 'TS' then ts3d=data
        if result.name eq 'lat' then lat=data
        if result.name eq 'lon' then lon=data
        if result.name eq 'lev' then lev=data
        if result.name eq 'time' then time=data
        if result.name eq 'date' then date=data
;       if result.name eq 'CH4' then ch44d=data
        if result.name eq 'CLONO2' then clono24d=data
;       if result.name eq 'CO' then co4d=data
;       if result.name eq 'MSKtem' then MSKtem4d=data
;       if result.name eq 'N2O' then n2o4d=data
;       if result.name eq 'NO' then no4d=data
        if result.name eq 'NOX' then nox4d=data
        if result.name eq 'NOY' then noy4d=data
        if result.name eq 'O3' then o34d=data
        if result.name eq 'OMEGA' then omega4d=data
        if result.name eq 'QSUM' then qsum4d=data
        if result.name eq 'T' then t4d=data
;       if result.name eq 'TH' then th4d=data
;       if result.name eq 'TTGW' then ttgw4d=data
        if result.name eq 'U' then u4d=data
        if result.name eq 'V' then v4d=data
;       if result.name eq 'UV3d' then uv3d4d=data
;       if result.name eq 'UW3d' then uw3d4d=data
;       if result.name eq 'VTH3d' then vth3d4d=data
        if result.name eq 'Z3' then z4d=data
jumpvar:
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid

    sdate=strcompress(date,/remove_all)
    index=where(date lt 100101)
    if index(0) ne -1L then sdate(index)='0'+sdate(index)
    syear=strmid(sdate,0,2)
    smon=strmid(sdate,2,2)
    sday=strmid(sdate,4,2)
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
        DATENAFPL=sdate(iday)
;
; global averaged p and z
;
        p=fltarr(nl)
        alt=fltarr(nl)
        CLONO2NAFPL=fltarr(nr,nl)
        NOXNAFPL=fltarr(nr,nl)
        NOYNAFPL=fltarr(nr,nl)
        O3NAFPL=fltarr(nr,nl)
        OMEGANAFPL=fltarr(nr,nl)
        QSUMNAFPL=fltarr(nr,nl)
        TNAFPL=fltarr(nr,nl)
        UNAFPL=fltarr(nr,nl)
        VNAFPL=fltarr(nr,nl)
        for k=0L,nl-1L do begin
            p(k)=mean(p4d(*,*,k,iday))
            alt(k)=mean(z4d(*,*,k,iday))
;
; zonally averaged variables
;
            for j=0L,nr-1L do begin
                CLONO2NAFPL(j,k)=mean(clono24d(*,j,k,iday))
                NOXNAFPL(j,k)=mean(nox4d(*,j,k,iday))
                NOYNAFPL(j,k)=mean(noy4d(*,j,k,iday))
                O3NAFPL(j,k)=mean(o34d(*,j,k,iday))
                OMEGANAFPL(j,k)=mean(omega4d(*,j,k,iday))
                QSUMNAFPL(j,k)=mean(qsum4d(*,j,k,iday))
                TNAFPL(j,k)=mean(t4d(*,j,k,iday))
                UNAFPL(j,k)=mean(u4d(*,j,k,iday))
                VNAFPL(j,k)=mean(v4d(*,j,k,iday))
            endfor	; loop over latitude
        endfor		; loop over altitude
        ofile=dir+'Year'+syear(iday)+'_'+smon(iday)+sday(iday)+'.sav'
        print,ofile
        save,file=ofile,LAT,P,ALT,DATENAFPL,CLONO2NAFPL,NOXNAFPL,NOYNAFPL,O3NAFPL,$
             OMEGANAFPL,QSUMNAFPL,TNAFPL,UNAFPL,VNAFPL
    endfor		; loop over days
endfor			; loop over 20-day files
end
