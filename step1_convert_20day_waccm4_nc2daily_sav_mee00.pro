;
; read WACCM4 netcdf data from Ethan Peck
; convert 20-day files to daily files
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/mee00fpl_FW2.cam2.h3.dyns.*00000.nc
;
; dimensions:
;        time = UNLIMITED ; // (20 currently)
;        lev = 66 ;
;        ilev = 67 ;
;        lat = 96 ;
;        lon = 144 ;
;variables:
;        double P0 ;
;                P0:long_name = "reference pressure" ;
;                P0:units = "Pa" ;
;        double lev(lev) ;
;                lev:long_name = "hybrid level at midpoints (1000*(A+B))" ;
;                lev:units = "level" ;
;                lev:positive = "down" ;
;                lev:standard_name = "atmosphere_hybrid_sigma_pressure_coordinate" ;
;                lev:formula_terms = "a: hyam b: hybm p0: P0 ps: PS" ;
;        double ilev(ilev) ;
;                ilev:long_name = "hybrid level at interfaces (1000*(A+B))" ;
;                ilev:units = "level" ;
;                ilev:positive = "down" ;
;                ilev:standard_name = "atmosphere_hybrid_sigma_pressure_coordinate" ;
;                ilev:formula_terms = "a: hyai b: hybi p0: P0 ps: PS" ;
;        double lat(lat) ;
;                lat:long_name = "latitude" ;
;                lat:units = "degrees_north" ;
;        double lon(lon) ;
;                lon:long_name = "longitude" ;
;                lon:units = "degrees_east" ;
;        double hyai(ilev) ;
;                hyai:long_name = "hybrid A coefficient at layer interfaces" ;
;        double hybi(ilev) ;
;                hybi:long_name = "hybrid B coefficient at layer interfaces" ;
;        double hyam(lev) ;
;                hyam:long_name = "hybrid A coefficient at layer midpoints" ;
;        double hybm(lev) ;
;                hybm:long_name = "hybrid B coefficient at layer midpoints" ;
;        double time(time) ;
;                time:long_name = "time" ;
;                time:units = "days since 0001-01-01 00:00:00" ;
;                time:calendar = "noleap" ;
;                time:bounds = "time_bnds" ;
;        int date(time) ;
;                date:long_name = "current date (YYYYMMDD)" ;
;        float PS(time, lat, lon) ;
;                PS:units = "Pa" ;
;                PS:long_name = "Surface pressure" ;
;                PS:cell_methods = "time: mean" ;
;        float PSL(time, lat, lon) ;
;                PSL:units = "Pa" ;
;                PSL:long_name = "Sea level pressure" ;
;                PSL:cell_methods = "time: mean" ;
;        float T(time, lev, lat, lon) ;
;                T:mdims = 1 ;
;                T:units = "K" ;
;                T:long_name = "Temperature" ;
;                T:cell_methods = "time: mean" ;
;        float U(time, lev, lat, lon) ;
;                U:mdims = 1 ;
;                U:units = "m/s" ;
;                U:long_name = "Zonal wind" ;
;                U:cell_methods = "time: mean" ;
;        float V(time, lev, lat, lon) ;
;                V:mdims = 1 ;
;                V:units = "m/s" ;
;                V:long_name = "Meridional wind" ;
;                V:cell_methods = "time: mean" ;
;        float OMEGA(time, lev, lat, lon) ;
;                OMEGA:mdims = 1 ;
;                OMEGA:units = "Pa/s" ;
;                OMEGA:long_name = "Vertical velocity (pressure)" ;
;                OMEGA:cell_methods = "time: mean" ;
;        float Z3(time, lev, lat, lon) ;
;                Z3:mdims = 1 ;
;                Z3:units = "m" ;
;                Z3:long_name = "Geopotential Height (above sea level)" ;
;                Z3:cell_methods = "time: mean" ;
;        float QSUM(time, lev, lat, lon) ;
;                QSUM:mdims = 1 ;
;                QSUM:units = "/s" ;
;                QSUM:long_name = "total ion production" ;
;                QSUM:cell_methods = "time: mean" ;
;        float TS(time, lat, lon) ;
;                TS:units = "K" ;
;                TS:long_name = "Surface temperature (radiative)" ;
;                TS:cell_methods = "time: mean" ;
;        float TTGW(time, lev, lat, lon) ;
;                TTGW:mdims = 1 ;
;                TTGW:units = "K/s" ;
;                TTGW:long_name = "T tendency - gravity wave drag" ;
;                TTGW:cell_methods = "time: mean" ;
;        float UW3d(time, ilev, lat, lon) ;
;                UW3d:mdims = 2 ;
;                UW3d:units = "M2/S2" ;
;                UW3d:long_name = "Vertical Flux of Zonal Momentum: 3D zon. mean" ;
;                UW3d:cell_methods = "time: mean" ;
;        float UV3d(time, ilev, lat, lon) ;
;                UV3d:mdims = 2 ;
;                UV3d:units = "M2/S2" ;
;                UV3d:long_name = "Meridional Flux of Zonal Momentum: 3D zon. mean" ;
;                UV3d:cell_methods = "time: mean" ;
;        float VTH3d(time, ilev, lat, lon) ;
;                VTH3d:mdims = 2 ;
;                VTH3d:units = "MK/S" ;
;                VTH3d:long_name = "Meridional Heat Flux: 3D zon. mean" ;
;                VTH3d:cell_methods = "time: mean" ;
;        float TH(time, ilev, lat, lon) ;
;                TH:mdims = 2 ;
;                TH:units = "K" ;
;                TH:long_name = "Potential Temperature" ;
;                TH:cell_methods = "time: mean" ;
;        float QRS_TOT(time, lev, lat, lon) ;
;                QRS_TOT:mdims = 1 ;
;                QRS_TOT:units = "K/s" ;
;                QRS_TOT:long_name = "Merged SW heating: QRS+QCP+QRS_EUV+QRS_CO2NIR+QRS_AUR+QTHERMAL" ;
;                QRS_TOT:cell_methods = "time: mean" ;
;        float QRL_TOT(time, lev, lat, lon) ;
;                QRL_TOT:mdims = 1 ;
;                QRL_TOT:units = "K/s" ;
;                QRL_TOT:long_name = "Merged LW heating: QRL+QRLNLTE" ;
;                QRL_TOT:cell_methods = "time: mean" ;
;        float AOA1(time, lev, lat, lon) ;
;                AOA1:mdims = 1 ;
;                AOA1:units = "kg/kg" ;
;                AOA1:long_name = "Age-of_air tracer 1" ;
;                AOA1:cell_methods = "time: mean" ;
;
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/mee00fpl_FW2.cam2.h3.dyns.'
;spawn,'ls '+dir+'*00000.nc',ifiles
spawn,'ls '+dir+'0002-12-22-00000.nc',ifiles
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
        if name eq 'ilev' then nl1=dim
        if name eq 'time' then nt=dim
        print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'P0' then p0=data		; Pa
        if result.name eq 'hyai' then hyai=data
        if result.name eq 'hybi' then hybi=data
        if result.name eq 'hyam' then hyam=data
        if result.name eq 'hybm' then hybm=data
        if result.name eq 'PS' then ps3d=data		; Pa
        if result.name eq 'PSL' then psl3d=data		; Pa
        if result.name eq 'lat' then lat=data
        if result.name eq 'lon' then lon=data
        if result.name eq 'lev' then lev=data
        if result.name eq 'ilev' then ilev=data
        if result.name eq 'time' then time=data		; days since 0001-01-01 00:00:00
        if result.name eq 'date' then date=data
        if result.name eq 'OMEGA' then omega4d=data
        if result.name eq 'T' then t4d=data
        if result.name eq 'TS' then ts3d=data
        if result.name eq 'TH' then th4d=data
        if result.name eq 'TTGW' then ttgw4d=data
        if result.name eq 'U' then u4d=data
        if result.name eq 'V' then v4d=data
        if result.name eq 'UV3d' then uv3d4d=data
        if result.name eq 'UW3d' then uw3d4d=data
        if result.name eq 'VTH3d' then vth3d4d=data
        if result.name eq 'QRL_TOT' then qrltot4d=data
        if result.name eq 'QRS_TOT' then qrstot4d=data
        if result.name eq 'Z3' then z4d=data
jumpvar:
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; Calculate Pressure: p(i,j,k,n) = A(k)*PO + B(k)*PS3d(i,j,n) in hPa
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
        sdate=strcompress(date(iday),/remove_all)
        index=where(date lt 100101)
        if index(0) ne -1L then sdate(index)='0'+sdate(index)
        syear=strmid(sdate,0,2)
        smon=strmid(sdate,2,2)
        sday=strmid(sdate,4,2)

        ps2d=reform(ps3d(*,*,iday))
        ts2d=reform(ts3d(*,*,iday))
        p3d=reform(p4d(*,*,*,iday))
        omega3d=reform(omega4d(*,*,*,iday))
        t3d=reform(t4d(*,*,*,iday))
        th3d=reform(th4d(*,*,*,iday))
        ttgw3d=reform(ttgw4d(*,*,*,iday))
        u3d=reform(u4d(*,*,*,iday))
        v3d=reform(v4d(*,*,*,iday))
        uv3d3d=reform(uv3d4d(*,*,*,iday))
        uw3d3d=reform(uw3d4d(*,*,*,iday))
        vth3d3d=reform(vth3d4d(*,*,*,iday))
        qrltot3d=reform(qrltot4d(*,*,*,iday))
        qrstot3d=reform(qrstot4d(*,*,*,iday))
        z3d=reform(z4d(*,*,*,iday))

        ofile=dir+'20'+syear+smon+sday+'_3D_dyn.sav'
        print,ofile
        save,file=ofile,LON,LAT,LEV,ilev,ps2d,ts2d,p3d,omega3d,t3d,ts2d,th3d,ttgw3d,u3d,$
             v3d,uv3d3d,uw3d3d,vth3d3d,qrltot3d,qrstot3d,z3d
    endfor		; loop over days
    spawn,'rm -f '+ncfile0
endfor			; loop over 20-day files
end
