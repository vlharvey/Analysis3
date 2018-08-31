;
; read WACCM netcdf data from Amal
; convert multi-day files to daily files
;
; /Volumes/Data/WACCM/Datfiles_Chandran/wa3548_2x_refb1.1.cam2.h3.1997-10-20-00000.nc
;
;dimensions:
;        lat = 96 ;
;        lon = 144 ;
;        slat = 95 ;
;        slon = 144 ;
;        lev = 66 ;
;        ilev = 67 ;
;        isccp_prs = 7 ;
;        isccp_tau = 7 ;
;        isccp_prstau = 49 ;
;        time = UNLIMITED ; // (73 currently)
;        tbnd = 2 ;
;        chars = 8 ;
;variables:
;        double P0 ;
;                P0:long_name = "reference pressure" ;
;                P0:units = "Pa" ;
;        double lat(lat) ;
;                lat:long_name = "latitude" ;
;                lat:units = "degrees_north" ;
;        double lon(lon) ;
;                lon:long_name = "longitude" ;
;                lon:units = "degrees_east" ;
;        double slat(slat) ;
;                slat:long_name = "staggered latitude" ;
;                slat:units = "degrees_north" ;
;        double slon(slon) ;
;                slon:long_name = "staggered longitude" ;
;                slon:units = "degrees_east" ;
;        double w_stag(slat) ;
;                w_stag:long_name = "staggered latitude weights" ;
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
;        double isccp_prs(isccp_prs) ;
;                isccp_prs:long_name = "Mean ISCCP pressure" ;
;                isccp_prs:units = "mb" ;
;                isccp_prs:isccp_prs_bnds = 0., 180., 310., 440., 560., 680., 800., 1000. ;
;        double isccp_tau(isccp_tau) ;
;                isccp_tau:long_name = "Mean ISCCP optical depth" ;
;                isccp_tau:units = "unitless" ;
;                isccp_tau:isccp_tau_bnds = 0., 0.3, 1.3, 3.6, 9.4, 23., 60., 379. ;
;        double isccp_prstau(isccp_prstau) ;
;                isccp_prstau:long_name = "Mean pressure (mb).mean optical depth (unitless)/1000" ;
;                isccp_prstau:units = "mixed" ;
;        double time(time) ;
;                time:long_name = "time" ;
;                time:units = "days since 1953-01-01 00:00:00" ;
;                time:calendar = "noleap" ;
;                time:bounds = "time_bnds" ;
;        double time_bnds(time, tbnd) ;
;                time_bnds:long_name = "time interval endpoints" ;
;        char date_written(time, chars) ;
;        char time_written(time, chars) ;
;        int ntrm ;
;                ntrm:long_name = "spectral truncation parameter M" ;
;        int ntrn ;
;                ntrn:long_name = "spectral truncation parameter N" ;
;        int ntrk ;
;                ntrk:long_name = "spectral truncation parameter K" ;
;        int ndbase ;
;                ndbase:long_name = "base day" ;
;        int nsbase ;
;                nsbase:long_name = "seconds of base day" ;
;        int nbdate ;
;                nbdate:long_name = "base date (YYYYMMDD)" ;
;        int nbsec ;
;                nbsec:long_name = "seconds of base date" ;
;        int mdt ;
;                mdt:long_name = "timestep" ;
;                mdt:units = "s" ;
;        int nlon(lat) ;
;                nlon:long_name = "number of longitudes" ;
;        int wnummax(lat) ;
;                wnummax:long_name = "cutoff Fourier wavenumber" ;
;        double hyai(ilev) ;
;                hyai:long_name = "hybrid A coefficient at layer interfaces" ;
;        double hybi(ilev) ;
;                hybi:long_name = "hybrid B coefficient at layer interfaces" ;
;        double hyam(lev) ;
;                hyam:long_name = "hybrid A coefficient at layer midpoints" ;
;        double hybm(lev) ;
;                hybm:long_name = "hybrid B coefficient at layer midpoints" ;
;        double gw(lat) ;
;                gw:long_name = "gauss weights" ;
;        int ndcur(time) ;
;                ndcur:long_name = "current day (from base day)" ;
;        int nscur(time) ;
;                nscur:long_name = "current seconds of current day" ;
;        int date(time) ;
;                date:long_name = "current date (YYYYMMDD)" ;
;        double co2vmr(time) ;
;                co2vmr:long_name = "co2 volume mixing ratio" ;
;        int datesec(time) ;
;                datesec:long_name = "current seconds of current date" ;
;        double f107(time) ;
;                f107:long_name = "81-day centered mean of 10.7 cm solar radio flux (F10.7)" ;
;                f107:units = "10^-22 W m^-2 Hz^-1" ;
;        double f107a(time) ;
;                f107a:long_name = "10.7 cm solar radio flux (F10.7)" ;
;        double kp(time) ;
;                kp:long_name = "Daily planetary K geomagnetic index" ;
;        double ap(time) ;
;                ap:long_name = "Daily planetary A geomagnetic index" ;
;        int nsteph(time) ;
;                nsteph:long_name = "current timestep" ;
;        float FRONTGF(time, lev, lat, lon) ;
;                FRONTGF:units = "K^2/M^2/S" ;
;                FRONTGF:long_name = "Frontogenesis function at gws src level" ;
;        float OMEGA(time, lev, lat, lon) ;
;                OMEGA:units = "Pa/s" ;
;                OMEGA:long_name = "Vertical velocity (pressure)" ;
;                OMEGA:cell_method = "time: mean" ;
;        float PHIS(time, lat, lon) ;
;                PHIS:units = "m2/s2" ;
;                PHIS:long_name = "Surface geopotential" ;
;                PHIS:cell_method = "time: mean" ;
;        float PS(time, lat, lon) ;
;                PS:units = "Pa" ;
;                PS:long_name = "Surface pressure" ;
;                PS:cell_method = "time: mean" ;
;        float PSL(time, lat, lon) ;
;                PSL:units = "Pa" ;
;                PSL:long_name = "Sea level pressure" ;
;                PSL:cell_method = "time: mean" ;
;        float T(time, lev, lat, lon) ;
;                T:units = "K" ;
;                T:long_name = "Temperature" ;
;                T:cell_method = "time: mean" ;
;        float U(time, lev, lat, lon) ;
;                U:units = "m/s" ;
;                U:long_name = "Zonal wind" ;
;                U:cell_method = "time: mean" ;
;        float V(time, lev, lat, lon) ;
;                V:units = "m/s" ;
;                V:long_name = "Meridional wind" ;
;                V:cell_method = "time: mean" ;
;        float Z3(time, lev, lat, lon) ;
;                Z3:units = "m" ;
;                Z3:long_name = "Geopotential Height (above sea level)" ;
;                Z3:cell_method = "time: mean" ;
;
dir='/Volumes/Data/WACCM/Datfiles_Chandran/wa3548_2x_refb1.1.cam2.h3.'
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
        if name eq 'ilev' then nl1=dim
        if name eq 'time' then nt=dim
;       print,'read ',name,' dimension ',dim
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
        if result.name eq 'T' then t4d=data
        if result.name eq 'U' then u4d=data
        if result.name eq 'V' then v4d=data
        if result.name eq 'Z3' then z4d=data
jumpvar:
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; IDL save file for each day
;
    for iday=0L,nt-1L do begin 
        sdate=strcompress(date(iday),/remove_all)
        syear=strmid(sdate,0,4)
        smon=strmid(sdate,4,2)
        sday=strmid(sdate,6,2)

        t3d=reform(t4d(*,*,*,iday))
        u3d=reform(u4d(*,*,*,iday))
        v3d=reform(v4d(*,*,*,iday))
        z3d=reform(z4d(*,*,*,iday))

        ofile=dir+syear+smon+sday+'.sav'
        print,ofile
        save,file=ofile,LON,LAT,LEV,t3d,u3d,v3d,z3d
    endfor		; loop over days
;   spawn,'rm -f '+ncfile0
endfor			; loop over days in files
end
