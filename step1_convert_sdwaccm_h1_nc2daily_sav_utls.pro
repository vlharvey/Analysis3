;
; read SDWACCM h1 netcdf data files that I made to continue Doug K. simulation
; output daily .sav files
;
loadct,39
mcolor=255
device,decompose=0
window
!p.background=mcolor
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/sdwaccm2012-2014_1_2_2.cam.h1.*
;
; H2O, N2O, O3, QRS_TOT, QRL_TOT, T, U, V, Z3
;
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
spawn,'ls '+dir+'sdwaccm2012-2014_1_2_2.cam.h1.201*-00000.nc',ifiles
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
        if result.name eq 'date' then begin
           date=data
           print,date(0)
           dum=findfile(dir+'sdwaccm2012-2014_1_2_2.cam.h1.utls.'+strcompress(date(0),/remove_all)+'.sav')
;          if dum(0) ne '' then goto,jumpnc
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
; check
;
erase
th=0.*t
for k=0L,n_elements(lev)-1L do th(*,*,k)=t(*,*,k)*(1000./lev(k))^0.286

index=where(min(abs(lev-200.)) eq abs(lev-200.))
plev=reform(p(*,*,index(0)))
zlev=reform(z(*,*,index(0)))/1000.
tlev=reform(t(*,*,index(0)))
thlev=tlev*(1000./plev)^0.286
h2olev=reform(h2o(*,*,index(0)))
o3lev=reform(o3(*,*,index(0)))

h2o2d=mean(h2o,dim=1)
o32d=mean(o3,dim=1)
z2d=mean(z,dim=1)/1000.
th2d=mean(th,dim=1)

xindex=where(lon ge 60. and lon le 120.)
h2o3da=reform(h2o(xindex,*,*))
o33da=reform(o3(xindex,*,*))
h2o2da=100.*(mean(h2o3da,dim=1)-h2o2d)/h2o2d		; % difference in mean water in asian sector over zonal mean
o32da=100.*(mean(o33da,dim=1)-o32d)/o32d		; % difference in mean ozone in asian sector over zonal mean

!type=2^2+2^3
set_viewport,.1,.45,.55,.95
map_set,0,0,0,/contin,/grid,/noeras,color=0,title=today+' H2O at 200 hPa'	;,limit=[0.,0.,60.,180.]
level=[1,2,3,4,5,10,15,20,25,30,40,50,60,75,100,125,150]
nlvls=n_elements(level)
col1=(findgen(nlvls)/float(nlvls))*mcolor
contour,h2olev*1.e6,lon,lat,/overplot,levels=level,c_color=col1,/cell
contour,h2olev*1.e6,lon,lat,/overplot,levels=level,color=0,/follow
map_set,0,0,0,/contin,/grid,/noeras,color=0,title=today+' H2O at 200 hPa'	;,limit=[0.,0.,60.,180.]
contour,zlev,lon,lat,/overplot,level=10+0.1*findgen(31),color=mcolor,thick=3,/follow

set_viewport,.55,.9,.55,.95
map_set,0,0,0,/contin,/grid,/noeras,color=0,title=today+' O3 at 200 hPa'	;,limit=[0.,0.,60.,180.]
level=0.05*findgen(30)
nlvls=n_elements(level)
col1=(findgen(nlvls)/float(nlvls))*mcolor
contour,o3lev*1.e6,lon,lat,/overplot,levels=level,c_color=col1,/cell
contour,o3lev*1.e6,lon,lat,/overplot,levels=level,color=0,/follow
map_set,0,0,0,/contin,/grid,/noeras,color=0,title=today+' O3 at 200 hPa'	;,limit=[0.,0.,60.,180.]
contour,zlev,lon,lat,/overplot,level=10+0.1*findgen(31),color=mcolor,thick=3,/follow

set_viewport,.1,.45,.05,.45
level=-200+20*findgen(21)
nlvls=n_elements(level)
col1=(findgen(nlvls)/float(nlvls))*mcolor
contour,h2o2da,lat,lev,color=0,levels=level,c_color=col1,/cell,/ylog,yrange=[1000.,50.],/noeras
contour,h2o2da,lat,lev,/overplot,levels=level,color=0,/follow,/ylog
contour,z2d,lat,lev,/overplot,level=10+findgen(21),color=mcolor,thick=3,/follow,/ylog
contour,th2d,lat,lev,/overplot,level=300+10*findgen(21),color=0,thick=3,/follow,/ylog

set_viewport,.55,.9,.05,.45
contour,o32da,lat,lev,color=0,levels=level,c_color=col1,/cell,/ylog,yrange=[1000.,50.],/noeras
contour,o32da,lat,lev,/overplot,levels=level,color=0,/follow,/ylog
contour,z2d,lat,lev,/overplot,level=10+findgen(21),color=mcolor,thick=3,/follow,/ylog
contour,th2d,lat,lev,/overplot,level=300+10*findgen(21),color=0,thick=3,/follow,/ylog
wait,1
stop
;
; save daily file
;
        ofile=dir+'sdwaccm2012-2014_1_2_2.cam.h1.utls.'+today+'.sav'
        print,ofile
        save,file=ofile,LAT,LON,LEV,P,O3,H2O,N2O,QSUM,T,U,V,Z
    endfor		; loop over days
jumpnc:
    ncdf_close,ncid
endfor			; loop over 10-day files
end
