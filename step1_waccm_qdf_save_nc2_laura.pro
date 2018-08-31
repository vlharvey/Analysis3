;
; for Laura and Matthias
; compute QDF and compute vortex marker field from WACCM data that is already on theta and
; has streamfunction (psi), PV, and zeta (relative vorticity)
; store .nc3 files with qdf and marker field
;
@marker_lows_v7

loadct,39
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
;
; read dmps_0009-12-27.nc
;
idir='/Users/harvey/Brakebusch/Datfiles/'
ifile=idir+'dmps_0009-12-27.nc'
ofile=idir+'dmps_0009-12-27.sav'
ncid=ncdf_open(ifile)
result0=ncdf_inquire(ncid)
for idim=0,result0.ndims-1 do begin
    ncdf_diminq,ncid,idim,name,dim
    if name eq 'longitude' then nc=dim
    if name eq 'latitude' then nr=dim
    if name eq 'theta' then nth=dim
    if name eq 'time' then nt=dim
    print,'read ',name,' dimension ',dim
endfor
for ivar=0,result0.nvars-1 do begin
    result=ncdf_varinq(ncid,ivar)
    ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
    if result.name eq 'elat' then elatgrd=data		; equivalent latitude (deg)
    if result.name eq 'latitude' then alat=data		; latitude  (deg)
    if result.name eq 'longitude' then alon=data	; longitude (deg E)
    if result.name eq 'p' then pgrd=data		; pressure  (hPa)
    if result.name eq 'phi' then vpgrd=data		; velocity potential (m^2/s)
    if result.name eq 'psi' then sfgrd=data		; stream function (m^2/s)
    if result.name eq 'pv' then ipvgrd=data		; Ertel potential vorticity (K m^2 /s /kg)
    if result.name eq 'spv' then spvgrd=data		; scaled potential vorticity (K m^2 /s /kg)
    if result.name eq 't' then tgrd=data		; temperature (K)
    if result.name eq 'theta' then thlev=data		; potential temperature (K)
    if result.name eq 'time' then time=data		; time (days since 20040901 0UTC)
    if result.name eq 'u' then ugrd=data		; zonal wind (m/s)
    if result.name eq 'v' then vgrd=data		; meridional wind (m/s)
    if result.name eq 'zeta' then zetagrd=data		; relative vorticity (1/s)
    print,ivar,result.name,min(data),max(data)
endfor
ncdf_close,ncid
;
; COMPUTE Q diagnostic
;
QDFGRD=fltarr(nc,nr,nth,nt)
MARKGRD=fltarr(nc,nr,nth,nt)
;
; loop over time
;
for itime=0,nt-1L do begin
;
; LOOP OVER LATITUDES
;
for LAT=0L,NR-1L do begin
    JP1=LAT-1
    JM1=LAT+1
    IF LAT EQ 0 THEN JP1=0
    IF LAT EQ NR-1L THEN JM1=NR-1L
    DY1=RADEA*(ALAT(JP1)-ALAT(JM1))*DTR
    DX1=RADEA*COS(ALAT(LAT)*DTR)*PI2/(.5*NC)
;
; LOOP OVER LONGITUDES
;
    for I=0,NC-1L do begin
        IP1=I+1
        IM1=I-1
        IF I EQ 0 THEN IM1=NC-1
        IF I EQ NC-1 THEN IP1=0
;
; LOOP OVER THETA LEVELS
;
        for K=0,NTH-1L do begin
;
; normalized by RADEA. The signed sqrt of Q is taken.
;
            arg1 = (UGRD(IP1,LAT,K,itime)-UGRD(IM1,LAT,K,itime))/DX1 $
                  - VGRD(I,LAT,K,itime)*TAN(ALAT(LAT)*DTR)/RADEA
            arg2 = (VGRD(IP1,LAT,K,itime)-VGRD(IM1,LAT,K,itime))/DX1 $
                  + UGRD(I,LAT,K,itime)*TAN(ALAT(LAT)*DTR)/RADEA
            DVDY = (VGRD(I,JP1,K,itime)-VGRD(I,JM1,K,itime))/DY1
            DUDY = (UGRD(I,JP1,K,itime)-UGRD(I,JM1,K,itime))/DY1
            if abs(arg1) gt 1.e12 or abs(arg2) gt 1.e12 then QDFGRD(I,LAT,K,itime) = 1.e12
            if abs(arg1) lt 1.e12 and abs(arg2) lt 1.e12 then begin
               qtemp=(0.5*(arg1*arg1+DVDY*DVDY)+arg2*DUDY)*RADEA*RADEA
               if qtemp ge 0.0 then QDFGRD(I,LAT,K,itime) = sqrt(qtemp)
               if qtemp lt 0.0 then QDFGRD(I,LAT,K,itime) = -sqrt(-qtemp)
            endif
        endfor	; loop over theta
    endfor	; loop over longitudes
endfor		; loop over latitudes
;
; average QDF for polar rows
;
for k= 0, NTH-1 do begin
    xlst=0.0
    frst=0.0
    for I = 0, nc-1 do begin
       xlst = xlst + QDFGRD(i,nr-2,k,itime)/float(nc)
       frst = frst + QDFGRD(i,1,k,itime)/float(nc)
    endfor
    for I = 0, nc-1 do begin
       QDFGRD(i,nr-1,k,itime) = xlst
       QDFGRD(i,0,k,itime) = frst
    endfor
;
; check
; 
    rlev=thlev(k)
    index=where(thlev eq rlev)
    ilev=index(0)
    slev=string(rlev)
    pp=qdfgrd(*,*,ilev,itime)
    level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
    !type=2^2+2^3
    erase
    contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
            yrange=[-90.,90.],title=ifile+'  '+slev+' K'
    contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
    contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot

    theta=thlev(k)
    u1=ugrd(*,*,k,itime)
    v1=vgrd(*,*,k,itime)
    qdf1=qdfgrd(*,*,k,itime)
    sf1=sfgrd(*,*,k,itime)
    zeta1=zetagrd(*,*,k,itime)
    zeta=fltarr(nc+1,nr)
    zeta(0:nc-1,0:nr-1)=zeta1(0:nc-1,0:nr-1)
    zeta(nc,*)=zeta(0,*)
    u=fltarr(nc+1,nr)
    u(0:nc-1,0:nr-1)=u1(0:nc-1,0:nr-1)
    u(nc,*)=u(0,*)
    v=fltarr(nc+1,nr)
    v(0:nc-1,0:nr-1)=v1(0:nc-1,0:nr-1)
    v(nc,*)=v(0,*)
    qdf=fltarr(nc+1,nr)
    qdf(0:nc-1,0:nr-1)=qdf1(0:nc-1,0:nr-1)
    qdf(nc,*)=qdf(0,*)
    sf=0.*fltarr(nc+1,nr)
    sf(0:nc-1,0:nr-1)=sf1(0:nc-1,0:nr-1)
    sf(nc,*)=sf(0,*)
speed=sqrt(u*u+v*v)
    x2d=0.*sf
    y2d=0.*sf
    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.
    for i=0,nc do y2d(i,*)=alat
    for j=0,nr-1 do x2d(*,j)=x
;
; streamfunction based polar vortex marker
;
    markl=0.*qdf
    marker_lows_v7,sf,markl,qdf,zeta,u,v,x,alat,theta

    pp=sf
    level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
;    contour,pp,x,alat,levels=level,/noeras,thick=3,/overplot
    contour,speed,x,alat,levels=20*findgen(20),/noeras,/overplot,thick=3
    index=where(qdf lt 0.)
;   if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=1,color=mcolor,symsize=0.5
    contour,markl,x,alat,levels=[.1],/overplot,thick=10

    markl1=0.*qdf1
    markl1(0:nc-1,0:nr-1)=markl(0:nc-1,0:nr-1)
    markgrd(*,*,k,itime)=markl1
endfor	; loop over theta
endfor	; loop over days
;
; write marker file
;
save,filename=ofile,alon,alat,thlev,time,markgrd

;ofile=dirh+sdate+'.nc3'
;write_geos5_nc3_brakebusch,ofile,nc,nr,nth,alon,alat,th,pv2,p2,u2,v2,qdf2,mark2,vp2,sf2
end
