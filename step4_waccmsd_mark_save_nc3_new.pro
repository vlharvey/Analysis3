;
; compute vortex marker field from WACCM data that is already on theta and
; has streamfunction (psi), PV, and zeta (relative vorticity)
; store .nc3 files with qdf and marker field
;
@comprelvort
@marker_lows_v7
@marker_highs_v6
@marker_circhighs_v7
@write_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
;
; get file listing
;
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
ifiles=file_search(dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.????????.nc2',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    sdate=result(-2)
    print,sdate
    result=file_search(dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.'+sdate+'.nc3')
    if result(0) ne '' then goto,jumpstep
;
; read daily file
;
    ifile=ifiles(n)
    ncid=ncdf_open(ifile)
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
        if result.name eq 'latitude' then alat=data		; latitude  (deg)
        if result.name eq 'longitude' then alon=data	; longitude (deg E)
        if result.name eq 'theta' then th=data           ; potential temperature (K)
        if result.name eq 'P' then p2=data		; pressure  (hPa)
        if result.name eq 'IPV' then pv2=data		; Ertel potential vorticity (K m^2 /s /kg)
        if result.name eq 'U' then u2=data		; zonal wind (m/s)
        if result.name eq 'V' then v2=data		; meridional wind (m/s)
        if result.name eq 'GPH' then g2=data		; geopotential height (m)
        if result.name eq 'QDF' then qdf2=data		; Strain/Rotation Parameter (s-1)
        if result.name eq 'Q' then q2=data		; net diabatic heating rate (K/day)
        if result.name eq 'SF' then sf2=data              ; stream function (m^2/s)
        if result.name eq 'H2O' then h2o2=data              ; water vapor
        if result.name eq 'N2O' then n2o2=data              ; nitrous oxide
        if result.name eq 'O3' then o32=data              ; ozone
        if result.name eq 'CO2' then co22=data
        if result.name eq 'NO' then no2=data
        if result.name eq 'NO2' then no22=data
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; compute vortex edge
;
    MARK2=0.*pv2
;
; loop over theta
; 
    for k=0L,nth-1L do begin
        rlev=th(k)
        slev=string(rlev)
;       pp=transpose(qdf2(*,*,k))
;       level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
;       !type=2^2+2^3
;       erase
;       contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
;               yrange=[-90.,90.],title=ifile+'  '+slev+' K'
;       contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
;       contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
;wait,1
        pv1=transpose(pv2(*,*,k))
        p1=transpose(p2(*,*,k))
        mpv1=pv1*((th(k)/300.))^(-9./2.)
        u1=transpose(u2(*,*,k))
        v1=transpose(v2(*,*,k))
        qdf1=transpose(qdf2(*,*,k))
        sf1=transpose(sf2(*,*,k))
        zeta1=u1*0.0
        comprelvort,u1,v1,zeta1,alon,alat,nc,nr
        zeta=fltarr(nc+1,nr)
        zeta(0:nc-1,0:nr-1)=zeta1(0:nc-1,0:nr-1)
        zeta(nc,*)=zeta(0,*)
        mpv=fltarr(nc+1,nr)
        mpv(0:nc-1,0:nr-1)=mpv1(0:nc-1,0:nr-1)
        mpv(nc,*)=mpv(0,*)
        pv=fltarr(nc+1,nr)
        pv(0:nc-1,0:nr-1)=pv1(0:nc-1,0:nr-1)
        pv(nc,*)=pv(0,*)
        p=fltarr(nc+1,nr)
        p(0:nc-1,0:nr-1)=p1(0:nc-1,0:nr-1)
        p(nc,*)=p(0,*)
        tp=th(k)*(p/1000.)^0.286
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
        marker_lows_v7,sf,markl,qdf,zeta,u,v,x,alat,th(k)
;
; sub-vortex modification
;
        if th(k) eq 500. then begin
           nhindex=where(markl gt 0. and y2d gt 0.)
           shindex=where(markl gt 0. and y2d lt 0.)
           if nhindex(0) ne -1 then mpvnh=mean(mpv(nhindex))
           if shindex(0) ne -1 then mpvsh=mean(mpv(shindex))
;          print,'AVG MPV ',mpvnh,mpvsh
        endif
        if th(k) lt 500. then begin
           index=where(y2d gt 20. and tp gt 240.)
           if index(0) ne -1 then markl(index)=0.

           index=where(y2d lt -20. and tp gt 240.)
           if index(0) ne -1 then markl(index)=0.
        endif
;
; mark anticyclones. This is computationally expensive.
;
        markh=0.*qdf
;       marker_highs_v6,sf,markh,qdf,zeta,u,v,x,alat,pv
;
; like polar vortex marker routine modified to find circumpolar highs
;
        markh2=0.*qdf
        marker_circhighs_v7,sf,markh2,qdf,zeta,u,v,x,alat,th(k)
;
; check for overlap between circumpolar high and non circumpolar highs
;
        lindex=where(markh2 lt 0. and markh lt 0.)
        if lindex(0) ne -1 then begin
;print,'highs overlapping'
;erase
;!type=2^2+2^3
;contour,sf,x,alat,nlevels=20,/noeras,title=string(th(k))
;index=where(markh lt 0.)
;if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=8,color=mcolor
;index=where(markh2 lt 0.)
;if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=mcolor*.3,symsize=0.5
             if min(y2d(lindex)) gt 0. then begin
                kindex=where(markh lt 0. and y2d gt 0.,kpt)
                jindex=where(markh2 lt 0. and y2d gt 0.,jpt)
                if max(y2d(jindex)) eq max(y2d) then markh(lindex)=0.           ; keep circumpolar
                if max(y2d(jindex)) lt max(y2d) then markh2(lindex)=0.
;               if kpt gt jpt then markh2(lindex)=0.            ; keep bigger
;               if kpt lt jpt then markh(lindex)=0.
             endif
             if max(y2d(lindex)) lt 0. then begin
                kindex=where(markh lt 0. and y2d lt 0.,kpt)
                jindex=where(markh2 lt 0. and y2d lt 0.,jpt)
                if min(y2d(jindex)) eq min(y2d) then markh(lindex)=0.           ; keep circumpolar
                if min(y2d(jindex)) gt min(y2d) then markh2(lindex)=0.
;               if kpt gt jpt then markh2(lindex)=0.            ; keep bigger
;               if kpt lt jpt then markh(lindex)=0.
             endif
;print,'are the highs still overlapping ?'
;erase
;!type=2^2+2^3
;contour,sf,x,alat,nlevels=20,/noeras,title=string(th(k))
;index=where(markh lt 0.)
;if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=8,color=mcolor
;index=where(markh2 lt 0.)
;if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=mcolor*.3,symsize=0.5
;stop
          endif
;
; merge 2 anticyclone marker fields
;
         index=where(markh2 lt 0.)
         if index(0) ne -1L then markh(index)=min(markh)-1.0
;
; check for vortex and anticyclone overlap
;
        lindex=where(markl gt 0. and markh lt 0.)
        if lindex(0) ne -1 then begin
;          if min(y2d(lindex)) lt 0. and max(y2d(lindex)) gt 0. then stop
;
; NH
;
           if min(y2d(lindex)) gt 0. then begin
              s0=min(sf(lindex))
              kindex=where(markl gt 0. and y2d gt 0.)
              s1=min(sf(kindex))
              index=where(sf ge (s0+s1)/2.0 and y2d gt 0.)
              markl(index)=0.
           endif
;
; SH
;
           if max(y2d(lindex)) lt 0. then begin
              s0=max(sf(lindex))
              kindex=where(markl gt 0. and y2d lt 0.)
              s1=max(sf(kindex))
              index=where(sf le (s0+s1)/2.0 and y2d lt 0.)
              markl(index)=0.
           endif
        endif
;
; check
;
erase
        xmn=xorig(0)
        xmx=xorig(0)+xlen
        ymn=yorig(0)
        ymx=yorig(0)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        !type=2^2+2^3
        contour,sf,x,alat,nlevels=20,/noeras,xrange=[0.,360.],yrange=[-90.,90],$
                title=sdate+'  '+string(th(k))+' K',charsize=2
        map_set,0,180,0,/contin,/grid,/noeras
        index=where(markl gt 0.)
        if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=2,color=.2*mcolor
        contour,markl,x,alat,levels=[0.1],/overplot,thick=5,color=mcolor
        index=where(markh lt 0.)
        if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=.9*mcolor
        contour,markh,x,alat,levels=[-0.1],/overplot,thick=5,color=mcolor
        markl1=0.*qdf1
        markl1(0:nc-1,0:nr-1)=markl(0:nc-1,0:nr-1)
        mark2(*,*,k)=transpose(markl1)
        markh1=0.*qdf1
        markh1(0:nc-1,0:nr-1)=markh(0:nc-1,0:nr-1)
        mark2(*,*,k)=mark2(*,*,k)+transpose(markh1)
endfor	; loop over theta
;
; write marker file
;
ofile=dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.'+sdate+'.nc3'
print,'writing ',ofile
nocid = ncdf_create(ofile,/CLOBBER)
latdimid=ncdf_dimdef(nocid, 'number_of_latitudes' , nr)
londimid=ncdf_dimdef(nocid, 'number_of_longitudes', nc)
levdimid=ncdf_dimdef(nocid, 'number_of_levels'    , nth)
lonsid = ncdf_vardef(nocid, 'longitude',  londimid)
latsid = ncdf_vardef(nocid, 'latitude' ,  latdimid)
levsid = ncdf_vardef(nocid, 'theta'    ,  levdimid)
vid  = ncdf_vardef(nocid, 'IPV' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'P'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'U'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'V'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'QDF' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'Q'   , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'GPH' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'SF'  , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'MARK', [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'H2O' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'N2O' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'O3'  , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'CO2' , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'NO'  , [latdimid,londimid,levdimid])
vid  = ncdf_vardef(nocid, 'NO2' , [latdimid,londimid,levdimid])

ncdf_attput, nocid, 'longitude', 'longname', 'longitude' & ncdf_attput, nocid, 'longitude', 'units', 'deg E'
ncdf_attput, nocid, 'latitude', 'longname', 'latitude' & ncdf_attput, nocid, 'latitude', 'units', 'deg'
ncdf_attput, nocid, 'theta', 'longname', 'potential temperature' & ncdf_attput, nocid, 'theta', 'units', 'K'
ncdf_attput, nocid, 'IPV', 'longname', 'Isentropic Potential Vorticity' & ncdf_attput, nocid, 'IPV', 'units', 'K m^2 /s /kg'
ncdf_attput, nocid, 'P', 'longname', 'Pressure' & ncdf_attput, nocid, 'P', 'units', 'hPa'
ncdf_attput, nocid, 'U', 'longname', 'Zonal Wind' & ncdf_attput, nocid, 'U', 'units', 'm/s'
ncdf_attput, nocid, 'V', 'longname', 'Meridional Wind' & ncdf_attput, nocid, 'V', 'units', 'm/s'
ncdf_attput, nocid, 'QDF', 'longname', 'Strain/Rotation Parameter' & ncdf_attput, nocid, 'QDF', 'units', 's-1'
ncdf_attput, nocid, 'Q', 'longname', 'Net Diabatic Heating Rate' & ncdf_attput, nocid, 'Q', 'units', 'K/day'
ncdf_attput, nocid, 'GPH', 'longname', 'Geopotential Height' & ncdf_attput, nocid, 'GPH', 'units', 'm'
ncdf_attput, nocid, 'SF', 'longname', 'Streamfunction' & ncdf_attput, nocid, 'SF', 'units', 'm2/s'
ncdf_attput, nocid, 'MARK', 'longname', 'Vortex Marker' & ncdf_attput, nocid, 'MARK', 'units', 'negative for anticyclones: positive for polar vortices'
ncdf_attput, nocid, 'H2O', 'longname', 'Water Vapor' & ncdf_attput, nocid, 'H2O', 'units', 'mol/mol'
ncdf_attput, nocid, 'N2O', 'longname', 'Nitrous Oxide' & ncdf_attput, nocid, 'N2O', 'units', 'mol/mol'
ncdf_attput, nocid, 'O3', 'longname', 'Ozone' & ncdf_attput, nocid, 'O3', 'units', 'mol/mol'
ncdf_attput, nocid, 'CO2', 'longname', 'Carbon Dioxide' & ncdf_attput, nocid, 'CO2', 'units', 'mol/mol'
ncdf_attput, nocid, 'NO', 'longname', 'Nitric Oxide' & ncdf_attput, nocid, 'NO', 'units', 'mol/mol'
ncdf_attput, nocid, 'NO2', 'longname', 'Nitrogen Dioxide' & ncdf_attput, nocid, 'NO2', 'units', 'mol/mol'

ncdf_control,nocid,/ENDEF
ncdf_varput, nocid, 'longitude', alon  , COUNT=[nc]
ncdf_varput, nocid, 'latitude' , alat  , COUNT=[nr]
ncdf_varput, nocid, 'theta'    , th    , COUNT=[nth]
ncdf_varput, nocid, 'IPV' , pv2      , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'P'   , p2       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'U'   , u2       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'V'   , v2       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'QDF' , qdf2     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'Q'   , q2       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'GPH' , g2       , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'SF'  , sf2      , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'MARK', mark2    , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'H2O' , h2o2     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'N2O' , n2o2     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'O3'  , o32      , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'CO2' , co22     , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'NO'  , no2      , COUNT=[nr,nc,nth]
ncdf_varput, nocid, 'NO2' , no22     , COUNT=[nr,nc,nth]
ncdf_close,nocid
;spawn,'rm -f '+ifile

jumpstep:
endfor	; loop over files
end
