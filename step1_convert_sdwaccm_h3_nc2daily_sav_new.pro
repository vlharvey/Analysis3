;
; read SDWACCM h3 netcdf data files from Doug Kinnison
; output daily .sav files
;
; /Volumes/cloud/data/WACCM_data/Datfiles_SD/f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h3.2014-07-30-00000.nc
;
device,decompose=0
loadct,39
mcolor=255
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
nxdim=800
nydim=800
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
!type=2^2+2^3
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
spawn,'ls '+dir+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h3.20*-00000.nc',ifiles
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
        if result.name eq 'lat' then lat=data
        if result.name eq 'lon' then lon=data
        if result.name eq 'lev' then lev=data
        if result.name eq 'time' then time=data
        if result.name eq 'date' then date=data
        if result.name eq 'PS' then ps3d=data           ; Pa
        if result.name eq 'BUTGWSPEC' then BUTGWSPEC4d=data
        if result.name eq 'UTGWORO' then UTGWORO4d=data
        if result.name eq 'UTGWSPEC' then UTGWSPEC4d=data
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
        UTGWSPEC=reform(UTGWSPEC4d(*,*,*,iday))
        UTGWORO=reform(UTGWORO4d(*,*,*,iday))
        BUTGWSPEC=reform(BUTGWSPEC4d(*,*,*,iday))
        P=reform(p4d(*,*,*,iday))
        T=reform(t4d(*,*,*,iday))
        U=reform(u4d(*,*,*,iday))
        V=reform(v4d(*,*,*,iday))
        Z=reform(z4d(*,*,*,iday))
;
; save daily file
;
        ofile=dir+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h3.'+today+'.sav'
        print,ofile
        save,file=ofile,LAT,LON,LEV,P,T,U,V,Z,UTGWSPEC,UTGWORO,BUTGWSPEC
;
; quick check
;
; save postscript version
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='Figures/'+today+'_sdwaccm_ubar+gw.ps'
       device,/color
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
       !p.thick=2.0                   ;Plotted lines twice as thick
       !p.charsize=2.0
    endif
erase
nlvls=20
col1=1.+(findgen(nlvls)/float(nlvls))*mcolor
level=-100+10.*findgen(nlvls)
dum=mean(u,dim=1)
zdum=mean(z,dim=1)/1000.
contour,dum,lat,zdum,yrange=[0,100],levels=level,c_color=col1,/fill,title=today,ytitle='Altitude (km)',charsize=2,charthick=2,xtitle='Latitude',/noeras,color=0
contour,dum,lat,zdum,levels=10+10*findgen(10),/overplot,color=0,/noeras,thick=3
contour,dum,lat,zdum,levels=-100+10*findgen(10),/overplot,color=mcolor,c_linestyle=5,/noeras,thick=3
dum=mean(t,dim=1)                                                                                  
contour,dum,lat,zdum,levels=100+10*findgen(5),/overplot,color=10,thick=5,/noeras
utgw=(UTGWSPEC+UTGWORO)*86400.
dum=mean(utgw,dim=1)
contour,dum,lat,zdum,levels=10+20*findgen(5),/overplot,color=250,thick=5,/noeras
contour,dum,lat,zdum,levels=-100+20*findgen(5),/overplot,color=50,thick=5,/noeras

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim Figures/'+today+'_sdwaccm_ubar+gw.ps -rotate -90 Figures/'+today+'_sdwaccm_ubar+gw.jpg'
       spawn,'rm -f Figures/'+today+'_sdwaccm_ubar+gw.ps'
    endif

    endfor		; loop over days
jumpnc:
    ncdf_close,ncid
endfor			; loop over 10-day files
end
