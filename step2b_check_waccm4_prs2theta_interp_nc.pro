;
; check isentropic data
; Input data:  IDL>restore,'/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2.cam2.h3.dyns.20421231_3D_dyn.sav
; 
@compvort

loadct,39
device,decompose=0
mcolor=byte(!p.color)
nlvls=30L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
dirw='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
ifiles=file_search(dirw+'mee00fpl_FW2.cam2.h3.dyns.*_3D_dyn.nc',count=nfile)
;
; loop over files
;
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    result2=strsplit(result(4),'_',/extract)
    sdate=result2(0)
    print,sdate
;
; read daily file
;
    ncfile0=ifiles(ifiles(n))
    print,ncfile0
    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'number_of_latitudes' then nc=dim
        if name eq 'number_of_longitudes' then nr=dim
        if name eq 'number_of_levels' then nl=dim
        print,'read ',name,' dimension ',dim
    endfor
    for ivar=0,result0.nvars-1 do begin
        result=ncdf_varinq(ncid,ivar)
        ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
        if result.name eq 'latitude' then alat=data
        if result.name eq 'longitude' then alon=data
        if result.name eq 'theta' then theta=data
        if result.name eq 'IPV' then pvgrd=data
        if result.name eq 'P' then pgrd=data
        if result.name eq 'U' then ugrd=data
        if result.name eq 'V' then vgrd=data
        if result.name eq 'QDF' then qdfgrd=data
        if result.name eq 'Q' then qgrd=data
        if result.name eq 'GPH' then zgrd=data
        if result.name eq 'TTGW' then ttgwgrd=data
jumpvar:
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; check
; 
rlev=2000.
print,theta
read,'Enter theta surface ',rlev
index=where(theta eq rlev)
ilev=index(0)
slev=string(rlev)
pp2=transpose(ugrd(*,*,ilev))
pp=transpose(zgrd(*,*,ilev))
level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
level2=min(pp2)+((max(pp2)-min(pp2))/float(nlvls))*findgen(nlvls)
!type=2^2+2^3
erase
contour,pp,alon,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],$
        yrange=[-90.,90.],title=sdate+'  '+slev+' K'
contour,pp,alon,alat,levels=level,/follow,c_color=0,/noeras,/overplot
contour,pp2,alon,alat,levels=level2,/follow,c_color=mcolor,/noeras,/overplot,thick=3
contour,pp,alon,alat,levels=[0.],/follow,c_color=0,thick=3,/noeras,/overplot
stop
ENDFOR		; LOOP OVER TIMESTEPS
end
