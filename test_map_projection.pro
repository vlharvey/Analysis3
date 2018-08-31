;
; test map projections
;
@rd_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a)
icmm1=mcolor-1B
icmm2=mcolor-2B
!noeras=1
setplot='ps'
read,'enter setplot',setplot
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.2]
xlen=0.7
ylen=0.5
cbaryoff=0.09
cbarydel=0.01
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
diru='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
thlab='360K'
sdate='20130716'
    ifile=diru+'sdwaccm2012-2014_1_2_2.cam.h1.'+sdate+'_utls.nc3'
    rd_sdwaccm4_nc3,ifile,nc,nr,nth,alon,alat,th,$
         pv2,prs2,z2,u2,v2,q2,qdf2,mark2,sf2,h2o2,n2o2,o32,iflag

    theta=360.
    index=where(theta eq th)
    thlev=index(0)
    stheta=strcompress(string(fix(theta)),/remove_all)
    qdf1=transpose(qdf2(*,*,thlev))
    sf1=transpose(sf2(*,*,thlev))
    pv1=transpose(pv2(*,*,thlev))
    o31=transpose(o32(*,*,thlev))*1.e6
    h2o1=transpose(h2o2(*,*,thlev))*1.e6
    mark1=transpose(mark2(*,*,thlev))
    qdf=0.*fltarr(nc+1,nr)
    qdf(0:nc-1,0:nr-1)=qdf1(0:nc-1,0:nr-1)
    qdf(nc,*)=qdf(0,*)
    sf=0.*fltarr(nc+1,nr)
    sf(0:nc-1,0:nr-1)=sf1(0:nc-1,0:nr-1)
    sf(nc,*)=sf(0,*)
    pv=0.*fltarr(nc+1,nr)
    pv(0:nc-1,0:nr-1)=pv1(0:nc-1,0:nr-1)*1.e6	; PVU
    pv(nc,*)=pv(0,*)
    mark=0.*fltarr(nc+1,nr)
    mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
    mark(nc,*)=mark(0,*)
    o3=0.*fltarr(nc+1,nr)
    o3(0:nc-1,0:nr-1)=o31(0:nc-1,0:nr-1)
    o3(nc,*)=o3(0,*)
    h2o=0.*fltarr(nc+1,nr)
    h2o(0:nc-1,0:nr-1)=h2o1(0:nc-1,0:nr-1)
    h2o(nc,*)=h2o(0,*)

    x=fltarr(nc+1)
    x(0:nc-1)=alon
    x(nc)=alon(0)+360.
    lon=0.*sf
    lat=0.*sf
    for i=0,nc   do lat(i,*)=alat
    for j=0,nr-1 do lon(*,j)=x
;
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nlvls=11
col1=1+indgen(nlvls)*icolmax/nlvls
imin=0.0
imax=0.5
iint=(imax-imin)/float(nlvls)
level=imin+iint*findgen(nlvls)
index=where(lat gt -90.)
sflevel=min(sf(index))+((max(sf(index))-min(sf(index)))/float(nlvls-1))*findgen(nlvls-1)
pv(*,-1)=0./0.
for i=0,10 do begin
    erase
    if i eq 0L then map_set,/AITOFF,0,0,0,/contin,/grid,color=0,title='AITOFF'
    if i eq 0L then map_set,/ALBERS,0,0,0,/contin,/grid,color=0,title='Albers'

MAP_SET [, P0lat, P0lon, Rot]

Keywords-Projection Types: [ [, /AITOFF | , /ALBERS | , /AZIMUTHAL | , /CONIC | , /CYLINDRICAL | , /GNOMIC | , /GOODESHOMOLOSINE | , /HAMMER | , /LAMBERT | , /MERCATOR | , /MILLER_CYLINDRICAL | , /MOLLWEIDE | , /ORTHOGRAPHIC | , /ROBINSON | , /SATELLITE | , /SINUSOIDAL | , /STEREOGRAPHIC | , /TRANSVERSE_MERCATOR ] | [, NAME=string] ]

Keywords-Map Characteristics: [, /ADVANCE] [, CHARSIZE=value] [, /CLIP] [, COLOR=index] [, /CONTINENTS [, CON_COLOR=index] [, /HIRES]] [, E_CONTINENTS=structure] [, E_GRID=structure] [, E_HORIZON=structure] [, GLINESTYLE={0 | 1 | 2 | 3 | 4 | 5}] [, GLINETHICK=value] [, /GRID] [, /HORIZON] [, LABEL=n{label every nth gridline}] [, LATALIGN=value{0.0 to 1.0}] [, LATDEL=degrees] [, LATLAB=longitude] [, LONALIGN=value{0.0 to 1.0}] [, LONDEL=degrees] [, LONLAB=latitude] [, MLINESTYLE={0 | 1 | 2 | 3 | 4 | 5}] [, MLINETHICK=value] [, /NOBORDER] [, /NOERASE] [, REVERSE={0 | 1 | 2 | 3}] [, TITLE=string] [, /USA] [, XMARGIN=value] [, YMARGIN=value]

Keywords-Projection Parameters: [, CENTRAL_AZIMUTH=degrees_east_of_north] [, ELLIPSOID=array] [, /ISOTROPIC] [, LIMIT=vector] [, SAT_P=vector] [, SCALE=value] [, STANDARD_PARALLELS=array]

Graphics Keywords: [, POSITION=[X0, Y0, X1, Y1]] [, /T3D] [, ZVALUE=value{0 to 1}]
map_set,/MOLLWEIDE,0,120,/noeras,color=0,title='SD-WACCM Ozone'    ; on '+sdate
contour,o3,x,alat,/noeras,/cell_fill,c_color=col1,levels=level,color=0,charsize=1.25,charthick=2,/overplot
map_set,/MOLLEWEIDE,0,120,/contin,/grid,/noeras,color=0
contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4.],thick=10,c_labels=0
contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=5,c_label=0*sflevel

end
