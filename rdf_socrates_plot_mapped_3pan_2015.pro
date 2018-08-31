;
; 2015 SDWACCM version
; add a panel that shows July ozone at the measurement locations
;
; plot mapped RDF and FDF map NOGAPS ozone at SOCRATES
; to a daily grid.  superimpose coincident occultation
; locations colored by instrument. plot mapped arrays
; note: need time as Julian hour
; VLH 9/10/15
;
@fillit
@smoothit
@rd_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
icmm1=mcolor-1B
icmm2=mcolor-2B
!noeras=1
setplot='ps'
read,'enter setplot',setplot
nxdim=750
nydim=750
xorig=[0.15,0.15,0.15]
yorig=[0.75,0.475,0.2]
xlen=0.7
ylen=0.2
cbaryoff=0.09
cbarydel=0.01
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
;diru='/aura7/harvey/NOGAPS_Alpha/Datfiles/'
diru='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
thlab='360K'
theta=360.
smooth_field='n'
fill_field='n'
;
; restore mapped output
;
; save,filename='rdf_socrates_'+dates(0)+'_'+sxc+stc+'.sav',imax,jmax,nthp,$
;     xnd,ynd,thlevp,x0map,y0map,th0map,xmrk0map,pv0map,xmap,ymap,thmap,agemap,$
;     pvmap,instmap,xinstmap,yinstmap,qdfmap,fdmap,xmintmap,xmrkmap,o3map,$
;     xo3map,yo3map,xh2omap,yh2omap,h2omap
;
;sdate='20080707'
sdate='20130716'
restore,'rdf_socrates_'+sdate+'_250KMx12h_2weeks.sav
;
; extract desired theta surfaces
;
;index=where(thlevp eq theta)
;AGEMAP=reform(agemap(*,*,index(0)))	;          FLOAT     = Array[180, 91, 4]
;FDMAP=reform(FDMAP(*,*,index(0)))	;           FLOAT     = Array[180, 91, 4]
;H2OMAP=reform(H2OMAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]
;INSTMAP=reform(INSTMAP(*,*,index(0)))       ;         FLOAT     = Array[180, 91, 4]
;O3MAP=reform(O3MAP(*,*,index(0)))       ;           FLOAT     = Array[180, 91, 4]
;PV0MAP=reform(PV0MAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]
;PVMAP=reform(PVMAP(*,*,index(0)))       ;           FLOAT     = Array[180, 91, 4]
;QDFMAP=reform(QDFMAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]
;TH0MAP=reform(TH0MAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]
;THMAP=reform(THMAP(*,*,index(0)))       ;           FLOAT     = Array[180, 91, 4]
;X0MAP=reform(X0MAP(*,*,index(0)))       ;           FLOAT     = Array[180, 91, 4]
;XH2OMAP=reform(XH2OMAP(*,*,index(0)))       ;         FLOAT     = Array[180, 91, 4]
;XINSTMAP=reform(XINSTMAP(*,*,index(0)))       ;        FLOAT     = Array[180, 91, 4]
;XMAP=reform(XMAP(*,*,index(0)))       ;            FLOAT     = Array[180, 91, 4]
;XMINTMAP=reform(XMINTMAP(*,*,index(0)))       ;        FLOAT     = Array[180, 91, 4]
;XMRK0MAP=reform(XMRK0MAP(*,*,index(0)))       ;        FLOAT     = Array[180, 91, 4]
;XMRKMAP=reform(XMRKMAP(*,*,index(0)))       ;         FLOAT     = Array[180, 91, 4]
;XO3MAP=reform(XO3MAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]
;Y0MAP=reform(Y0MAP(*,*,index(0)))       ;           FLOAT     = Array[180, 91, 4]
;YH2OMAP=reform(YH2OMAP(*,*,index(0)))       ;         FLOAT     = Array[180, 91, 4]
;YINSTMAP=reform(YINSTMAP(*,*,index(0)))       ;        FLOAT     = Array[180, 91, 4]
;YMAP=reform(YMAP(*,*,index(0)))       ;            FLOAT     = Array[180, 91, 4]
;YO3MAP=reform(YO3MAP(*,*,index(0)))       ;          FLOAT     = Array[180, 91, 4]

index=where(pvmap ne 9999.)
if index(0) ne -1L then pvmap(index)=pvmap(index)*1.e6
index=where(pv0map ne 9999.)
if index(0) ne -1L then pv0map(index)=pv0map(index)*1.e6
;
; filter out polar satellite and 2 others
;
index=where(instmap ge 9.)
if index(0) ne -1L then o3map(index)=9999.
if index(0) ne -1L then h2omap(index)=9999.

    if setplot eq 'ps' then begin
       lc=0
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='rdf_socrates_'+sdate+'_3pan+xys.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif
;
; fill
;
    if fill_field eq 'y' then begin
       fillit,o3map,o3mapfill
       fillit,pvmap,pvmapfill
       fillit,h2omap,h2omapfill
       o3map=o3mapfill
       pvmap=pvmapfill
       h2omap=h2omapfill
    endif
;
; smooth
;
    if smooth_field eq 'y' then begin
       smoothit,o3map,o3mapsmooth
       smoothit,pvmap,pvmapsmooth
       smoothit,h2omap,h2omapsmooth
       o3map=o3mapsmooth
       pvmap=pvmapsmooth
       h2omap=h2omapsmooth
    endif
;
; add wrap around point for plotting
;
    o3map2=fltarr(imax+1,jmax)
    pvmap2=fltarr(imax+1,jmax)
    pv0map2=fltarr(imax+1,jmax)
    h2omap2=fltarr(imax+1,jmax)
    o3map2(0:imax-1,0:jmax-1)=o3map(0:imax-1,0:jmax-1)
    pvmap2(0:imax-1,0:jmax-1)=pvmap(0:imax-1,0:jmax-1)
    pv0map2(0:imax-1,0:jmax-1)=pv0map(0:imax-1,0:jmax-1)
    h2omap2(0:imax-1,0:jmax-1)=h2omap(0:imax-1,0:jmax-1)
    o3map2(imax,*)=o3map2(0,*)
    pvmap2(imax,*)=pvmap2(0,*)
    pv0map2(imax,*)=pv0map2(0,*)
    h2omap2(imax,*)=h2omap2(0,*)
    xnd2=fltarr(imax+1)
    xnd2(0:imax-1)=xnd(0:imax-1)
    xnd2(imax)=xnd2(0)+360.
;
; read NOGAPS data
;
    ifile=diru+'sdwaccm2012-2014_1_2_2.cam.h1.'+sdate+'_utls.nc3'
;   rd_nogaps_nc3,ifile,nc,nr,inth,alon,alat,th,pv2,prs2,msf2,u2,v2,q2,$
;                 qdf2,mark2,sf2,vp2,o32,h2o2,iflag
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
; interpolate PV to mapped grid
;
ncnew=n_elements(alon)
nrnew=n_elements(alat)
nc=n_elements(xnd)
nr=n_elements(ynd)
lonnew=alon
latnew=alat
lonold=xnd
latold=ynd
;
; interpolate old grid to new resolution (latold->latnew)
;
olddata=o3map
newdata=fltarr(ncnew,nrnew)

for ii=0L,ncnew-1L do begin
    xp=lonnew(ii)
    for jj=0L,nrnew-1L do begin
        yp=latnew(jj)

        for i=0L,nc-1L do begin
            ip1=i+1
            if i eq nc-1L then ip1=0
            xlon=lonold(i)
            xlonp1=lonold(ip1)
            if i eq nc-1L then xlonp1=360.+lonold(ip1)

              if xp ge xlon and xp le xlonp1 then begin
                 xscale=(xp-xlon)/(xlonp1-xlon)
                 for j=0L,nr-2L do begin
                     jp1=j+1
                     xlat=latold(j)
                     xlatp1=latold(jp1)
                     if yp ge xlat and yp le xlatp1 then begin
                        yscale=(yp-xlat)/(xlatp1-xlat)
                        tj1=olddata(i,j)+xscale*(olddata(ip1,j)-olddata(i,j))
                        tjp1=olddata(i,jp1)+xscale*(olddata(ip1,jp1)-olddata(i,jp1))
                        newdata(ii,jj)=tj1+yscale*(tjp1-tj1)
                     endif
                 endfor
              endif
          endfor

   endfor       ; loop over new latitudes
endfor          ; loop over new longitudes
o3interp=fltarr(ncnew+1,nrnew)
o3interp(0:ncnew-1,0:nrnew-1)=newdata(0:ncnew-1,0:nrnew-1)
o3interp(ncnew,*)=o3interp(0,*)
;
; plot
;
; SDWACCM ozone
;
    erase
;   xyouts,.425,.95,sdate,/normal,charsize=1.5,color=0,charthick=3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    nlvls=7
    col1=1+indgen(nlvls+1)*icolmax/nlvls
    col1(2)=100
    col1(3)=150
    col1(4)=190
    col1(5)=200
    col1(6)=215
    col1(7)=254
    imin=0.0
    imax=0.7
    iint=(imax-imin)/float(nlvls)
    level=imin+iint*findgen(nlvls+1)
nlvls=n_elements(level)
index=where(lat gt -90.)
sflevel=min(sf(index))+((max(sf(index))-min(sf(index)))/float(nlvls))*findgen(nlvls)
;    map_set,0,-180,0,/noeras,color=0,title='SDWACCM Ozone',limit=[-90,-180,90,180]
    contour,o3,x,alat,/noeras,/cell_fill,c_color=col1,levels=level,color=0,yrange=[-90,90],xrange=[0,360],xticks=4,charsize=1.25,charthick=2,$
            ytitle='Latitude',title='SD-WACCM on '+sdate,yticks=6
    map_set,0,180,0,/contin,/grid,/noeras,color=mcolor,limit=[-90,0,90,360]
;    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4.],thick=10,c_labels=0
;   contour,o3,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[0.2],thick=8,c_labels=0
;    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=4,c_label=0*sflevel
;    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-90,-180,90,180]
;
; Mapped Ozone
; 
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    index=where(o3map2 eq 9999)
    if index(0) ne -1 then o3map2(index)=0./0.
;o3map2=smooth(o3map2,3,/Nan,/edge_truncate)
;   MAP_SET,0,-180,0,/GRID,/CONTIN,/noeras,color=0,title='Trajectory Mapped',limit=[-90,-180,90,180]
    contour,o3map2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,color=0,yrange=[-90,90],xrange=[0,360],xticks=4,charsize=1.25,charthick=2,$
            ytitle='Latitude',title='Trajectory Mapped',yticks=6
    map_set,0,180,0,/contin,/grid,/noeras,color=0,limit=[-90,0,90,360]
;   contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4.],thick=10,c_labels=0
;    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=4,c_label=0*sflevel
;   map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-90,-180,90,180]
    for i=0,5 do begin
        index=where(instmap eq (i+1))
;        oplot,xo3map(index),yo3map(index),psym=8,color=0	;(float(i+1)/11.)*mcolor
    endfor
;
; insitu ozone
;
    restore,'/Volumes/cloud/data/WACCM_data/Datfiles_SOSST/dmps_socrates.sdwaccm.2023
;
; strip out 360 K
;
    index=where(th eq 360.)
    o3lev=reform(O3_PROF(*,index(0)))
;
; strip out July 
;
    index=where(date ge 20130709L and date le 20130722L,npts)
    LATITUDE=LATITUDE(index)
    LONGITUDE=LONGITUDE(index)
    o3lev=o3lev(index)
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    contour,o3map2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,color=0,yrange=[-90,90],xrange=[0,360],xticks=4,charsize=1.25,charthick=2,$
            ytitle='Latitude',xtitle='Longitude',title='2 Weeks of Binned SOCRATES',/nodata,yticks=6
    map_set,0,180,0,/contin,/grid,/noeras,limit=[-90,0,90,360],color=0
    index=where(o3lev le level(1))
    if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(0),psym=8,symsize=0.6
    for ii=1L,nlvls-2L do begin
    index=where(o3lev gt level(ii) and o3lev le level(ii+1))
    if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(ii),psym=8,symsize=0.6
    endfor
    index=where(o3lev gt level(-1))
    if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(-1),psym=8,symsize=0.6
stop

;   for i=0L,npts-1L do begin
;       oplot,[LONGITUDE(i),LONGITUDE(i)],[LATITUDE(i),LATITUDE(i)],color=((o3lev(i)-imin)/(imax-imin))*mcolor,psym=8,symsize=0.8
;   endfor
;    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=0,thick=3,c_label=0*sflevel
;
; bin to WACCM longitude/latitude grid
;
o3binned=0.*o31
no3binned=0.*o31
x2d=0.*o31
y2d=0.*o31
for i=0,ncnew-1 do y2d(i,*)=latnew
for j=0,nrnew-1 do x2d(*,j)=lonnew
dxx=lonnew(1)-lonnew(0)
dyy=latnew(1)-latnew(0)
for ii=0L,npts-1L do begin
xp=longitude(ii)
if xp gt lonnew(-1)+dxx/2. then xp=xp-360.
yp=latitude(ii)
for i=0L,ncnew-1L do begin
    xlon=lonnew(i)-dxx/2.
    xlonp1=lonnew(i)+dxx/2.
    if xlon le xp and xlonp1 gt xp then begin
    for j=0L,nrnew-1L do begin
        xlat=latnew(j)-dyy/2.
        xlatp1=latnew(j)+dyy/2.
        if xlat le yp and xlatp1 gt yp then begin
           o3binned(i,j)=o3binned(i,j)+o3lev(ii)
           no3binned(i,j)=no3binned(i,j)+1.
        endif
    endfor       ; loop over new latitudes
    endif
endfor          ; loop over new longitudes
endfor		; loop over points
index=where(no3binned gt 1.)
if index(0) ne -1L then o3binned(index)=o3binned(index)/no3binned(index)
index=where(o3binned eq 0.)
if index(0) ne -1L then o3binned(index)=9999.
    if fill_field eq 'y' then begin
       fillit,o3binned,o3binnedfill
       o3binned=o3binnedfill
    endif
    if smooth_field eq 'y' then begin
       smoothit,o3binned,o3binnedsmooth
       o3binned=o3binnedsmooth
    endif
index=where(o3binned eq 9999.)
if index(0) ne -1L then o3binned(index)=0./0.
contour,o3binned,lonnew,latnew,/noeras,/cell_fill,c_color=col1,levels=level,color=0,/overplot
a=findgen(5)*(2*!pi/5.)
usersym,cos(a),sin(a),/fill
index=where(finite(o3binned) eq 1)
o3lev=o3binned(index)
longitude=x2d(index)
latitude=y2d(index)
index=where(o3lev le level(1))
if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(0),psym=8,symsize=0.8
for ii=1L,nlvls-2L do begin
    index=where(o3lev gt level(ii) and o3lev le level(ii+1))
    if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(ii),psym=8,symsize=0.8
endfor
index=where(o3lev gt level(-1))
if index(0) ne -1L then oplot,LONGITUDE(index),LATITUDE(index),color=col1(-1),psym=8,symsize=0.8

    map_set,0,180,0,/contin,/grid,/noeras,limit=[-90,0,90,360],color=0
;
; color bar
;
    ymnb=yorig(2) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle=thlab+' Ozone (ppmv)',charsize=1.25,charthick=2,xticks=nlvls-1,xtickv=level
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
;
;loadct,0
;    map_grid,label=1,lats=[-90.,0.,30.,60.,90.],lons=[0.,90.,180.,270.],latlab=359.,$
;        lonlab=-19.,color=200,charsize=3,charthick=2,lonnames=['GM','90E','DL','90W'],$
;        latnames=['-20      ','EQ      ','30      ','60      ','90      ']
;loadct,39

    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim rdf_socrates_'+sdate+'_3pan+xys.ps -rotate -90 rdf_socrates_'+sdate+'_3pan+xys.jpg'
;      spawn,'/usr/bin/rm rdf_socrates_'+sdate+'_3pan+xys.ps'
    endif
end
