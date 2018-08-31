;
; 2015 SDWACCM version
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
usersym,cos(a),sin(a)
icmm1=mcolor-1B
icmm2=mcolor-2B
!noeras=1
setplot='ps'
read,'enter setplot',setplot
nxdim=750
nydim=750
xorig=[0.1,0.55,0.1,0.55]
yorig=[0.6,0.6,0.15,0.15]
xlen=0.4
ylen=0.3
cbaryoff=0.08
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
smooth_field='y'
fill_field='y'
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
restore,'rdf_socrates_'+sdate+'_250KMx12h.sav
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
              /bold,/color,bits_per_pixel=8,/helvetica,filename='rdf_socrates_'+sdate+'_4pan+xys.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
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
    h2omap2=fltarr(imax+1,jmax)
    o3map2(0:imax-1,0:jmax-1)=o3map(0:imax-1,0:jmax-1)
    pvmap2(0:imax-1,0:jmax-1)=pvmap(0:imax-1,0:jmax-1)
    h2omap2(0:imax-1,0:jmax-1)=h2omap(0:imax-1,0:jmax-1)
    o3map2(imax,*)=o3map2(0,*)
    pvmap2(imax,*)=pvmap2(0,*)
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
; plot
;
; SDWACCM ozone
;
    erase
    xyouts,.4,.95,sdate+' '+thlab,/normal,charsize=2,color=0,charthick=3
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
index=where(lat gt -20.)
sflevel=min(sf(index))+((max(sf(index))-min(sf(index)))/float(nlvls))*findgen(nlvls)
;    map_set,0,-180,0,/noeras,color=0,title='SDWACCM Ozone',limit=[-20,-180,90,180]
    contour,o3,x,alat,/noeras,/cell_fill,c_color=col1,levels=level,color=0,yrange=[-20,90],xrange=[0,360],xticks=4,charsize=1.5,charthick=2,$
            ytitle='Latitude',xtitle='Longitude',title='SD-WACCM Ozone'
    map_set,0,180,0,/contin,/grid,/noeras,color=mcolor,limit=[-20,0,90,360]
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=8,c_labels=0
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=2,c_label=0*sflevel
;    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]
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
;   MAP_SET,0,-180,0,/GRID,/CONTIN,/noeras,color=0,title='Trajectory Mapped Ozone',limit=[-20,-180,90,180]
    contour,o3map2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,color=0,yrange=[-20,90],xrange=[0,360],xticks=4,charsize=1.5,charthick=2,$
            xtitle='Longitude',title='Trajectory Mapped Ozone'
    map_set,0,180,0,/contin,/grid,/noeras,color=mcolor,limit=[-20,0,90,360]
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=8,c_labels=0
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=2,c_label=0*sflevel
;   map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]
for i=0,5 do begin
    index=where(instmap eq (i+1))
;    oplot,xo3map(index),yo3map(index),psym=8,color=0	;(float(i+1)/11.)*mcolor
endfor
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='(ppmv)',charsize=1.5,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
;
; SDWACCM Water Vapor
;
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    nlvls=11
    col1=1+indgen(nlvls)*icolmax/nlvls
    imin=0.0
    imax=100.
    iint=(imax-imin)/float(nlvls)
    level=imin+iint*findgen(nlvls)
;   map_set,0,-180,0,/noeras,color=0,title='SDWACCM Water',limit=[-20,-180,90,180]
    contour,h2o,x,alat,/noeras,/cell_fill,c_color=col1,levels=level,color=0,yrange=[-20,90],xrange=[0,360],xticks=4,charsize=1.5,charthick=2,$
            xtitle='Longitude',title='SD-WACCM H!l2!nO'
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=2,c_label=0*sflevel
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=8,c_labels=0
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,0,90,360]
;loadct,0
;    map_grid,label=1,lats=[-20.,0.,30.,60.,90.],lons=[0.,90.,180.,270.],latlab=359.,$
;        lonlab=-19.,color=200,charsize=3,charthick=2,lonnames=['GM','90E','DL','90W'],$
;        latnames=['-20      ','EQ      ','30      ','60      ','90      ']
;loadct,39
;
; Mapped Water Vapor
;
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    index=where(h2omap2 eq 9999)
    if index(0) ne -1 then h2omap2(index)=0./0.
;    MAP_SET,0,-180,0,/GRID,/CONTIN,/noeras,color=0,title='Trajectory Mapped Water',limit=[-20,-180,90,180]
;for i=0,imax-1 do y0map(i,*)=ynd
;for j=0,jmax-1 do x0map(*,j)=xnd
    contour,h2omap2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,color=0,yrange=[-20,90],xrange=[0,360],xticks=4,charsize=1.5,charthick=2,$
            xtitle='Longitude',title='Trajectory Mapped H!l2!nO'
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=2,c_label=0*sflevel
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=8,c_labels=0
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,0,90,360]

    ymnb=yorig(3) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='(ppmv)',charsize=1.5,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor

    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim rdf_socrates_'+sdate+'_4pan+xys.ps -rotate -90 rdf_socrates_'+sdate+'_4pan+xys.jpg'
;      spawn,'/usr/bin/rm rdf_socrates_'+sdate+'_4pan+xys.ps'
    endif
end
