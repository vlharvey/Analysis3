;
; plot mapped RDF and FDF map NOGAPS ozone at SOCRATES
; to a daily grid.  superimpose coincident occultation
; locations colored by instrument. plot mapped arrays
; note: need time as Julian hour
; VLH 8/16/11
;
@fillit
@smoothit
@rd_nogaps_nc3

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
xorig=[0.1,0.55,0.1,0.55]
yorig=[0.6,0.6,0.15,0.15]
xlen=0.4
ylen=0.3
cbaryoff=0.03
cbarydel=0.02
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
diru='/aura7/harvey/NOGAPS_Alpha/Datfiles/'
thlab='350K'
theta=350.
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
;sdate='20080701'
;sdate='20080706'
sdate='20080707'
;sdate='20080708'
restore,'rdf_socrates_'+sdate+'_250KMx12h.sav
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
    xnd2(imax)=xnd2(0)
;
; read NOGAPS data
;
    ifile=diru+'NOGAPSA_'+sdate+'12_MetO_sabmls_aim9c.nc3'
    rd_nogaps_nc3,ifile,nc,nr,inth,alon,alat,th,pv2,prs2,msf2,u2,v2,q2,$
                  qdf2,mark2,sf2,vp2,o32,h2o2,iflag
    theta=350.
    index=where(theta eq th)
    thlev=index(0)
    stheta=strcompress(string(fix(theta)),/remove_all)
    qdf1=transpose(qdf2(*,*,thlev))
    sf1=transpose(sf2(*,*,thlev))
    pv1=transpose(pv2(*,*,thlev))
    o31=transpose(o32(*,*,thlev))
    h2o1=transpose(h2o2(*,*,thlev))
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
; NOGAPS Ozone
;
    erase
    xyouts,.4,.95,sdate+' '+thlab,/normal,charsize=2,color=0
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !type=2^2+2^3
    nlvls=11
    col1=1+indgen(nlvls)*icolmax/nlvls
    imin=0.0
    imax=0.6
    iint=(imax-imin)/float(nlvls)
    level=imin+iint*findgen(nlvls)
    map_set,0,-180,0,/noeras,color=0,title='NOGAPS Ozone',limit=[-20,-180,90,180]
    contour,o3,x,alat,/noeras,/fill,c_color=col1,levels=level,/overplot
;   contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[-4.,4.],thick=8
;   contour,sf,x,alat,nlevel=30,/overplot,/follow,color=mcolor,thick=3
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]
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
    MAP_SET,0,-180,0,/GRID,/CONTIN,/noeras,color=0,title='Trajectory Mapped Ozone',limit=[-20,-180,90,180]
    contour,o3map2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,/overplot
;   contour,sf,x,alat,nlevel=30,/overplot,/follow,color=mcolor,thick=3
;   contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[-4.,4.],thick=8
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]
;for i=0,9 do begin
;    index=where(instmap eq (i+1))
;    oplot,xo3map(index),yo3map(index),psym=8,color=(float(i+1)/11.)*mcolor
;endfor
;stop

    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='(ppmv)'
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
;
; NOGAPS Water Vapor
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
    imax=50.0
    iint=(imax-imin)/float(nlvls)
    level=imin+iint*findgen(nlvls)
    map_set,0,-180,0,/noeras,color=0,title='NOGAPS Water',limit=[-20,-180,90,180]
    contour,h2o,x,alat,/noeras,/fill,c_color=col1,levels=level,/overplot
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[-4,4],thick=8
    contour,sf,x,alat,nlevel=30,/overplot,/follow,color=mcolor,thick=3
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]
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
    MAP_SET,0,-180,0,/GRID,/CONTIN,/noeras,color=0,title='Trajectory Mapped Water',limit=[-20,-180,90,180]
;for i=0,imax-1 do y0map(i,*)=ynd
;for j=0,jmax-1 do x0map(*,j)=xnd
    contour,h2omap2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,/overplot
    contour,sf,x,alat,nlevel=30,/overplot,/follow,color=mcolor,thick=3
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[-4,4],thick=8
    map_set,0,-180,0,/noeras,color=mcolor,/contin,/grid,limit=[-20,-180,90,180]

    ymnb=yorig(3) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xorig(0),xorig(1)+xlen,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='(ppmv)'
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
