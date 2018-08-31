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
;nxdim=470
;nydim=130

xorig=[0.0125,0.5125]
yorig=[0.4,0.4]
xlen=0.475
ylen=0.3
cbaryoff=0.00
cbarydel=0.01
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
diru='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
thlab='360K'
theta=360.
smooth_field='y'
fill_field='y'
;
; restore mapped output
;
sdate='20130716'
isuff=['2weeks']
for nn=0L,n_elements(isuff)-1L do begin
restore,'rdf_socrates_'+sdate+'_250KMx12h_'+isuff(nn)+'.sav'
;
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
       device,/portrait,/encapsulated,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='rdf_socrates_'+sdate+'_2pan+xys_'+isuff(nn)+'_final.eps'
       !p.charsize=1.2
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
; read SD-WACCM data
;
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
    nlvls=11
    col1=1+indgen(nlvls)*icolmax/nlvls
    imin=0.0
    imax=0.5
    iint=(imax-imin)/float(nlvls)
    level=imin+iint*findgen(nlvls)
    index=where(lat gt -90.)
    sflevel=min(sf(index))+((max(sf(index))-min(sf(index)))/float(nlvls-1))*findgen(nlvls-1)
    pv(*,-1)=0./0.
    map_set,/MOLLWEIDE,0,140,0,/noeras,color=0,/contin,/grid,/isotropic,/horizon
    contour,o3,x,alat,/noeras,/cell_fill,c_color=col1,levels=level,color=0,charsize=1,charthick=2,/overplot
    map_set,/MOLLWEIDE,0,140,0,/noeras,color=0,/contin,/grid,/isotropic,/horizon
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=12,c_labels=0
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=5,c_label=0*sflevel
    xyouts,xmn+0.11,ymx-0.02,'SD-WACCM Ozone',color=0,/normal,charthick=2
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
    map_set,/MOLLWEIDE,0,140,0,/noeras,color=0,/contin,/grid,/isotropic,/horizon
    contour,o3map2,xnd2,ynd,level=level,/cell_fill,c_color=col1,/noeras,color=0,/overplot,charsize=1,charthick=2
    map_set,/MOLLWEIDE,0,140,0,/noeras,color=0,/contin,/grid,/isotropic,/horizon
    contour,pv,x,alat,/overplot,/noeras,/follow,color=mcolor,levels=[4],thick=12,c_labels=0
    contour,sf,x,alat,level=sflevel,/overplot,/follow,color=mcolor,thick=5,c_label=0*sflevel
    xyouts,xmn+0.08,ymx-0.02,'Trajectory Mapped Ozone',color=0,/normal,charthick=2

    ymnb=yorig(1) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,min(xorig)+0.1,max(xorig)+xlen-0.1,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle=thlab+' Ozone (ppmv)',charsize=1.25,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim rdf_socrates_'+sdate+'_2pan+xys_'+isuff(nn)+'_final.eps -rotate -90 rdf_socrates_'+sdate+'_2pan+xys_'+isuff(nn)+'_final.jpg'
;      spawn,'/usr/bin/rm rdf_socrates_'+sdate+'_2pan+xys_final.eps'
    endif
endfor
end
