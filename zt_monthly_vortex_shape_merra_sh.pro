;
; SH version
; save monthly vortex shape diagnostics
; MERRA version
;
@rd_merra_nc3
@vortexshape

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.20]
yorig=[0.35]
cbaryoff=0.1
cbarydel=0.01
xlen=0.6
ylen=0.6
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
re=40000./2./!pi
earth_area=4.*!pi*re*re
hem_area=earth_area/2.0
rtd=double(180./!pi)
dtr=1./rtd
dum=1979+indgen(36)
syear=strcompress(dum,/remove_all)	; 1979-2014
nyear=n_elements(syear)
smon=['01','02','03','04','05','06','07','08','09','10','11','12']
nmon=n_elements(smon)
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
;
; get file listing
;
dir='/Volumes/Data/MERRA_data/Datfiles/MERRA-on-WACCM_theta_'

for iyear=0L,nyear-1L do begin
for imon=0L,nmon-1L do begin
dum=findfile('ztd+vortex_shape_merra_'+syear(iyear)+smon(imon)+'_sh.sav')
;if dum(0) ne '' then goto,skipmon
ifiles=file_search(dir+syear(iyear)+smon(imon)+'??.nc3',count=nfile)
print,syear(iyear),smon(imon),nfile
if nfile eq 0L then goto,skipmon
sdate_all=strarr(nfile)
;
; loop over files
;
icount=0L
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    result2=result(0)
    result3=strsplit(result2,'_',/extract)
    sdate=result3(-1)
    print,sdate
    sdate_all(n)=sdate
    dum=findfile(dir+sdate+'.nc3')
    if dum ne '' then ncfile0=dir+sdate+'.nc3'
    rd_merra_nc3,ncfile0,nc,nr,nth,alon,alat,th,pv2,p2,$
       u2,v2,qdf2,mark2,qv2,z2,sf2,q2,iflag
    if iflag ne 0L then goto,jumpstep
    tmp2=0.*p2
    for k=0L,nth-1L do tmp2(*,*,k)=th(k)*(p2(*,*,k)/1000.)^0.286
;
; monthly files
;
    if icount eq 0L then begin
       rth=2400.
       rth=3000.
;      print,th
;      read,'Enter desired theta ',rth
       index=where(abs(rth-th) eq min(abs(rth-th)))
       ith=index(0)
       sth=strcompress(long(th(ith)),/remove_all)+'K'
       x=fltarr(nc+1)
       x(0:nc-1)=alon(0:nc-1)
       x(nc)=alon(0)+360.
       x2d=fltarr(nc+1,nr)
       y2d=fltarr(nc+1,nr)
       for i=0,nc do y2d(i,*)=alat
       for j=0,nr-1 do x2d(*,j)=x

         lon=fltarr(nc,nr)
         lat=fltarr(nc,nr)
         for i=0,nc-1 do lat(i,*)=alat
         for j=0,nr-1 do lon(*,j)=alon
         area=0.*lat
         deltax=alon(1)-alon(0)
         deltay=alat(1)-alat(0)
         for j=0,nr-1 do begin
             hy=re*deltay*dtr
             dx=re*cos(alat(j)*dtr)*deltax*dtr
             area(*,j)=dx*hy    ; area of each grid point
         endfor

       ztd=fltarr(nfile,nth)

       area1=fltarr(nfile,nth)
       centroid_longitude1=fltarr(nfile,nth)
       centroid_latitude1=fltarr(nfile,nth)
       number_vortex_lobes1=fltarr(nfile,nth)
       ellipticity1=fltarr(nfile,nth)
       altitude=fltarr(nfile,nth)
    endif
;
; find vortex centroids, ellipticity_profile
;
      marker_USLM = make_array(nc,nr,nth)
      for k=0,nth-1 do marker_USLM(*,*,k) = transpose(mark2(*,*,k))
      shape = vortexshape(marker_USLM, alat, alon)
      centroid=shape.nhcentroid
      centroidx=reform(centroid(0,*))
      centroidy=reform(centroid(1,*))
      axis=shape.axis
      majoraxis=reform(axis(0,*))
      minoraxis=reform(axis(1,*))
      ellipticity_profile=minoraxis/majoraxis
      index=where(centroidx lt 0.)
      if index(0) ne -1L then centroidx(index)=centroidx(index)+360.
      centroid_longitude1(icount,*)=centroidx
      centroid_latitude1(icount,*)=centroidy
      ellipticity1(icount,*)=ellipticity_profile
;
; compute vortex area based on SF and CO
;
      for k = 0L, nth - 1L do begin
          mark1=transpose(mark2(*,*,k))
          index=where(lat gt 0. and mark1 eq 1.0,nn)
          area1(icount,k)=100.*total(area(index))/hem_area
      endfor
;
; count number of vortex lobes at each altitude
;
      nr2=nr/2
      altarray=fltarr(nth)
;erase
;xmn=xorig(0)
;xmx=xorig(0)+xlen
;ymn=yorig(0)
;ymx=yorig(0)+ylen
;set_viewport,xmn,xmx,ymn,ymx
;!type=2^2+2^3
      for k = 0L, nth - 1L do begin
          x2d=fltarr(nr2,nc)
          y2d=fltarr(nr2,nc)
          for i=0L,nc-1 do y2d(*,i)=alat(nr2:nr-1)
          for j=0L,nr2-1 do x2d(j,*)=alon

          dailymark = transpose(reform(mark2[nr2:nr-1,*,k]))
          if max(dailymark) eq 0. then goto,jumpthislev

          marker=fltarr(nc+1L,nr2)
          marker[0L:nc-1L,*] = dailymark
          marker[nc,*] = marker(0,*)
          sf=fltarr(nc+1L,nr2)
          sf[0L:nc-1L,*] = transpose(reform(sf2[nr2:nr-1,*,k]))
          sf[nc,*] = sf(0,*)
          z=fltarr(nc+1L,nr2)
          z[0L:nc-1L,*] = transpose(reform(z2[nr2:nr-1,*,k]))/1000.
          z[nc,*] = z(0,*)
          index=where(marker gt 0.)
          if index(0) eq -1L then index=where(y2d ge 60.)       ; if no vortex then take polar cap average altitude
          if index(0) ne -1L then begin
             altarray(k)=mean(z(index))
             altitude(icount,k)=mean(z(index))
          endif
;map_set,90,0,-90,/stereo,/contin,/grid,/noerase,color=0,title=sdate+' '+strcompress(th(k))
;contour, transpose(reform(mark2[*,*,k])),alon,alat,levels=[0.1],color=(float(k)/(float(nth)))*mcolor,thick=5,/overplot
;oplot,[centroid_longitude1(icount,k),centroid_longitude1(icount,k)],[centroid_latitude1(icount,k),centroid_latitude1(icount,k)],psym=8,color=(float(k)/(float(nth)))*mcolor,symsize=2
;
; histograms of vortex latitude and longitude
;
          !type=2^2+2^3
          py1=fltarr(nc)
          px1=fltarr(nr)
          for i=0L,nc-1L do begin
              index=where(marker(i,*) gt 0.,nn)
              py1(i)=float(nn)
          endfor
          for j=0L,nr2-1L do begin
              index=where(marker(*,j) gt 0.,nn)
              px1(j)=float(nn)
          endfor
;
; are there two cyclonic vortices?
;
          n0=findgen(nc)
          n1=1.+findgen(nc)
          vortlon=0.*alon
          index=where(py1 ne 0.)
          if index(0) ne -1L then vortlon(index)=1.
          index=where(abs(vortlon(n0)-vortlon(n1)) gt 0.,nv)
          nextra=1
          if nv eq 0L then nv=2                   ; circumpolar has no zeros. set to 2 to get 1 vortex
          index=where(vortlon eq 1.)
          if min(alon(index)) eq min(alon) and max(alon(index)) ne max(alon) then nextra=0        ; GM edge
          if min(alon(index)) ne min(alon) and max(alon(index)) eq max(alon) then nextra=0        ; GM edge
          if nv gt 2L then begin
             nextra=0.5*nv                ; each vortex results in 2 edge points - unless it lies exactly along the GM
          endif
          nv=round(nv-nextra)
          number_vortex_lobes1(icount,k)=nv

jumpthislev:
;print,altitude(icount,k),th(k),number_vortex_lobes1(icount,k),ellipticity1(icount,k),centroid_longitude1(icount,k),centroid_latitude1(icount,k)

      endfor	; loop over levels

;set_viewport,.1,.35,.1,.3
;plot,area1(icount,*),th,thick=3,color=0,/noeras,xtitle='Area'
;set_viewport,.4,.65,.1,.3
;plot,ellipticity1(icount,*),th,thick=3,color=0,/noeras,xtitle='Ellipticity'
;set_viewport,.7,.95,.1,.3
;plot,centroid_latitude1(icount,*),th,thick=3,color=0,/noeras,xtitle='Centroid Latitude'
;stop
;
icount=icount+1L
jumpstep:
endfor	; loop over files
;
; save monthly file
;
jumpmon:
index=where(ztd eq 0.)
ztd(index)=0./0.
ztd=smooth(ztd,3,/nan)
ztd=smooth(ztd,3,/nan)
save,filename='vortex_shape_merra_'+syear(iyear)+smon(imon)+'_sh.sav',nth,nfile,th,sdate_all,centroid_longitude1,area1,$
     centroid_latitude1,number_vortex_lobes1,ellipticity1,altitude

skipmon:
endfor	; loop over months
endfor	; loop over years
end
