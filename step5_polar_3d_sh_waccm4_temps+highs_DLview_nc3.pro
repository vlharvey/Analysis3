;
; WACCM4 3D Antarctic vortex and anticyclones
; nc4 version where circumpolar highs are marked
; Antarctic vortex colored by Temperature
; Anticyclones in black poleward of 13.75S (or 20S)
;
@stddat
@kgmt
@ckday
@kdate

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,2*cos(a),2*sin(a),/fill
nxdim=800
nydim=800
cbaryoff=0.065
cbarydel=0.02
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
RADG = !PI / 180.
FAC20 = 1.0 / TAN(45.*RADG)
mon=['jan','feb','mar','apr','may','jun',$
     'jul','aug','sep','oct','nov','dec']
month=['January','February','March','April','May','June',$
       'July','August','September','October','November','December']
!noeras=1
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/mee00fpl_FW2.cam2.h3.dyns.'
dir='/Volumes/earth/harvey/WACCM_data/Datfiles/Datfiles_WACCM4/mee00fpl_FW2.cam2.h3.dyns.'
lstmn=1L & lstdy=1L & lstyr=2003L
;lstmn=10L & lstdy=1L & lstyr=2003L
ledmn=12L & leddy=31L & ledyr=2042L
lstday=0L & ledday=0L
;
; get date range
;
print, ' '
print, '      WACCM-4 Version '
print, ' '
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
if lstyr lt 2000 then lstyr=lstyr+2000
if ledyr lt 2000 then ledyr=ledyr+2000
if lstyr lt 2002 then stop,'Year out of range '
if ledyr lt 2002 then stop,'Year out of range '
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
;
; --- Loop here --------
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr
;
; --- Test for end condition
;
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then stop,' Normal termination condition '
;
; construct date string
;
      syr=strcompress(iyr,/remove_all)
      smn=string(FORMAT='(i2.2)',imn)
      sdy=string(FORMAT='(i2.2)',idy)
      sdate=syr+smn+sdy
;
; read daily data
;
    ncfile0=dir+sdate+'_3D_dyn.nc3'
    dum=findfile(ncfile0)
    if dum(0) eq '' then goto,jump
    print,ncfile0
    ncid=ncdf_open(ncfile0)
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
        if result.name eq 'latitude' then alat=data
        if result.name eq 'longitude' then alon=data
        if result.name eq 'theta' then th=data
        if result.name eq 'IPV' then pv2=data
        if result.name eq 'P' then p2=data
        if result.name eq 'U' then u2=data
        if result.name eq 'V' then v2=data
        if result.name eq 'QDF' then qdf2=data
        if result.name eq 'Q' then q2=data
        if result.name eq 'GPH' then gph2=data
        if result.name eq 'TTGW' then ttgw2=data
        if result.name eq 'SF' then sf2=data
        if result.name eq 'MARK' then mark2=data
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
    s2=sqrt(u2^2+v2^2)
    xlon=alon
    xlat=alat

    t2=0.*p2
    z2=gph2/1000.
    for k=0,nth-1 do t2(*,*,k) = th(k)*( (p2(*,*,k)/1000.)^(.286) )
    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.

; select theta levels to plot
;   if l eq 0 then begin
       zindex=where(th ge 450. and th le 30000.,nth2)
       thlevs=reverse(strcompress(string(fix(th(zindex))))+' K')
       thlw=min(th(zindex))
       thup=max(th(zindex))
       nr2=nr/2
       x2d=fltarr(nc+1,nr2)
       y2d=fltarr(nc+1,nr2)
       for i=0,nc do y2d(i,*)=alat(0:nr/2-1)
       for j=0,nr/2-1 do x2d(*,j)=x
       dy=alat(1)-alat(0)
;   endif

; save postscript version
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='3D_meeFW2_figs/polar_3d_sh_waccm4_'+sdate+'_DL+highs_nc3.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
   !p.thick=2.0                   ;Plotted lines twice as thick
   !p.charsize=2.0
endif

; coordinate transformation
    xcs=fltarr(nc+1,nr2)
    ycs=fltarr(nc+1,nr2)
    for j=0,nr2-1 do begin
        ANG = (90. + alat(j)) * RADG * 0.5
        FACTOR = TAN(ANG) * FAC20
        for i=0,nc do begin
            THETA = (x(i) - 90.) * RADG
            xcs(i,j) = FACTOR * COS(THETA)
            ycs(i,j) = -1.0 * FACTOR * SIN(THETA)
        endfor
    endfor

    erase
    !psym=0
    plots,.48,.226,/normal
    plots,.48,.78,/continue,/normal,thick=3
    set_viewport,.1,.9,.1,.9
    !type=2^6+2^5     ; suppress x and y axes
    dum=fltarr(nc+1,nr2)
    irot=-150.
    surface,dum,xcs,ycs,xrange=[-1.0,1.0],yrange=[-1.0,1.0],/noeras,$
            zrange=[thlw,thup],/save,/nodata,zstyle=4,charsize=3.0,az=irot
    col1=fltarr(nth2)
    for kk=0,nth2-1 do begin
        lev=zindex(nth2-1-kk)
        nz=kk*(1./(nth2-1.))
        nz2=(kk+1.)*(1./(nth2+1.))
        nz3=(kk+4.)*(1./(nth2+8.))
        nz4=(kk+8.)*(1./(nth2+16.))
        mark1=transpose(mark2(*,*,lev))
        sf1=transpose(sf2(*,*,lev))
        pv1=transpose(pv2(*,*,lev))
        p1=transpose(p2(*,*,lev))
        mpv1=pv1*((th(lev)/300.))^(-9./2.)

; temperature
        temp1=transpose(t2(*,*,lev))
        temp=fltarr(nc+1,nr2)
        temp(0:nc-1,0:nr2-1)=temp1(0:nc-1,0:nr/2-1)    ; SH
        temp(nc,*)=temp(0,*)

; height of theta surface
        zth=transpose(z2(*,*,lev))
        zth=reform(zth(*,0:nr/2-1))	; SH
        index=where(zth ne 0.)
        if n_elements(index) lt 2L then goto,jumplev
	result=moment(zth(index))
	avgz=result(0)
        savgz=strcompress(string(fix(avgz)),/remove_all)

; draw latitude circles
        if kk eq 0 then begin
        !psym=0
        lon=findgen(361)
        lonp=0.*lon
        latp=0.*lon
        for k=0,0 do begin
            if k eq 0 then lat=0.*fltarr(361)
            if k eq 1 then lat=-30.+0.*fltarr(361)
            if k eq 2 then lat=-60.+0.*fltarr(361)
            for j=0,360 do begin
                ANG = (90. + lat(j)) * RADG * 0.5
                FACTOR = TAN(ANG) * FAC20
                THETA = (lon(j) - 90.) * RADG
                lonp(j) = FACTOR * COS(THETA)
                latp(j) = -1. * FACTOR * SIN(THETA)
            endfor
            oplot,lonp,latp,/T3D,zvalue=nz,color=0,thick=2
        endfor
;       MAP_SET,90,0,-1.*irot,/stereo,/contin,/grid,/noborder,/noeras,londel=90.,$
        MAP_SET,-90,0,0,/stereo,/contin,/grid,/noborder,/noeras,londel=90.,$
            label=1,lonlab=1,charsize=2,latdel=180.,/t3d,zvalue=nz
;
; fill continents grey
;
        loadct,0
        map_continents,mlinethick=2,/t3d,zvalue=nz,color=mcolor*.45,/fill_continents
        loadct,39
;
; superimpose stream function
;
        dum(0:nc-1,0:nr2-1)=sf1(0:nc-1,0:nr/2-1)    ; SH
        dum(nc,*)=dum(0,*)
        smin=min(dum)
        smax=max(dum)
        sint=(smax-smin)/15.
        sflevel=smin+sint*findgen(15)
        contour,dum,xcs,ycs,levels=sflevel,color=0,c_labels=0+0.*sflevel,$
                /T3D,zvalue=nz,thick=1
        endif
        loadct,39
;index=where(lon ge 180. and lon le 270.)
;oplot,lonp(index),latp(index),psym=8,symsize=2,color=250,/t3d,zvalue=nz
;stop
 
        nz2=(kk+1.)*(1./(nth2+1.))
        col1(kk)=nz2*icolmax
        dum=fltarr(nc+1,nr2)
        dum(0:nc-1,0:nr2-1)=mark1(0:nc-1,0:nr/2-1)    ; SH
        dum(nc,*)=dum(0,*)
;
; sub-vortex modification
;
        if th(lev) le 400. then begin
           lindex=where(dum gt 0.0,nl)
           mpv=fltarr(nc+1,nr2)
           mpv(0:nc-1,0:nr2-1)=mpv1(0:nc-1,0:nr2-1)
           mpv(nc,*)=mpv(0,*)
           if lindex(0) eq -1 then begin
              index=where(mpv le -0.000004 and y2d le -40.)
              if index(0) ne -1 then dum(index)=1.
           endif
           if lindex(0) ne -1 then begin
              if max(y2d(lindex)) ge -40. then begin
                 index=where(mpv le -0.000004 and y2d le -40.)
                 if index(0) ne -1 then dum(index)=1.
                 index=where(mpv gt -0.000004)
                 if index(0) ne -1 then dum(index)=0.
              endif
           endif
        endif

        lindex=where(dum gt 0.0,nl)
        imin=120.
        imax=300.
hot=where(temp ge 300.)
if hot(0) ne -1L then temp(hot)=300.-1.
	if lindex(0) ne -1 then begin
           for ii=0,nl-1 do begin
               oplot,[xcs(lindex(ii)),xcs(lindex(ii))],$
                     [ycs(lindex(ii)),ycs(lindex(ii))],$
                     /T3D,zvalue=nz,psym=8,symsize=2,$
                     color=((temp(lindex(ii))-imin)/(imax-imin))*icolmax
           endfor
;          contour,temp,xcs,ycs,levels=[280.,290.,300.,310.],color=.9*mcolor,$
;                  /T3D,zvalue=nz,thick=2,max_value=1.e15
           contour,dum,xcs,ycs,levels=[0.1],color=0,$
                   c_labels=0,/T3D,zvalue=nz,thick=3,max_value=1.e15
        endif
;
; anticyclones
;
        lindex=where(dum lt 0.0,nl)
        if lindex(0) ne -1 then begin
;            oplot,xcs(lindex),ycs(lindex),/T3D,zvalue=nz,psym=8,symsize=2,color=0
loadct,0
;            contour,dum,xcs,ycs,levels=[-0.1],color=mcolor*.7,c_labels=0,/T3D,zvalue=nz,thick=3
           nhigh=abs(min(dum(lindex)))
sdum=0.*dum
        sdum(0:nc-1,0:nr2-1)=sf1(0:nc-1,0:nr2-1)    ; SH
        sdum(nc,*)=sdum(0,*)
dx=x2d(1,0)-x2d(0,0)
           for ihigh=0,nhigh-1 do begin
               index=where(dum eq -1.0*(ihigh+1))
               if index(0) eq -1 then goto,jump1
               if max(y2d(index)) ge -1.2500 then goto,jump1
               sedge=max(sdum(index))     ; value of SF to contour
               tmp=sdum
               xmax=max(x2d(index))+2.0*dx      ; isolate region
               xmin=min(x2d(index))-2.0*dx
               ymax=max(y2d(index))+3.0*dy
               ymin=min(y2d(index))-3.0*dy
               if xmin lt x(0) and xmax gt x(nc) then begin     ; GM
                  index=where(x2d gt 180. and dum eq -1.0*(ihigh+1))
                  xmax2=min(x2d(index))-2.0*dx
                  index=where(x2d lt 180. and dum eq -1.0*(ihigh+1))
                  xmin2=max(x2d(index))+2.0*dx
                  index=where((x2d lt xmax2 and x2d gt xmin2) or (y2d lt ymin or y2d gt ymax))
               endif
               if xmin gt x(0) or xmax lt x(nc) then $
                  index=where(x2d lt xmin or x2d gt xmax or y2d lt ymin or y2d gt ymax)
               if index(0) ne -1 then tmp(index)=0./0.
               index=where(tmp ne -9999. and y2d lt -1.2500 and dum eq -1.0*(ihigh+1))
               if index(0) eq -1L then goto,jump1
               oplot,xcs(index),ycs(index),psym=8,color=0,/T3D,zvalue=nz,symsize=2
               contour,tmp,xcs,ycs,levels=[sedge],color=icolmax*.7,$
                 /T3D,zvalue=nz,c_linestyle=0,/overplot,thick=3
               jump1:
           endfor               ; loop over anticyclones
loadct,39
        endif
jumplev:
        if kk mod 2 eq 0 then xyouts,.08,nz4,savgz+' km',color=0,/normal,charsize=2,charthick=2
;       xyouts,.08,nz4,thlevs(kk),color=0,/normal,charsize=2,charthick=2
    endfor	; loop over stacked polar plots
    !psym=0
    xyouts,0.2,0.9,'Antarctic Vortex in WACCM-4',/normal,charsize=3,color=0,charthick=2
    xyouts,0.4,0.95,sdate,/normal,charsize=3,color=0,charthick=2
;   xyouts,.08,.83,'Theta (K)',charsize=2,/normal,color=0,charthick=2
    xyouts,.08,.85,'Altitude (km)',charsize=2,/normal,color=0,charthick=2
    set_viewport,.2,.78,.14-cbaryoff,.14-cbaryoff+cbarydel
    !type=2^2+2^3+2^6
    iint=(imax-imin)/13.
    level=imin+iint*findgen(14)
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],xtitle='Temperature',/noeras,$
          xtickname=strcompress(string(fix(level)),/remove_all),$
          xstyle=1,xticks=13,charsize=1.5,color=0,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nth2)
    for j=0,nth2-1 do begin
      xbox=[x1,x1,x1+dx,x1+dx,x1]
      polyfill,xbox,ybox,color=col1(j)
      x1=x1+dx
    endfor
    !p.charthick=1.
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim 3D_meeFW2_figs/polar_3d_sh_waccm4_'+sdate+'_DL+highs_nc3.ps -rotate -90 '+$
                           '3D_meeFW2_figs/polar_3d_sh_waccm4_'+sdate+'_DL+highs_nc3.jpg'
       spawn,'rm -f 3D_meeFW2_figs/polar_3d_sh_waccm4_'+sdate+'_DL+highs_nc3.ps'
    endif

goto, jump
end
