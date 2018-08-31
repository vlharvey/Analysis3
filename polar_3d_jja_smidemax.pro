;
; WACCM 3D Arctic vortex and anticyclones for 300-year JJA average
; Arctic vortex colored by Temperature
;
!noeras=-1
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
cbarydel=0.01
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

;
; restore 300-year seasonal averages
; ALAT            FLOAT     = Array[96]
; ALON            FLOAT     = Array[144]
; DJF_MARK        FLOAT     = Array[96, 144, 22]
; DJF_PRESS       FLOAT     = Array[22]
; DJF_TEMP        FLOAT     = Array[96, 144, 22]
; DJF_U           FLOAT     = Array[96, 144, 22]
; DJF_Z           FLOAT     = Array[96, 144, 22]
; JJA_MARK        FLOAT     = Array[96, 144, 22]
; JJA_PRESS       FLOAT     = Array[22]
; JJA_TEMP        FLOAT     = Array[96, 144, 22]
; JJA_U           FLOAT     = Array[96, 144, 22]
; JJA_Z           FLOAT     = Array[96, 144, 22]
; NC              LONG      =          144
; NR              LONG      =           96
; NTH             LONG      =           22
; TH              FLOAT     = Array[22]
;
restore,'smidemax_300-year_TUmark_djf_jja.sav

mark2=JJA_MARK
p2=JJA_PRESS
t2=JJA_TEMP
u2=JJA_U
z2=JJA_Z

    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.

; select theta levels to plot
;   if l eq 0 then begin
       zindex=where(th ge 600.,nth2)
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
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_3d_jja_smidemax.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

; coordinate transformation
    xcn=fltarr(nc+1,nr2)
    ycn=fltarr(nc+1,nr2)
    for j=nr2,nr-1 do begin
        ANG = (90. - alat(j)) * RADG * 0.5
        FACTOR = TAN(ANG) * FAC20
        for i=0,nc do begin
            THETA = (x(i) - 90.) * RADG
            xcn(i,j-nr2) = FACTOR * COS(THETA)
            ycn(i,j-nr2) = FACTOR * SIN(THETA)
        endfor
    endfor
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
    set_viewport,.1,.9,.1,.9
    !type=2^6+2^5     ; suppress x and y axes
    dum=fltarr(nc+1,nr2)
    irot=210.
    surface,dum,xcs,ycs,xrange=[-1.0,1.0],yrange=[-1.0,1.0],/noeras,$
            zrange=[0,1],/save,/nodata,zstyle=4,charsize=3.0,az=irot
    for kk=0,nth2-1 do begin
        lev=zindex(nth2-1-kk)
        nz=kk*(1./(nth2-1.))
        nz2=(kk+1.)*(1./(nth2+1.))
        nz3=(kk+4.)*(1./(nth2+8.))
        nz4=(kk+8.)*(1./(nth2+16.))
        mark1=transpose(mark2(*,*,lev))

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
            if k eq 1 then lat=30.+0.*fltarr(361)
            if k eq 2 then lat=60.+0.*fltarr(361)
            for j=0,360 do begin
                ANG = (90. - lat(j)) * RADG * 0.5
                FACTOR = TAN(ANG) * FAC20
                THETA = (lon(j) - 90.) * RADG
                lonp(j) = FACTOR * COS(THETA)
                latp(j) = FACTOR * SIN(THETA)
            endfor
            oplot,lonp,latp,/T3D,zvalue=nz,color=0,thick=2
        endfor
        MAP_SET,-90,0,0,/stereo,/contin,/grid,/noborder,/noeras,londel=90.,$
            label=1,lonlab=-1,charsize=3,latdel=180.,/t3d,zvalue=nz,color=0
;
; fill continents grey
;
        loadct,0
        map_continents,mlinethick=2,/t3d,zvalue=nz,color=mcolor*.45,/fill_continents
        loadct,39
        endif
;index=where(lon ge 180. and lon le 270.)
;oplot,lonp(index),latp(index),psym=8,symsize=2,color=250,/t3d,zvalue=nz
;stop
 
        nz2=(kk+1.)*(1./(nth2+1.))
        dum=fltarr(nc+1,nr2)
        dum(0:nc-1,0:nr2-1)=mark1(0:nc-1,0:nr/2-1)    ; SH
        dum(nc,*)=dum(0,*)
;
; vortex 
;
        lindex=where(dum gt 0.1,nl)
        imin=180.
        imax=300.
	if lindex(0) ne -1 then begin
           index=where(temp lt imin)
           if index(0) ne -1L then temp(index)=imin+1.
           index=where(temp gt imax)
           if index(0) ne -1L then temp(index)=imax-1.

           contour,dum,xcs,ycs,levels=[0.1],color=0,/noeras,$
                   c_labels=0,/T3D,zvalue=nz,thick=10,max_value=1.e15
           for ii=0,nl-1 do begin
               oplot,[xcs(lindex(ii)),xcs(lindex(ii))],$
                     [ycs(lindex(ii)),ycs(lindex(ii))],$
                     /T3D,zvalue=nz,psym=8,symsize=2,$
                     color=((temp(lindex(ii))-imin)/(imax-imin))*icolmax
           endfor
           contour,dum,xcs,ycs,levels=[0.1],color=0,$
                   c_labels=0,/T3D,zvalue=nz,thick=10,max_value=1.e15
        endif
;
; anticyclones
;
        index=where(y2d ge -45.)         ; ignore tropical/subtropical highs
        dum(index)=0./0.
        lindex=where(dum lt -0.25,nl)
        if lindex(0) ne -1 then begin
           oplot,xcs(lindex),ycs(lindex),/T3D,zvalue=nz,psym=8,symsize=2,color=0
loadct,0
            contour,dum,xcs,ycs,levels=[-0.25],color=mcolor*.7,$
                    c_labels=0,/T3D,zvalue=nz,thick=10
loadct,39
        endif
;
jumplev:
        if kk mod 2 eq 0 then xyouts,.08,nz4,savgz,color=0,/normal,charsize=2,charthick=2
;       xyouts,.08,nz4,thlevs(kk),color=0,/normal,charsize=2,charthick=2
    endfor	; loop over stacked polar plots
;
    !psym=0
    xyouts,0.42,0.85,'JJA',/normal,charsize=3,color=0,charthick=2
    xyouts,.05,.8,'Altitude (km)',charsize=2,/normal,color=0,charthick=2
    set_viewport,.2,.78,.14-cbaryoff,.14-cbaryoff+cbarydel
    !type=2^2+2^3+2^6
nlvls=15
iint=(imax-imin)/float(nlvls)
level=imin+iint*findgen(nlvls)
col1=1+(findgen(nlvls)/nlvls)*mcolor
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],xtitle='Temperature',/noeras,$
          xtickname=strcompress(string(fix(level)),/remove_all),$
          xtickv=level,xticks=nlvls-1,charsize=1,color=0,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
      xbox=[x1,x1,x1+dx,x1+dx,x1]
      polyfill,xbox,ybox,color=col1(j)
      x1=x1+dx
    endfor
    !p.charthick=1.
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim polar_3d_jja_smidemax.ps -rotate -90 polar_3d_jja_smidemax.jpg'
;      spawn,'rm -f polar_3d_jja_smidemax.ps'
    endif
end
