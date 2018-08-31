;
; Antarctic vortex colored by Temperature
; Anticyclones in black poleward of 13.75S (or 20S)
;
@stddat
@kgmt
@ckday
@kdate
@rd_sdwaccm4_nc3_new

loadct,39
mcolor=byte(!p.color)
mcolor=255
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,2*cos(a),2*sin(a),/fill
nxdim=800
nydim=800
cbaryoff=0.065
cbarydel=0.02
lstmn=1
lstdy=1
lstyr=1979
ledmn=1
leddy=1
ledyr=2015
lstday=0
ledday=0
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
;
; Ask interactive questions- get starting/ending date and p surface
;
print, ' '
print, '      SD-WACCM Version '
print, ' '
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
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
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD_New/'
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount=0L
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
      date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4)',$
                              month(imn-1),' ',idy,', ',iyr))

      syr=strcompress(iyr,/remove_all)
      smn=string(FORMAT='(i2.2)',imn)
      sdy=string(FORMAT='(i2.2)',idy)
      sdate=syr+smn+sdy
      ifile=dirw+'f.e11.FWTREFC1SD.f19.f19.ccmi30.001.cam.h5.'+sdate+'.nc3'
      rd_sdwaccm4_nc3_new,ifile,nc,nr,nth,alon,alat,th,$
         pv2,p2,z2,u2,v2,q2,qdf2,mark2,sf2,h2o2,n2o2,o32,co22,no2,no22,iflag
      if iflag ne 0L then goto,jump
tmp2=0.*p2
for k=0L,nth-1L do tmp2(*,*,k)=th(k)*(p2(*,*,k)/1000.)^0.286
      x=fltarr(nc+1)
      x(0:nc-1)=alon(0:nc-1)
      x(nc)=alon(0)+360.
;
; select theta levels to plot
    if icount eq 0L then begin
       zindex=where(th ge 300. and th le 100000.,nth2)
       thlevs=reverse(strcompress(string(fix(th(zindex))))+' K')
       thlw=min(th(zindex))
       thup=max(th(zindex))
       th2=reverse(th(zindex))
       nr2=nr/2
       x2d=fltarr(nc+1,nr2)
       y2d=fltarr(nc+1,nr2)
       for i=0,nc do y2d(i,*)=alat(0:nr/2-1)
       for j=0,nr2-1 do x2d(*,j)=x
       dy=alat(1)-alat(0)
       icount=1L
    endif

; save postscript version
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='3D_Antarctic_vortex/'+sdate+'_sdwaccm_new_3D_antarctic_vortex.ps'
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
    irot=-110.
    surface,dum,xcs,ycs,xrange=[-1.0,1.0],yrange=[-1.0,1.0],/noeras,$
            zrange=[thlw,thup],/save,/nodata,zstyle=4,charsize=3.0,az=irot
    col1=fltarr(nth2)
    for kk=0,nth2-1 do begin
        index=where(th eq th2(kk))
        lev=index(0)
        nz=kk*(1./(nth2-1.))
        nz2=(kk+1.)*(1./(nth2+1.))
        nz3=(kk+4.)*(1./(nth2+8.))
        nz4=(kk+8.)*(1./(nth2+16.))
        nz5=(kk+16.)*(1./(nth2+32.))
        mark1=transpose(mark2(*,*,lev))
        sf1=transpose(sf2(*,*,lev))
        pv1=transpose(pv2(*,*,lev))
        p1=transpose(p2(*,*,lev))
        mpv1=pv1*((th(lev)/300.))^(-9./2.)
;print,th(lev),max(p1)

; temperature
        temp1=th(lev)*(p1/1000.)^.286
        temp=fltarr(nc+1,nr2)
        temp(0:nc-1,0:nr2-1)=temp1(0:nc-1,0:nr2-1)    ; SH
        temp(nc,*)=temp(0,*)
        index=where(temp eq 0.)
        if index(0) ne -1L then temp(index)=-1.e15

; height and pressure of theta surface
        zth=transpose(z2(*,*,lev))/1000.
        zth=reform(zth(*,0:nr2-1))      ; SH
        index=where(zth ne 0.)
        if n_elements(index) le 3L then goto,jumplev
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
                latp(j) = -1.0 * FACTOR * SIN(THETA)
            endfor
            oplot,lonp,latp,/T3D,zvalue=nz,color=0,thick=2
        endfor
        MAP_SET,-90,0,-1.*irot,/stereo,/contin,/grid,/noborder,/noeras,londel=90.,$
            label=1,lonlab=1,charsize=2,latdel=180.,/t3d,zvalue=nz
;
; fill continents green
;
        loadct,0
        map_continents,mlinethick=2,/t3d,zvalue=nz,color=mcolor*.4,/fill_continents
;
; superimpose stream function
;
        dum(0:nc-1,0:nr2-1)=sf1(0:nc-1,0:nr2-1)    ; SH
        dum(nc,*)=dum(0,*)
        smin=min(dum)
        smax=max(dum)
        sint=(smax-smin)/15.
        sflevel=smin+sint*findgen(15)
        contour,dum,xcs,ycs,levels=sflevel,color=0,c_labels=0+0.*sflevel,$
                /T3D,zvalue=nz,thick=2
        loadct,39
        endif
 
        nz2=(kk+1.)*(1./(nth2+1.))
        col1(kk)=nz2*icolmax
        dum=fltarr(nc+1,nr2)
        dum(0:nc-1,0:nr2-1)=mark1(0:nc-1,0:nr2-1)    ; SH
        dum(nc,*)=dum(0,*)
;
; sub-vortex modification
;
        if th(lev) le 300. then begin
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
        imin=150.
        imax=320.
	if lindex(0) ne -1 then begin
           for ii=0,nl-1 do begin
               if temp(lindex(ii)) ne 1.e15 then $
               oplot,[xcs(lindex(ii)),xcs(lindex(ii))],$
                     [ycs(lindex(ii)),ycs(lindex(ii))],$
                     /T3D,zvalue=nz,psym=8,symsize=2,$
                     color=((temp(lindex(ii))-imin)/(imax-imin))*icolmax
;              if temp(lindex(ii)) eq 1.e15 then $
;              oplot,[xcs(lindex(ii)),xcs(lindex(ii))],$
;                    [ycs(lindex(ii)),ycs(lindex(ii))],$
;                    /T3D,zvalue=nz,psym=8,symsize=0.5,color=0
           endfor
;          contour,temp,xcs,ycs,levels=[180.],color=0.1*mcolor,/T3D,zvalue=nz,thick=2,min_value=-1.e15
           if th(lev) gt 2000. then contour,temp,xcs,ycs,levels=[120,130,140,150,160],c_color=[20,50,70,90,110],/T3D,zvalue=nz,thick=10,min_value=-1.e15
           contour,dum,xcs,ycs,levels=[0.1],color=0,$
                   c_labels=0,/T3D,zvalue=nz,thick=3,max_value=1.e15
        endif
;
;
; anticyclones
;
        lindex=where(dum lt 0.0,nl)
        if lindex(0) ne -1 then begin
           oplot,xcs(lindex),ycs(lindex),/T3D,zvalue=nz,psym=8,symsize=2,color=0
 loadct,0
index=where(dum lt 0.)
if index(0) ne -1L then dum(index)=-1.
           contour,dum,xcs,ycs,levels=[-0.1],color=mcolor*.7,$
                   c_labels=0,/T3D,zvalue=nz,thick=3
;            nhigh=abs(min(dum(lindex)))
; sdum=0.*dum
;         sdum(0:nc-1,0:nr2-1)=sf1(0:nc-1,0:nr2-1)    ; SH
;         sdum(nc,*)=sdum(0,*)
; dx=x2d(1,0)-x2d(0,0)
;            for ihigh=0,nhigh-1 do begin
;                index=where(dum eq -1.0*(ihigh+1))
;                if index(0) eq -1 then goto,jump1
; ;              if max(y2d(index)) ge -13.7500 then goto,jump1
;                if max(y2d(index)) ge -20. then goto,jump1              ; exclude more tropical anticyclones
;                sedge=max(sdum(index))     ; value of SF to contour
;                tmp=sdum
;                xmax=max(x2d(index))+1.0*dx      ; isolate region
;                xmin=min(x2d(index))-1.0*dx
;                ymax=max(y2d(index))+2.0*dy
;                ymin=min(y2d(index))-2.0*dy
;                if xmin lt x(0) and xmax gt x(nc) then begin     ; GM
;                   index=where(x2d gt 180. and dum eq -1.0*(ihigh+1))
;                   xmax2=min(x2d(index))-2.0*dx
;                   index=where(x2d lt 180. and dum eq -1.0*(ihigh+1))
;                   xmin2=max(x2d(index))+2.0*dx
;                   index=where((x2d lt xmax2 and x2d gt xmin2) or (y2d lt ymin or y2d gt ymax))
;                endif
;                if xmin gt x(0) or xmax lt x(nc) then $
;                   index=where(x2d lt xmin or x2d gt xmax or y2d lt ymin or y2d gt ymax)
;                if index(0) ne -1 then tmp(index)=9999.
;                index=where(tmp ne -9999. and y2d le -3.75 and dum eq -1.0*(ihigh+1))
;;               index=where(tmp ne -9999. and y2d lt -20. and dum eq -1.0*(ihigh+1))
;                if index(0) eq -1L then goto,jump1
;                oplot,xcs(index),ycs(index),psym=8,color=0,/T3D,zvalue=nz,symsize=2
;;               contour,tmp,xcs,ycs,levels=[sedge],color=icolmax*.7,$
;;                 /T3D,zvalue=nz,c_linestyle=0,/overplot,max_value=9999.,thick=3
;                jump1:
;            endfor               ; loop over anticyclones

 loadct,39
         endif

jumplev:
        if kk mod 4 eq 0 or kk eq nth2-1 or kk eq 0 then begin
        if kk ne nth2-1L then xyouts,.83,nz5-0.05,savgz+' km',color=0,/normal,charsize=2,charthick=2
        if kk eq nth2-1L then xyouts,.83,nz5,savgz+' km',color=0,/normal,charsize=2,charthick=2
;       xyouts,.08,nz4,thlevs(kk),color=0,/normal,charsize=2,charthick=2
        endif
    endfor	; loop over stacked polar plots
    !psym=0
    xyouts,0.35,0.88,sdate,/normal,charsize=3.0,color=0,charthick=2
;   xyouts,.08,.85,'Theta (K)',charsize=2,/normal,color=0,charthick=2
;   xyouts,.78,.85,'Pressure (hPa)',charsize=2,/normal,color=0,charthick=2
    xyouts,.78,.9,'Altitude (km)',charsize=2,/normal,color=0,charthick=2
    set_viewport,.2,.78,.14-cbaryoff,.14-cbaryoff+cbarydel
    !type=2^2+2^3+2^6
    iint=(imax-imin)/12.
    level=imin+iint*findgen(13)
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],xtitle='SD-WACCM Temperature',/noeras,$
          xtickname=strcompress(string(fix(level)),/remove_all),$
          xstyle=1,xticks=12,charsize=1.5,color=0,charthick=2
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
       spawn,'convert -trim 3D_Antarctic_vortex/'+sdate+'_sdwaccm_new_3D_antarctic_vortex.ps -rotate -90 3D_Antarctic_vortex/'+sdate+'_sdwaccm_new_3D_antarctic_vortex.jpg'
       spawn,'rm -f 3D_Antarctic_vortex/'+sdate+'_sdwaccm_new_3D_antarctic_vortex.ps'
    endif

goto, jump
end
