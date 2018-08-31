;
; save DJF and JJA 300-year average marker for Cora
; 8/16/2016
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
cbaryoff=0.02
cbarydel=0.01
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
!noeras=1
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
lstmn=1L & lstdy=1L & lstyr=2001L
ledmn=12L & leddy=31L & ledyr=2300L
lstday=0L & ledday=0L
;
goto,quick
;
; get date range
;
print, ' '
print, '      WACCM Version '
print, ' '
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
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
      if ndays gt ledday then goto,saveit
;
; construct date string
;
      syr=string(FORMAT='(I3.3)',iyr-2000L)
      smn=string(FORMAT='(i2.2)',imn)
      sdy=string(FORMAT='(i2.2)',idy)
      sdate=syr+smn+sdy
;
; only read DJF and JJA
;
      if smn ne '12' and smn ne '01' and smn ne '02' and smn ne '06' and smn ne '07' and smn ne '08' then goto,jump
;
; read daily data
;
    ncfile0=dir+sdate+'.nc3'
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
;       if result.name eq 'IPV' then pv2=data
        if result.name eq 'P' then p2=data
;       if result.name eq 'U' then u2=data
;       if result.name eq 'V' then v2=data
;       if result.name eq 'QDF' then qdf2=data
;       if result.name eq 'CO' then co2=data
;       if result.name eq 'GPH' then gph2=data/1000.
;       if result.name eq 'SF' then sf2=data
        if result.name eq 'MARK' then mark2=data
;       print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; first time declare mean arrays
;
    if icount eq 0L then begin
       djf_mark=0.*mark2
       jja_mark=0.*mark2
       jja_pressure=0.*p2
       djf_pressure=0.*p2
       ndjf_mark=0.*mark2
       njja_mark=0.*mark2
       icount=1L
    endif
;
; DJF
;
    if smn eq '12' or smn eq '01' or smn eq '02' then begin
       djf_mark=djf_mark+mark2
       djf_pressure=djf_pressure+p2
       ndjf_mark=ndjf_mark+1.
    endif
;
; JJA
;
    if smn eq '06' or smn eq '07' or smn eq '08' then begin
       jja_mark=jja_mark+mark2
       jja_pressure=jja_pressure+p2
       njja_mark=njja_mark+1.
    endif
    goto,jump
;
; save IDL save file
;
saveit:
djf_mark=djf_mark/ndjf_mark
djf_pressure=djf_pressure/ndjf_mark
jja_mark=jja_mark/njja_mark
jja_pressure=jja_pressure/njja_mark
djf_press=fltarr(nth)
jja_press=fltarr(nth)
for kk=0,nth-1 do djf_press(kk)=mean(djf_pressure(nr/2:nr-1,*,kk))  ; NH
for kk=0,nth-1 do jja_press(kk)=mean(jja_pressure(0:nr/2-1,*,kk))  ; SH
save,file='smidemax_300-year_mark_djf_jja.sav',nc,nr,nth,alon,alat,th,djf_mark,jja_mark,djf_press,jja_press
quick:
restore,'smidemax_300-year_mark_djf_jja.sav'
zprof=[129,123,118,113,108,99,95,91,85,79,75,69,65,61,55,48,46,43,40,38,34,30]

; save postscript version
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_djf_jja_smidemax.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

;
; check
;
    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.
;
; select theta levels to plot
;   if l eq 0 then begin
       zindex=where(th le 100000.,nth2)
       thlevs=strcompress(string(fix(th(zindex))))
;   endif

y2d=fltarr(nc+1,nr)
for i=0L,nc do y2d(i,*)=alat

    erase
    set_viewport,.525,.95,.3,.725
    !type=2^2+2^3     ; suppress x and y axes
irot=135
    MAP_SET,90,0,irot,/ortho,/contin,/grid,/noeras,londel=90.,$
            label=1,lonlab=10,charsize=2,latdel=180.,color=0,title='DJF'
    for kk=0,nth2-1 do begin
        lev=zindex(nth2-1-kk)
        mark1=transpose(djf_mark(*,*,lev))
        dum=fltarr(nc+1,nr)
        dum(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)    ; NH
        dum(nc,*)=dum(0,*)
        clab=strcompress(long(zprof(lev)),/r)
        contour,dum,x,alat,levels=[0.1],color=(float(kk)/nth2)*mcolor,/overplot,c_labels=0,thick=15,max_value=1.e15	;,C_ANNOTATION=[clab]
        loadct,0
        index=where(y2d le 45.)
        dum(index)=0./0.
index=where(dum lt -0.25)
;if index(0) ne -1L then dum(index)=200.
if index(0) ne -1L and zprof(lev) gt 60. then print,'DJF ',zprof(lev)
;       if zprof(lev) gt 60 then contour,dum,x,alat,levels=[zprof(lev)],color=-30+(float(kk)/nth2)*mcolor,/overplot,c_labels=[1],thick=15,max_value=1.e15,c_charsize=2,c_charthick=2     ;,C_ANNOTATION=[clab]
        if zprof(lev) gt 60 then contour,dum,x,alat,levels=[-0.25],color=-30+(float(kk)/nth2)*mcolor,/overplot,c_labels=0,thick=15,max_value=1.e15	;,C_ANNOTATION=[clab]
        loadct,39
    endfor
    set_viewport,.05,.475,.3,.725
    !type=2^2+2^3     ; suppress x and y axes
    MAP_SET,-90,0,irot,/ortho,/contin,/grid,/noeras,londel=90.,$
            label=1,lonlab=-10,charsize=2,latdel=180.,color=0,title='JJA'
    for kk=0,nth2-1 do begin
        lev=zindex(nth2-1-kk)
        mark1=transpose(jja_mark(*,*,lev))
        dum=fltarr(nc+1,nr)
        dum(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)    ; SH
        dum(nc,*)=dum(0,*)
        clab=strcompress(long(zprof(lev)),/r)
        contour,dum,x,alat,levels=[0.1],color=(float(kk)/nth2)*mcolor,/overplot,c_labels=[0],thick=15,max_value=1.e15	;,C_ANNOTATION=[clab]
        
        loadct,0
        index=where(y2d ge -45.)
        dum(index)=0./0.
index=where(dum lt -0.25)
;if index(0) ne -1L then dum(index)=200.
if index(0) ne -1L and zprof(lev) gt 60. then print,'JJA ',zprof(lev)
;       contour,dum,x,alat,levels=[zprof(lev)],color=-30+(float(kk)/nth2)*mcolor,/overplot,c_labels=[1],thick=15,max_value=1.e15,c_charsize=2,c_charthick=2	;,C_ANNOTATION=[clab]
        contour,dum,x,alat,levels=[-0.25],color=-30+(float(kk)/nth2)*mcolor,/overplot,c_labels=0,thick=15,max_value=1.e15	;,C_ANNOTATION=[clab]
        loadct,39
    endfor

    set_viewport,.1,.9,.3-cbaryoff,.3-cbaryoff+cbarydel
    !type=2^2+2^3+2^6
    nlvls=nth2
    level=reverse(zprof(zindex))
    col1=1+(findgen(nlvls)/nlvls)*mcolor
    imin=min(level)
    imax=max(level)
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],xtitle='Approximate Altitude (km)',/noeras,$
          xtickname=strcompress(long(level(0:-1:2)),/remove_all),$
          xticks=nlvls/2-1,charsize=1,color=0,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
      xbox=[x1,x1,x1+dx,x1+dx,x1]
      polyfill,xbox,ybox,color=col1(j)
      x1=x1+dx
    endfor

loadct,0
    set_viewport,.1,.9,.225-cbaryoff,.225-cbaryoff+cbarydel
    !type=2^2+2^3+2^6
    nlvls=nth2
    level=reverse(zprof(zindex))
    col1=-30+(findgen(nlvls)/nlvls)*mcolor
    index=where(col1 lt 0.)
    if index(0) ne -1L then col1(index)=0
    imin=min(level)
    imax=max(level)
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],/noeras,$
          xtickname=strcompress(string(long(level)),/remove_all),$
          xticks=nlvls-1,charsize=1,color=0,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
      xbox=[x1,x1,x1+dx,x1+dx,x1]
      polyfill,xbox,ybox,color=col1(j)
      x1=x1+dx
    endfor
loadct,39

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim polar_djf_jja_smidemax.ps -rotate -90 polar_djf_jja_smidemax.jpg'
;      spawn,'rm -f polar_djf_jja_smidemax.ps'
    endif

end
