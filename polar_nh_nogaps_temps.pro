;
; NH Polar plot of NOGAPS raw 1x1 temperature data
;
@stddat
@kgmt
@ckday
@kdate
@read_nav

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,2*cos(a),2*sin(a),/fill
nxdim=750
nydim=750
xorig=[0.20]
yorig=[0.20]
xlen=0.7
ylen=0.7
cbaryoff=0.02
cbarydel=0.02
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
lstmn=6 & lstdy=25 & lstyr=2007
ledmn=7 & leddy=5 & ledyr=2007
ledmn=6 & leddy=25 & ledyr=2007
lstday=0 & ledday=0
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
if lstyr ge 91 and lstyr le 99 then lstyr=lstyr+1900
if ledyr ge 91 and ledyr le 99 then ledyr=ledyr+1900
if lstyr lt 91 then lstyr=lstyr+2000
if ledyr lt 91 then ledyr=ledyr+2000
if lstyr lt 2007 then stop,'Year out of range '
if ledyr lt 2007 then stop,'Year out of range '
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
;
; change to be the full path of where your data resides
;
dir='/Volumes/earth/harvey/NOGAPS_Alpha/Datfiles/'
!noeras=-1
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
;
; NOGAPS-Alpha variables
;
svars=['EPV','H2O','O3','T','U','V','Z']
nvars=n_elements(svars)
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
;
; loop over days
;
icount=0
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then stop,' Normal Termination Condition '
      syr=string(FORMAT='(i4)',iyr)
      smn=string(FORMAT='(i2.2)',imn)
      sdy=string(FORMAT='(i2.2)',idy)
      sdate=syr+smn+sdy
      print,sdate
;
; loop over variables
;
        for ivar=0L,nvars-1L do begin
;
; build filenames and read binary data
;
            filename=dir+svars(ivar)+'_'+sdate+'12_360x181x60_aim9c.NAV'
            dum=findfile(filename)
            if dum(0) eq '' then begin
               print,'Missing '+sdate+' '+svars(ivar)
               goto,jump
            endif
            data=read_NAV_macd88(filename,lat=alat,lon=alon,p=p,yyyymmddhh=yyyymmddhh)
            print,filename+' '+svars(ivar),' ',min(data),max(data)
;
; retain variables
;
            if svars(ivar) eq 'EPV' then pv3d=data
            if svars(ivar) eq 'H2O' then h2o3d=data
            if svars(ivar) eq 'O3' then o33d=data
            if svars(ivar) eq 'T' then tp3d=data
            if svars(ivar) eq 'U' then u3d=data
            if svars(ivar) eq 'V' then v3d=data
            if svars(ivar) eq 'Z' then z3d=data/1000.
        endfor  ; loop over variables
        nlv=n_elements(p)
        nc=n_elements(alon)
        nr=n_elements(alat)
    z=fltarr(nlv)
    for k=0L,nlv-1L do z(k)=mean(z3d(*,*,k))
    rlev=0.
    print,z
    print,p
    read,'Enter desired pressure level ',rlev
    index=where(abs(rlev-p) eq min(abs(rlev-p)))
    lev=index(0)
    slev=string(FORMAT='(f6.4)',p(lev))+'hPa'
    zlev=string(FORMAT='(f4.1)',z(lev))+'km'

    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.
;
; save postscript version
;
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_nh_nogaps_temps'+sdate+'_'+slev+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

; temperature
     temp1=tp3d(*,*,lev)
     temp=fltarr(nc+1,nr)
     temp(0:nc-1,*)=temp1(0:nc-1,*)
     temp(nc,*)=temp(0,*)

     nlvls=21
     col1=1+indgen(nlvls)*mcolor/nlvls
;
; for variable contour levels to make a plot pretty
;
     imin=min(temp) & imax=max(temp)
     level=imin+((imax-imin)/float(nlvls))*findgen(nlvls)
;
; for constant contour levels so you can see warming and cooling
;
;    level=180.+5.*findgen(nlvls)

     !type=2^2+2^3
     xyouts,.3,.93,sdate+'  '+slev+' (~'+zlev+')',/normal,charsize=2,color=0
     xmn=xorig(0)
     xmx=xorig(0)+xlen
     ymn=yorig(0)
     ymx=yorig(0)+ylen
     set_viewport,xmn,xmx,ymn,ymx
     MAP_SET,90,0,0,/ortho,/contin,/grid,/noeras
     contour,temp,x,alat,levels=level,color=0,/cell_fill,c_color=col1,/noeras,c_labels=0,/overplot
     contour,temp,x,alat,levels=level,color=0,/follow,/noeras,c_labels=1+0*level,/overplot
     contour,temp,x,alat,levels=[140],color=mcolor,/follow,/noeras,c_labels=1,thick=4,/overplot

    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    imin=min(level)
    imax=max(level)
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],xtitle='NOGAPS Temperature',/noeras,$
          charsize=1.5,color=0
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
       spawn,'convert -trim polar_nh_nogaps_temps'+sdate+'_'+slev+'.ps -rotate -90 polar_nh_nogaps_temps'+sdate+'_'+slev+'.jpg'
;      spawn,'/usr/bin/rm -f polar_nh_nogaps_temps'+sdate+'_'+slev+'.ps'
    endif
goto,jump
end
