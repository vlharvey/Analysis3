;
; NH Polar plot CIPS Daisies plus NOGAPS raw 1x1 temperature data
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
;
; restore CIPS procedures and functions
;
restore,'read_cips_file.sav
pth='/Volumes/Data/CIPS_data/Datfiles/cips_sci_3a_'
;
; LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
;
LIM=0.

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
      sday=string(FORMAT='(i3.3)',iday)
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
    if icount eq 0L then begin
       nlv=n_elements(p)
       nc=n_elements(alon)
       nr=n_elements(alat)
       z=fltarr(nlv)
       for k=0L,nlv-1L do z(k)=mean(z3d(*,*,k))
       rlev=0.0036000
;      print,z
;      print,p
;      read,'Enter desired pressure level ',rlev
       index=where(abs(rlev-p) eq min(abs(rlev-p)))
       lev=index(0)
       slev=string(FORMAT='(f6.4)',p(lev))+'hPa'
       zlev=string(FORMAT='(f4.1)',z(lev))+'km'
       x=fltarr(nc+1)
       x(0:nc-1)=alon(0:nc-1)
       x(nc)=alon(0)+360.
       icount=1
    endif
;
; NOGAPS temperature
;
     z=fltarr(nlv)
     for k=0L,nlv-1L do z(k)=mean(z3d(*,*,k))
     zindex=where(z ge 80. and z le 87.)
     print,'# alts ',n_elements(zindex)

     temp1=fltarr(nc,nr)
     for i=0L,nc-1 do begin
     for j=0L,nr-1 do begin
         temp1(i,j)=mean(tp3d(i,j,zindex))
     endfor
     endfor
     temp=fltarr(nc+1,nr)
     temp(0:nc-1,*)=temp1(0:nc-1,*)
     temp(nc,*)=temp(0,*)
;
; read CIPS daisy
;
    spawn,'ls '+pth+syr+'-'+sday+'_v04.20_r04.nc',fname
    if fname(0) eq '' then stop,'missing '+syr+'-'+sday
    data = read_cips_file(fname)
;
; extract variables from data structure
;
    clat=data.latitude
    clon=data.longitude
    ut_date=data.ut_date
    hemisphere=data.hemisphere
    albedo=data.albedo
    result=size(clat)
    cnc=result(1)
    cnr=result(2)
;
; save postscript version
;
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_nh_nogaps_temps+daisies_'+sdate+'_'+slev+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif

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

     erase
     !type=2^2+2^3
     xmn=xorig(0)
     xmx=xorig(0)+xlen
     ymn=yorig(0)
     ymx=yorig(0)+ylen
     set_viewport,xmn,xmx,ymn,ymx
; LIMIT has the form [Latmin, Lonmin, Latmax, Lonmax]
     map_set,90,0,0,/ortho,/contin,/grid,color=0,/noeras,title='CIPS L3 V4.20 on '+strcompress(ut_date)+' + NOGAPS Temp (~'+zlev+')',limit=[50, 0, 90, 360]
     level=[0.001,0.01,.1,1.+2.*findgen(nlvls-1)]
     nlvls=n_elements(level)
     col1=1+indgen(nlvls)*mcolor/nlvls
     bad=where(finite(albedo) eq 0 or albedo eq 0.)
     if bad(0) ne -1L then albedo(bad)=0./0.
     good=where(finite(albedo) eq 1)
     if good(0) ne -1L then print,ut_date,' Max Albedo = ',max(albedo(good))
     contour,albedo,clon,clat,/fill,levels=level,/noeras,/overplot,c_color=col1
     contour,temp,x,alat,levels=[140],color=0.3*mcolor,/follow,/noeras,c_labels=1,thick=15,/overplot
     contour,temp,x,alat,levels=[135],color=0.2*mcolor,/follow,/noeras,c_labels=1,thick=15,/overplot
     contour,temp,x,alat,levels=[130],color=0.1*mcolor,/follow,/noeras,c_labels=1,thick=15,/overplot
     contour,temp,x,alat,levels=[125],color=0,/follow,/noeras,c_labels=1,thick=15,/overplot

     imin=min(level)
     imax=max(level)
     ymnb=ymn -cbaryoff
     ymxb=ymnb+cbarydel
     set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
     !type=2^2+2^3+2^6
     index=where(level ge 1.)
     slevel=['0.001','0.01','0.1',strcompress(long(level(index)),/remove_all)]
     xtickindex=where(level gt 0,nxticks)
     plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xticks=nxticks-1,xtickname=slevel(xtickindex),xtitle='Albedo (x10-6 sr-1)'
     ybox=[0,10,10,0,0]
     x2=imin
     dx=(imax-imin)/(float(nlvls)-1)
     for j=1,nlvls-1 do begin
         xbox=[x2,x2,x2+dx,x2+dx,x2]
         polyfill,xbox,ybox,color=col1(j)
         x2=x2+dx
     endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim polar_nh_nogaps_temps+daisies_'+sdate+'_'+slev+'.ps -rotate -90 polar_nh_nogaps_temps+daisies_'+sdate+'_'+slev+'.jpg'
       spawn,'rm -f polar_nh_nogaps_temps+daisies_'+sdate+'_'+slev+'.ps'
    endif
goto,jump
end
