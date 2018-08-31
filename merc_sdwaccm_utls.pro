;
; SD-WACCM
; daily mercator plots in the UTLS
;
@stddat
@kgmt
@ckday
@kdate
@rd_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=800
nydim=800
cbaryoff=0.065
cbarydel=0.02
lstmn=6
lstdy=1
lstyr=2013
ledmn=8
leddy=31
ledyr=2013
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
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'

; Compute initial Julian date
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount=0L

; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then stop,' Normal termination condition '
      syr=strcompress(iyr,/remove_all)
      smn=string(FORMAT='(i2.2)',imn)
      sdy=string(FORMAT='(i2.2)',idy)
      sdate=syr+smn+sdy

      ifile=dirw+'sdwaccm2012-2014_1_2_2.cam.h1.'+sdate+'_utls.nc3'
      rd_sdwaccm4_nc3,ifile,nc,nr,nth,alon,alat,th,$
         pv2,p2,z2,u2,v2,q2,qdf2,mark2,sf2,h2o2,n2o2,o32,iflg
      if iflg eq 1 then goto,jump
      tmp2=0.*p2
      for k=0L,nth-1L do tmp2(*,*,k)=th(k)*(p2(*,*,k)/1000.)^0.286

; select theta levels to plot
    if icount eq 0L then begin
       rth=360.
       print,th
       read,'Enter desired theta surface ',rth
       index=where(th eq rth)
       lev=index(0)
       sth=strcompress(long(th(lev)),/r)
       icount=1L
       x2d=fltarr(nc,nr)
       y2d=fltarr(nc,nr)
       for i=0,nc-1 do y2d(i,*)=alat
       for j=0,nr-1 do x2d(*,j)=alon
    endif

; save postscript version
    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='sdwaccm_merc_'+sdate+'_'+sth+'.ps'
       device,/color
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
       !p.thick=2.0                   ;Plotted lines twice as thick
       !p.charsize=2.0
    endif
;
; strip out theta level
;
    mark1=transpose(mark2(*,*,lev))
    sf1=transpose(sf2(*,*,lev))
    pv1=transpose(pv2(*,*,lev))
    p1=transpose(p2(*,*,lev))
    mpv1=pv1*((th(lev)/300.))^(-9./2.)
    h2o1=transpose(h2o2(*,*,lev))*1.e6
    temp1=th(lev)*(p1/1000.)^.286
    zth=transpose(z2(*,*,lev))/1000.
;
; layer means
;
    result=moment(zth)
    avgz=result(0)
    savgz=strcompress(string(fix(avgz)),/remove_all)
    result=moment(p1)
    avgp=result(0)
    savgp=strcompress(string(FORMAT='(F7.3)',avgp))
;
; plot
;
    erase
    set_viewport,.1,.9,.2,.8
    MAP_SET,0,180,0,/contin,/grid,/noeras,londel=90.,label=1,lonlab=1,charsize=2,latdel=180.,color=0,title=sdate+' '+sth+' K',limit=[0.,0.,90.,360.]
    nlvls=20L
;   level=min(h2o1)+((max(h2o1)-min(h2o1))/float(nlvls))*findgen(nlvls)
    level=[1,2,3,5,10,20,30,40,50,75,100]
nlvls=n_elements(level)
    col1=1+(indgen(nlvls)/float(nlvls))*mcolor
    contour,h2o1,alon,alat,levels=level,/overplot,/cell_fill,c_color=col1
    contour,h2o1,alon,alat,levels=level,/overplot,/follow,color=0
    MAP_SET,0,180,0,/contin,/grid,/noeras,londel=90.,label=1,lonlab=1,charsize=2,latdel=180.,color=mcolor,limit=[0.,0.,90.,360.]
;sf1=pv1
    smin=0.	;min(sf1)
    smax=max(sf1)
    sint=(smax-smin)/nlvls
    sflevel=smin+sint*findgen(nlvls)
    contour,sf1,alon,alat,nlevels=40,color=0,c_labels=0+0.*sflevel,thick=3,/overplot
;   contour,mark1,alon,alat,/overplot,levels=[0.1],thick=5,color=mcolor,/follow
;   contour,mark1,alon,alat,/overplot,levels=[-0.1],thick=10,color=0,/follow,c_labels=[0]
;   index=where(mark1 gt 0.)
;   if index(0) ne -1L then oplot,x2d(index),y2d(index),psym=8,color=0,symsize=0.5
;   index=where(mark1 lt 0.)
;   if index(0) ne -1L then oplot,x2d(index),y2d(index),psym=8,color=mcolor,symsize=0.5

    xmnb=0.1
    xmxb=0.9
    ymnb=0.17
    ymxb=0.18
    set_viewport,xmnb,xmxb,ymnb,ymxb
    !type=2^2+2^3+2^6
    imin=min(level)
    imax=max(level)
    plot,[imin,imax],[0,0],yrange=[0,10],$
          xrange=[imin,imax],xtitle='Water Vapor (ppmv)',/noeras,$
          xtickname=strcompress(string(fix(level)),/remove_all),$
          xstyle=1,xticks=nlvls-1,charsize=1.5,color=0,charthick=2
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
       spawn,'convert -trim sdwaccm_merc_'+sdate+'_'+sth+'.ps -rotate -90 sdwaccm_merc_'+sdate+'_'+sth+'.jpg'
       spawn,'rm -f sdwaccm_merc_'+sdate+'_'+sth+'.ps
    endif

goto, jump

end
