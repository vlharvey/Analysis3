;
; plot mercator SDWACCM T, PV, O3, H2O from nc3 files
;
@stddat
@kgmt
@ckday
@kdate
@rd_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.05,0.55,0.05,0.55]
yorig=[0.6,0.6,0.15,0.15]
xlen=0.4
ylen=0.3
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
lstmn=7
lstdy=1
lstyr=2013
ledmn=7
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
if lstyr lt 2000 then lstyr=lstyr+2000
if ledyr lt 2000 then ledyr=ledyr+2000
if lstyr lt 2004 then stop,'Year out of range '
if ledyr lt 2004 then stop,'Year out of range '
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
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
;
; loop over theta
; 
;   for k=5L,9 do begin	;nth-1L do begin
    for k=9,9 do begin
        rlev=th(k)
        slev=strcompress(long(rlev),/r)
;
; save postscript file
;
        if setplot eq 'ps' then begin
           set_plot,'ps'
           xsize=nxdim/100.
           ysize=nydim/100.
           !psym=0
           !p.font=0
           device,font_size=9
           device,/landscape,bits=8,filename='merc_sdwaccm_'+sdate+'_'+slev+'_4pan.ps'
           device,/color
           device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                  xsize=xsize,ysize=ysize
           !p.thick=2.0                   ;Plotted lines twice as thick
           !p.charsize=2.0
        endif

        pv1=transpose(pv2(*,*,k))
        p1=transpose(p2(*,*,k))
        mpv1=pv1*((th(k)/300.))^(-9./2.)
        u1=transpose(u2(*,*,k))
        v1=transpose(v2(*,*,k))
        qdf1=transpose(qdf2(*,*,k))
        mark1=transpose(mark2(*,*,k))
        sf1=transpose(sf2(*,*,k))
        h2o1=transpose(h2o2(*,*,k))
        o31=transpose(o32(*,*,k))

        mpv=fltarr(nc+1,nr)
        mpv(0:nc-1,0:nr-1)=mpv1(0:nc-1,0:nr-1)
        mpv(nc,*)=mpv(0,*)
        pv=fltarr(nc+1,nr)
        pv(0:nc-1,0:nr-1)=pv1(0:nc-1,0:nr-1)
        pv(nc,*)=pv(0,*)
        p=fltarr(nc+1,nr)
        p(0:nc-1,0:nr-1)=p1(0:nc-1,0:nr-1)
        p(nc,*)=p(0,*)
        tp=th(k)*(p/1000.)^0.286
        u=fltarr(nc+1,nr)
        u(0:nc-1,0:nr-1)=u1(0:nc-1,0:nr-1)
        u(nc,*)=u(0,*)
        v=fltarr(nc+1,nr)
        v(0:nc-1,0:nr-1)=v1(0:nc-1,0:nr-1)
        v(nc,*)=v(0,*)
        qdf=fltarr(nc+1,nr)
        qdf(0:nc-1,0:nr-1)=qdf1(0:nc-1,0:nr-1)
        qdf(nc,*)=qdf(0,*)
        mark=fltarr(nc+1,nr)
        mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
        mark(nc,*)=mark(0,*)
        sf=0.*fltarr(nc+1,nr)
        sf(0:nc-1,0:nr-1)=sf1(0:nc-1,0:nr-1)
        sf(nc,*)=sf(0,*)
        h2o=0.*fltarr(nc+1,nr)
        h2o(0:nc-1,0:nr-1)=h2o1(0:nc-1,0:nr-1)
        h2o(nc,*)=h2o(0,*)
        o3=0.*fltarr(nc+1,nr)
        o3(0:nc-1,0:nr-1)=o31(0:nc-1,0:nr-1)
        o3(nc,*)=o3(0,*)
        speed=sqrt(u*u+v*v)
        x2d=0.*sf
        y2d=0.*sf
        x=fltarr(nc+1)
        x(0:nc-1)=alon(0:nc-1)
        x(nc)=alon(0)+360.
        for i=0,nc do y2d(i,*)=alat
        for j=0,nr-1 do x2d(*,j)=x
;
; plot
;
erase
xyouts,.2,.95,'SD-WACCM          '+sdate+' '+string(long(th(k)))+' K',/normal,charsize=2,charthick=2,color=0
        xmn=xorig(0)
        xmx=xorig(0)+xlen
        ymn=yorig(0)
        ymx=yorig(0)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        !type=2^2+2^3
        pp=pv*1.e6
        nlvls=20L
        col1=1+(indgen(nlvls)/float(nlvls))*mcolor
        nhindex=where(y2d gt 0)
        level=min(pp(nhindex))+((max(pp(nhindex))-min(pp(nhindex)))/float(nlvls))*findgen(nlvls)
;       if max(pp) eq min(pp) then level=findgen(10)
        contour,pp,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],yrange=[0.,90],$
                charsize=1.5,color=0,xticks=4,yticks=6,charthick=2
        contour,pp,x,alat,levels=level,/follow,color=0,/overplot,c_labels=0*level
        contour,pp,x,alat,levels=[4],/follow,color=mcolor*.9,thick=6,/overplot,c_labels=0*level
        pp2=sf
        level2=min(pp2)+((max(pp2)-min(pp2))/float(10))*findgen(10)
        contour,pp2,x,alat,levels=level2,/noeras,/overplot,thick=2
        map_set,0,180,0,/contin,/grid,/noeras,limit=[0.,0.,90.,360.]
        index=where(mark gt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=2,color=.2*mcolor
        contour,mark,x,alat,levels=[0.1],/overplot,thick=5,color=mcolor
        index=where(mark lt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=.9*mcolor
        contour,mark,x,alat,levels=[-0.1],/overplot,thick=5,color=mcolor
        imin=min(level)
        imax=max(level)
        ymnb=yorig(0) -0.06
        ymxb=ymnb  +0.01
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='PV (PVU)',/noeras,charthick=2
        ybox=[0,10,10,0,0]
        x1=imin
        dxx=(imax-imin)/float(nlvls)
        for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
        endfor

        xmn=xorig(1)
        xmx=xorig(1)+xlen
        ymn=yorig(1)
        ymx=yorig(1)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        !type=2^2+2^3
        pp=tp
        nlvls=20L
        col1=1+(indgen(nlvls)/float(nlvls))*mcolor
        level=min(pp(nhindex))+((max(pp(nhindex))-min(pp(nhindex)))/float(nlvls))*findgen(nlvls)
        if max(pp) eq min(pp) then level=findgen(10)
        contour,pp,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],yrange=[0.,90],$
                charsize=1.5,color=0,xticks=4,yticks=6,charthick=2
        contour,pp,x,alat,levels=level,/follow,color=0,/overplot,c_labels=0*level
        contour,pp2,x,alat,levels=level2,/noeras,/overplot,thick=2
        map_set,0,180,0,/contin,/grid,/noeras,limit=[0.,0.,90.,360.]
;       index=where(mark gt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=2,color=.2*mcolor
        contour,mark,x,alat,levels=[0.1],/overplot,thick=5,color=mcolor
;       index=where(mark lt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=.9*mcolor
        contour,mark,x,alat,levels=[-0.1],/overplot,thick=5,color=mcolor
        imin=min(level)
        imax=max(level)
        ymnb=yorig(1) -0.06
        ymxb=ymnb  +0.01
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='Temperature (K)',/noeras,charthick=2
        ybox=[0,10,10,0,0]
        x1=imin
        dxx=(imax-imin)/float(nlvls)
        for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
        endfor

        xmn=xorig(2)
        xmx=xorig(2)+xlen
        ymn=yorig(2)
        ymx=yorig(2)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        !type=2^2+2^3
        pp=o3*1.e6
        nlvls=20L
        col1=1+(indgen(nlvls)/float(nlvls))*mcolor
        level=min(pp(nhindex))+((max(pp(nhindex))-min(pp(nhindex)))/float(nlvls))*findgen(nlvls)
        if max(pp) eq min(pp) then level=findgen(10)
        contour,pp,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],yrange=[0.,90],$
                charsize=1.5,color=0,xticks=4,yticks=6,charthick=2
;       contour,pp,x,alat,levels=level,/follow,color=0,/overplot,c_labels=0*level
        contour,pp2,x,alat,levels=level2,/noeras,/overplot,thick=2
        map_set,0,180,0,/contin,/grid,/noeras,limit=[0.,0.,90.,360.]
;       index=where(mark gt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=2,color=.2*mcolor
        contour,mark,x,alat,levels=[0.1],/overplot,thick=5,color=mcolor
;       index=where(mark lt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=.9*mcolor
        contour,mark,x,alat,levels=[-0.1],/overplot,thick=5,color=mcolor
        imin=min(level)
        imax=max(level)
        ymnb=yorig(2) -0.06
        ymxb=ymnb  +0.01
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='Ozone (ppmv)',/noeras,charthick=2
        ybox=[0,10,10,0,0]
        x1=imin
        dxx=(imax-imin)/float(nlvls)
        for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
        endfor

        xmn=xorig(3)
        xmx=xorig(3)+xlen
        ymn=yorig(3)
        ymx=yorig(3)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        !type=2^2+2^3
        pp=h2o*1.e6
        nlvls=20L
        col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;       level=min(pp)+((max(pp)-min(pp))/float(nlvls))*findgen(nlvls)
        level=findgen(nlvls)
        contour,pp,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,xrange=[0.,360.],yrange=[0.,90],$
                charsize=1.5,color=0,xticks=4,yticks=6,charthick=2
;       contour,pp,x,alat,levels=level,/follow,color=0,/overplot,c_labels=0*level
        contour,pp2,x,alat,levels=level2,/noeras,/overplot,thick=2
        map_set,0,180,0,/contin,/grid,/noeras,limit=[0.,0.,90.,360.]
;       index=where(mark gt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=2,color=.2*mcolor
        contour,mark,x,alat,levels=[0.1],/overplot,thick=5,color=mcolor
;       index=where(mark lt 0.)
;       if index(0) ne -1 then oplot,x2d(index),y2d(index),psym=4,color=.9*mcolor
        contour,mark,x,alat,levels=[-0.1],/overplot,thick=5,color=mcolor
        imin=min(level)
        imax=max(level)
        ymnb=yorig(3) -0.06
        ymxb=ymnb  +0.01
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='Water Vapor (ppmv)',/noeras,charthick=2
        ybox=[0,10,10,0,0]
        x1=imin
        dxx=(imax-imin)/float(nlvls)
        for jj=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dxx,x1+dxx,x1]
        polyfill,xbox,ybox,color=col1(jj)
        x1=x1+dxx
        endfor



    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim merc_sdwaccm_'+sdate+'_'+slev+'_4pan.ps -rotate -90 merc_sdwaccm_'+sdate+'_'+slev+'_4pan.jpg'
;      spawn,'rm -f merc_sdwaccm_'+sdate+'_'+slev+'_4pan.ps'
    endif

endfor	; loop over theta
goto,jump
end
