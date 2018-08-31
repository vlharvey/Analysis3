;
; SmidEmax 300 years
; monthly mean of daily average data 
; pressure data
;
loadct,39
mcolor=byte(!p.color)
icmm1=mcolor-1B
icmm2=mcolor-2B
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.15,0.55,0.15,0.55]
yorig=[0.6,0.6,0.15,0.15]
xlen=0.3
ylen=0.3
cbaryoff=0.07
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)
;
; loop over years and months
;
for iyear=1,300 do begin
    syear=string(FORMAT='(I3.3)',iyear)
    for imonth=0L,nmonth-1L do begin
        imon=imonth+1
        smon=string(FORMAT='(I2.2)',imon)
        filenames=file_search(dir+syear+smon+'??_vE.sav')
;       if filenames(0) eq '' then goto,skipmonth
        ofile=dir+syear+smon+'_prs.sav'
        dum=file_search(ofile)
;       if dum(0) ne '' then goto,skipmonth
;
; loop over daily data
;
        nfile=n_elements(filenames)
        for ifile=0L,nfile-1L do begin
            print,filenames(ifile)
            ncfile0=filenames(ifile)
            dum=findfile(ncfile0)
;           if dum(0) eq '' then goto,jump
;
; COGRD           FLOAT     = Array[144, 96, 66]
; LAT             FLOAT     = Array[96]
; LEV             FLOAT     = Array[66]
; LON             FLOAT     = Array[144]
; TGRD            FLOAT     = Array[144, 96, 66]
; UGRD            FLOAT     = Array[144, 96, 66]
; VGRD            FLOAT     = Array[144, 96, 66]
; ZGRD            FLOAT     = Array[144, 96, 66]
;
            restore,ncfile0
            nr=n_elements(lat)
            nc=n_elements(lon)
            nl=n_elements(lev)
            alat=lat
            alon=lon

            if ifile eq 0L then begin
               coavg=0.*tgrd
               zavg=0.*tgrd
               tavg=0.*tgrd
               uavg=0.*tgrd
               vavg=0.*tgrd
            endif
            coavg=coavg+cogrd
            zavg=zavg+zgrd/1000.
            tavg=tavg+tgrd
            uavg=uavg+ugrd
            vavg=vavg+vgrd
        endfor	; loop over files
;
; average
;
        coavg=1.e6*coavg/float(nfile)
        zavg=zavg/float(nfile)
        tavg=tavg/float(nfile)
        uavg=uavg/float(nfile)
        vavg=vavg/float(nfile)
;
; save monthly mean
;
        ofile=dir+syear+smon+'_prs.sav'
        print,'saving '+ofile
        save,filename=ofile,nc,nr,nl,alon,alat,lev,coavg,zavg,tavg,uavg,vavg
;
; postscript file
;
        if setplot eq 'ps' then begin
           lc=0
           xsize=nxdim/100.
           ysize=nydim/100.
           set_plot,'ps'
           device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                  /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_ubar+tbar_waccm_smidemax_'+syear+smon+'.ps'
           !p.charsize=1.25
           !p.thick=2
           !p.charthick=2
           !y.thick=2
           !x.thick=2
        endif
;
; calculate zonal means
;
        uyz=mean(uavg,dim=1)
        vyz=mean(vavg,dim=1)
        coyz=mean(coavg,dim=1)
        zyz=mean(zavg,dim=1)
        tyz=mean(tavg,dim=1)
;
; plot
;
x2d=0.*tyz
for k=0L,nl-1L do x2d(*,k)=alat
erase
xyouts,.4,.95,'WACCM '+syear+smon,/normal,charsize=2,charthick=2,color=0
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlvls=27
tlevel=[130+5*findgen(nlvls),280,300,320,350,400,450,500]
nlvls=n_elements(tlevel)
col1=(findgen(nlvls)/float(nlvls))*mcolor
contour,tyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[-90,90],xtitle='Latitude',yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2
contour,tyz,alat,zyz,/noera,/foll,color=0,levels=tlevel,/overplot
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(tlevel)
imax=max(tlevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Temperature (K)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

nlvls=21
colevel=2.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,coyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=colevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
contour,coyz,alat,zyz,/noera,/foll,color=0,levels=colevel,/overplot
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(colevel)
imax=max(colevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='CO (ppmv)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

nlvls=21
ulevel=-100+10.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,uyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=ulevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude',ytitle='Altitude (km)'
index=where(ulevel gt 0.)
contour,uyz,alat,zyz,/noera,/foll,color=0,levels=ulevel(index),/overplot
index=where(ulevel lt 0.)
contour,uyz,alat,zyz,/noera,/foll,color=mcolor,c_linestyle=5,levels=ulevel(index),/overplot
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(ulevel)
imax=max(ulevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Ubar (m/s)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

nlvls=21
vlevel=-25.+2.5*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,vyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=vlevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
contour,vyz,alat,zyz,/noera,/foll,color=0,levels=vlevel,/overplot
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(vlevel)
imax=max(vlevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle='Vbar (m/s)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
;
; Close PostScript file and return control to X-windows
;
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim YZ_figures/yz_ubar+tbar_waccm_smidemax_'+syear+smon+'.ps -rotate -90 YZ_figures/yz_ubar+tbar_waccm_smidemax_'+syear+smon+'.jpg'
   spawn,'rm -f YZ_figures/yz_ubar+tbar_waccm_smidemax_'+syear+smon+'.ps'
endif

skipmonth:
endfor	; loop over months
endfor	; loop over years
end
