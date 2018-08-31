;
; SmidEmax 300 years
; monthly mean of daily average data 
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
xorig=[0.15,0.55,0.15,0.55,0.15,0.55]
yorig=[0.7,0.7,0.4,0.4,0.1,0.1]
xlen=0.225
ylen=0.225
cbaryoff=0.1
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
        filenames=file_search(dir+syear+smon+'??.nc3')
        if filenames(0) eq '' then goto,skipmonth
        ofile=dir+syear+smon+'.sav'
        dum=file_search(ofile)
        if dum(0) ne '' then goto,skipmonth
;
; loop over daily data
;
        nfile=n_elements(filenames)
        for ifile=0L,nfile-1L do begin
            print,filenames(ifile)
            ncfile0=filenames(ifile)
            dum=findfile(ncfile0)
;           if dum(0) eq '' then goto,jump
            ncid=ncdf_open(ncfile0)
            result0=ncdf_inquire(ncid)
            for idim=0,result0.ndims-1 do begin
                ncdf_diminq,ncid,idim,name,dim
                if name eq 'number_of_latitudes' then nr=dim
                if name eq 'number_of_longitudes' then nc=dim
                if name eq 'number_of_levels' then nth=dim
;               print,'read ',name,' dimension ',dim
            endfor
            for ivar=0,result0.nvars-1 do begin
                result=ncdf_varinq(ncid,ivar)
                ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
                if result.name eq 'latitude' then alat=data
                if result.name eq 'longitude' then alon=data
                if result.name eq 'theta' then th=data
                if result.name eq 'IPV' then ipvavg=data
                if result.name eq 'P' then pavg=data
                if result.name eq 'U' then uavg=data
                if result.name eq 'V' then vavg=data
                if result.name eq 'QDF' then qdfavg=data
                if result.name eq 'CO' then coavg=data
                if result.name eq 'GPH' then zavg=data/1000.
                if result.name eq 'SF' then sfavg=data
                if result.name eq 'MARK' then mavg=data
;               print,ivar,result.name,min(data),max(data)
            endfor
            ncdf_close,ncid

;           restore,filenames(ifile)	; alon,alat,th,nc,nr,nth,ipvavg,pavg,uavg,vavg,qdfavg,zavg,sfavg,mavg
            if ifile eq 0L then begin
               ipvavg2=0.*ipvavg
               pavg2=0.*ipvavg
               uavg2=0.*ipvavg
               vavg2=0.*ipvavg
               qdfavg2=0.*ipvavg
               coavg2=0.*ipvavg
               zavg2=0.*ipvavg
               sfavg2=0.*ipvavg
               mavg2=0.*ipvavg
            endif
            ipvavg2=ipvavg2+ipvavg
            pavg2=pavg2+pavg
            uavg2=uavg2+uavg
            vavg2=vavg2+vavg
            qdfavg2=qdfavg2+qdfavg
            coavg2=coavg2+coavg
            zavg2=zavg2+zavg
            sfavg2=sfavg2+sfavg
            mavg2=mavg2+mavg
        endfor	; loop over files
;
; average
;
        ipvavg2=ipvavg2/float(nfile)
        pavg2=pavg2/float(nfile)
        uavg2=uavg2/float(nfile)
        vavg2=vavg2/float(nfile)
        qdfavg2=qdfavg2/float(nfile)
        coavg2=1.e6*coavg2/float(nfile)
        zavg2=zavg2/float(nfile)
        sfavg2=sfavg2/float(nfile)
        mavg2=mavg2/float(nfile)
;
; save monthly mean
;
        ofile=dir+syear+smon+'.sav'
        print,'saving '+ofile
        save,filename=ofile,nc,nr,nth,alon,alat,th,ipvavg2,pavg2,uavg2,vavg2,qdfavg2,coavg2,zavg2,sfavg2,mavg2
;
; postscript file
;
        if setplot eq 'ps' then begin
           lc=0
           xsize=nxdim/100.
           ysize=nydim/100.
           set_plot,'ps'
           device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                  /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_ubar+markbar_waccm_smidemax_'+syear+smon+'.ps'
           !p.charsize=1.25
           !p.thick=2
           !p.charthick=2
           !y.thick=2
           !x.thick=2
        endif
;
; calculate temperature
;
        tavg2=0.*pavg2
        for k=0,nth-1 do tavg2(*,*,k) = th(k)*( (pavg2(*,*,k)/1000.)^(.286) )
;
; calculate zonal means
;
        ipvyz=mean(ipvavg2,dim=2)
        for k=0L,nth-1L do ipvyz(*,k)=ipvyz(*,k)*((th(k)/2000.))^(-9./2.)
        pyz=mean(pavg2,dim=2)
        uyz=mean(uavg2,dim=2)
        vyz=mean(vavg2,dim=2)
        qdfyz=mean(qdfavg2,dim=2)
        coyz=mean(coavg2,dim=2)
        zyz=mean(zavg2,dim=2)
        sfyz=mean(sfavg2,dim=2)
        myz=mean(mavg2,dim=2)
        tyz=mean(tavg2,dim=2)
;
; plot
;
x2d=0.*tyz
for k=0L,nth-1L do x2d(*,k)=alat
erase
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
contour,tyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[-90,90],xtitle='Latitude',yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='WACCM '+syear+smon
contour,tyz,alat,zyz,/noera,/foll,color=0,levels=tlevel,/overplot
;myz=smooth(myz,7,/edge_truncate)
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=5,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(0)+0.01,yorig(0)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(tlevel)
imax=max(tlevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Temperature (K)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
ulevel=-100+10.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,uyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=ulevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
index=where(ulevel gt 0.)
contour,uyz,alat,zyz,/noera,/foll,color=0,levels=ulevel(index),/overplot
index=where(ulevel lt 0.)
contour,uyz,alat,zyz,/noera,/foll,color=mcolor,c_linestyle=5,levels=ulevel(index),/overplot
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=5,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(1)+0.01,yorig(1)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(ulevel)
imax=max(ulevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Ubar (m/s)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
vlevel=-25.+2.5*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,vyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=vlevel,xrange=[-90,90],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,xtitle='Latitude'
contour,vyz,alat,zyz,/noera,/foll,color=0,levels=vlevel,/overplot
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=2,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(2)+0.01,yorig(2)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(vlevel)
imax=max(vlevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Vbar (m/s)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
qlevel=-200.+20.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,qdfyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=qlevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
contour,qdfyz,alat,zyz,/noera,/foll,color=0,levels=qlevel,/overplot
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=2,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(3)+0.01,yorig(3)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(qlevel)
imax=max(qlevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='QDF (m2/s)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
ipvyz=ipvyz/1000.
pvlevel=-200.+20*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(4)
xmx=xorig(4)+xlen
ymn=yorig(4)
ymx=yorig(4)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,ipvyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=pvlevel,xrange=[-90,90],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,xtitle='Latitude'
contour,ipvyz,alat,zyz,/noera,/foll,color=0,levels=pvlevel,/overplot
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=2,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(4)+0.01,yorig(4)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(pvlevel)
imax=max(pvlevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Laits PV (PVU)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
colevel=2.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(5)
xmx=xorig(5)+xlen
ymn=yorig(5)
ymx=yorig(5)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,coyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=colevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
contour,coyz,alat,zyz,/noera,/foll,color=0,levels=colevel,/overplot
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=5,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=2,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(5)+0.01,yorig(5)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(colevel)
imax=max(colevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='CO (ppmv)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

;
; Close PostScript file and return control to X-windows
;
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim YZ_figures/yz_ubar+markbar_waccm_smidemax_'+syear+smon+'.ps -rotate -90 YZ_figures/yz_ubar+markbar_waccm_smidemax_'+syear+smon+'.jpg'
   spawn,'rm -f YZ_figures/yz_ubar+markbar_waccm_smidemax_'+syear+smon+'.ps'
endif

skipmonth:
endfor	; loop over months
endfor	; loop over years
end
