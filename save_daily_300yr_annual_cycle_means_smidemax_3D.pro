;
; save daily 3d average of 
; SmidEmax 300 years
; i.e., Jan 1 is the average of ALL January 1sts.
; save 3D means and sigmas
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
; build MMDD dates
;
year1files=file_search(dir+'001????.nc3')
ndays=n_elements(year1files)
mmdd=strarr(ndays)
for ii=0L,ndays-1L do begin
    dum=strsplit(year1files(ii),'.',/extract)
    dum2=strsplit(dum(0),'_',/extract)
    mmdd(ii)=strmid(dum2(-1),3,4)
endfor
;
; loop over days of the year
;
for iday=0,ndays-1 do begin
    print,mmdd(iday)
    filenames=file_search(dir+'???'+mmdd(iday)+'.nc3')
    ofile=dir+mmdd(iday)+'.sav'
    dum=file_search(ofile)
;   if dum(0) ne '' then goto,skipday
;
; loop over all years for this day (300 January 1sts)
;
    nfile=n_elements(filenames)
    if nfile ne 300 then stop,'Is there missing data for this day?'
    for ifile=0L,nfile-1L do begin
        ncfile0=filenames(ifile)
        dum=findfile(ncfile0)
;       if dum(0) eq '' then goto,jump
        ncid=ncdf_open(ncfile0)
        result0=ncdf_inquire(ncid)
        for idim=0,result0.ndims-1 do begin
            ncdf_diminq,ncid,idim,name,dim
            if name eq 'number_of_latitudes' then nr=dim
            if name eq 'number_of_longitudes' then nc=dim
            if name eq 'number_of_levels' then nth=dim
;           print,'read ',name,' dimension ',dim
        endfor
        for ivar=0,result0.nvars-1 do begin
            result=ncdf_varinq(ncid,ivar)
            ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
            if result.name eq 'latitude' then alat=data
            if result.name eq 'longitude' then alon=data
            if result.name eq 'theta' then th=data
            if result.name eq 'IPV' then ipv=data
            if result.name eq 'P' then p=data
            if result.name eq 'U' then u=data
            if result.name eq 'V' then v=data
            if result.name eq 'QDF' then qdf=data
            if result.name eq 'CO' then co=data*1.e6
            if result.name eq 'GPH' then z=data/1000.
            if result.name eq 'SF' then sf=data
            if result.name eq 'MARK' then mark=data
;           print,ivar,result.name,min(data),max(data)
        endfor
        ncdf_close,ncid
        index=where(mark lt 0.)
        if index(0) ne -1L then mark(index)=-1.
;
; calculate temperature
;
        tp=0.*p
        for k=0,nth-1 do tp(*,*,k) = th(k)*( (p(*,*,k)/1000.)^(.286) )

        if ifile eq 0L then begin
           ipvavg=0.*ipv
           pavg=0.*ipv
           uavg=0.*ipv
           vavg=0.*ipv
           qdfavg=0.*ipv
           coavg=0.*ipv
           zavg=0.*ipv
           sfavg=0.*ipv
           mavg=0.*ipv
           tavg=0.*ipv
           ipvall=fltarr(nfile,nr,nc,nth)
           pall=fltarr(nfile,nr,nc,nth)
           uall=fltarr(nfile,nr,nc,nth)
           vall=fltarr(nfile,nr,nc,nth)
           qdfall=fltarr(nfile,nr,nc,nth)
           coall=fltarr(nfile,nr,nc,nth)
           zall=fltarr(nfile,nr,nc,nth)
           sfall=fltarr(nfile,nr,nc,nth)
           mall=fltarr(nfile,nr,nc,nth)
           tall=fltarr(nfile,nr,nc,nth)
        endif
        ipvavg=ipvavg+ipv
        pavg=pavg+p
        uavg=uavg+u
        vavg=vavg+v
        qdfavg=qdfavg+qdf
        coavg=coavg+co
        zavg=zavg+z
        sfavg=sfavg+sf
        mavg=mavg+mark
        tavg=tavg+tp
;
; retain all daily grids for sigma calculation
;
        ipvall(ifile,*,*,*)=ipv
        pall(ifile,*,*,*)=p
        uall(ifile,*,*,*)=u
        vall(ifile,*,*,*)=v
        qdfall(ifile,*,*,*)=qdf
        coall(ifile,*,*,*)=co
        zall(ifile,*,*,*)=z
        sfall(ifile,*,*,*)=sf
        mall(ifile,*,*,*)=mark
        tall(ifile,*,*,*)=tp
    endfor	; loop over files
;
; average
;
    ipvavg=ipvavg/float(nfile)
    pavg=pavg/float(nfile)
    uavg=uavg/float(nfile)
    vavg=vavg/float(nfile)
    qdfavg=qdfavg/float(nfile)
    coavg=coavg/float(nfile)
    zavg=zavg/float(nfile)
    sfavg=sfavg/float(nfile)
    mavg=mavg/float(nfile)
    tavg=tavg/float(nfile)
;
; sigma
;
    ipvsig=stddev(ipvall,dim=1)
    psig=stddev(pall,dim=1)
    usig=stddev(uall,dim=1)
    vsig=stddev(vall,dim=1)
    qdfsig=stddev(qdfall,dim=1)
    cosig=stddev(coall,dim=1)
    zsig=stddev(zall,dim=1)
    sfsig=stddev(sfall,dim=1)
    msig=stddev(mall,dim=1)
    tsig=stddev(tall,dim=1)
;
; save daily mean of all years
;
    ofile=dir+mmdd(iday)+'.sav'
    print,'saving '+ofile
    save,filename=ofile,nc,nr,nth,alon,alat,th,ipvavg,pavg,uavg,vavg,qdfavg,coavg,zavg,sfavg,mavg,$
                   ipvsig,psig,usig,vsig,qdfsig,cosig,zsig,sfsig,msig
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='YZ_figures/yz_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; calculate zonal means
;
    ipvyz=mean(ipvavg,dim=2)
    for k=0L,nth-1L do ipvyz(*,k)=ipvyz(*,k)*((th(k)/2000.))^(-9./2.)
    pyz=mean(pavg,dim=2)
    uyz=mean(uavg,dim=2)
    vyz=mean(vavg,dim=2)
    qdfyz=mean(qdfavg,dim=2)
    coyz=mean(coavg,dim=2)
    zyz=mean(zavg,dim=2)
    sfyz=mean(sfavg,dim=2)
    myz=mean(mavg,dim=2)
    tyz=mean(tavg,dim=2)

    ipvsigyz=mean(ipvsig,dim=2)
    for k=0L,nth-1L do ipvsigyz(*,k)=ipvsigyz(*,k)*((th(k)/2000.))^(-9./2.)
    psigyz=mean(psig,dim=2)
    usigyz=mean(usig,dim=2)
    vsigyz=mean(vsig,dim=2)
    qdfsigyz=mean(qdfsig,dim=2)
    cosigyz=mean(cosig,dim=2)
    zsigyz=mean(zsig,dim=2)
    sfsigyz=mean(sfsig,dim=2)
    msigyz=mean(msig,dim=2)
    tsigyz=mean(tsig,dim=2)
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
    contour,tyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[-90,90],xtitle='Latitude',yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='WACCM '+mmdd(iday)
    contour,tsigyz,alat,zyz,/noera,/foll,color=0,thick=2,levels=5*findgen(20),/overplot
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
    contour,usigyz,alat,zyz,/noeras,/foll,color=0,thick=2,levels=5*findgen(20),/overplot
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
    contour,vsigyz,alat,zyz,/noeras,/foll,color=0,thick=2,levels=5*findgen(20),/overplot
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
    qlevel=-1.+.1*findgen(nlvls)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    !type=2^2+2^3
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,myz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=qlevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
    contour,msigyz,alat,zyz,/noera,/foll,color=0,levels=0.2*findgen(20),/overplot,thick=2
    xmnb=xmx +cbaryoff
    xmxb=xmnb+cbarydel
    set_viewport,xmnb,xmxb,yorig(3)+0.01,yorig(3)+ylen-0.01
    !type=2^2+2^3+2^5
    imin=min(qlevel)
    imax=max(qlevel)
    plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Marker'
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
    ipvsigyz=ipvsigyz/1000.
    pvlevel=-200.+20*findgen(nlvls)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    !type=2^2+2^3
    xmn=xorig(4)
    xmx=xorig(4)+xlen
    ymn=yorig(4)
    ymx=yorig(4)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,ipvyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=pvlevel,xrange=[-90,90],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,xtitle='Latitude'
    contour,ipvsigyz,alat,zyz,/noera,/foll,color=0,levels=20*findgen(20),/overplot,thick=2
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
    contour,cosigyz,alat,zyz,/noera,/foll,color=0,levels=1*findgen(20),/overplot,thick=2
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
       spawn,'convert -trim YZ_figures/yz_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps -rotate -90 YZ_figures/yz_avg+sig_waccm_smidemax_'+mmdd(iday)+'.jpg'
       spawn,'rm -f YZ_figures/yz_avg+sig_waccm_smidemax_'+mmdd(iday)+'.ps'
    endif

skipday:
endfor	; loop over days of the year
end
