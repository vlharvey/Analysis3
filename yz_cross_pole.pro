
; plot polar projection and yz cross polar section

a=findgen(8)*(2*!pi/8.)
usersym,1.5*cos(a),1.5*sin(a),/fill

loadct,39
icolmax=byte(!p.color)
mcolor=icolmax
icmm1=icolmax-1B
icmm2=icolmax-2B
nlvls=31
col1=1+indgen(nlvls)*icolmax/nlvls
!NOERAS=-1
!P.FONT=0
SETPLOT='x'
device,decompose=0
;read,'setplot',setplot
; define viewport location
nxdim=750
nydim=750
xorig=[0.30,0.10,0.55]
yorig=[0.55,0.13,0.13]
xlen=0.4
ylen=0.4
cbaryoff=0.06
cbarydel=0.01
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/mee00fpl_FW2.cam2.h3.dyns.'
ifiles=file_search(dir+'*_3D_dyn.nc3',count=nfile)
for n=0,nfile-1 do begin
    result=strsplit(ifiles(n),'.',/extract)
    result2=strsplit(result(4),'_',/extract)
    sdate=result2(0)
    print,sdate
    iflag=0
;
; read daily file
;
    ncfile0=ifiles(n)
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
        if result.name eq 'IPV' then pv2=data
        if result.name eq 'P' then p2=data
        if result.name eq 'U' then u2=data
        if result.name eq 'V' then v2=data
        if result.name eq 'QDF' then qdf2=data
        if result.name eq 'Q' then q2=data
        if result.name eq 'GPH' then gph2=data
        if result.name eq 'TTGW' then ttgw2=data
        if result.name eq 'SF' then sf2=data
        if result.name eq 'MARK' then marksf2=data
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
    if iflag eq 1 then goto,jump
    s2=sqrt(u2^2+v2^2)
    xlon=alon
    xlat=alat

    t2=0.*p2
    z2=gph2
    for k=0,nth-1 do t2(*,*,k) = th(k)*( (p2(*,*,k)/1000.)^(.286) )
;
    if n eq 0 then begin
;   print,th
;   read,' Enter desired theta surface ',theta
    theta=2000.
    thlev=where(th eq theta)
    if thlev(0) eq -1 then stop,'Bad Theta'
    thlev=thlev(0)
    sth=strcompress(string(fix(theta)),/remove_all)
;   print,alon
;   read,' Enter longitude ',rlon1
    rlon1=0.
    index1=where(alon eq rlon1)
    if index1(0) eq -1 then stop,'Bad Longitude'
    ilon1=index1(0)
    rlon2=rlon1+180.
    if rlon2 gt max(alon) then rlon2=rlon2-360.
    index2=where(alon eq rlon2)
    ilon2=index2(0)
    slon1=strcompress(string(rlon1),/remove_all)+'E'
    slon2=strcompress(string(rlon2),/remove_all)+'E'
    print,'longitudes ',rlon1,rlon2
    x=fltarr(nc+1)
    x(0:nc-1)=alon(0:nc-1)
    x(nc)=alon(0)+360.
    x2d=fltarr(nc+1,nr)
    y2d=fltarr(nc+1,nr)
    for i=0,nc do y2d(i,*)=alat
    for j=0,nr-1 do x2d(*,j)=x
    xyz=fltarr(nr,nth)
    yyz=fltarr(nr,nth)
    for i=0,nr-1 do yyz(i,*)=th
    for j=0,nth-1 do xyz(0:nr/2-1,j)=alat(nr/2:nr-1) 
    for j=0,nth-1 do xyz(nr/2:nr-1,j)=reverse(alat(nr/2:nr-1)) 
    endif
    p1=transpose(p2(*,*,thlev))
    pv1=transpose(pv2(*,*,thlev))
    qdf1=transpose(qdf2(*,*,thlev))
    mark1=transpose(marksf2(*,*,thlev))
    sf1=transpose(sf2(*,*,thlev))
    u1=transpose(u2(*,*,thlev))
    t1=transpose(t2(*,*,thlev))
    z1=transpose(z2(*,*,thlev))
    u=fltarr(nc+1,nr)
    u(0:nc-1,0:nr-1)=u1(0:nc-1,0:nr-1)
    u(nc,*)=u(0,*)
    t=fltarr(nc+1,nr)
    t(0:nc-1,0:nr-1)=t1(0:nc-1,0:nr-1)
    t(nc,*)=t(0,*)
    z=fltarr(nc+1,nr)
    z(0:nc-1,0:nr-1)=z1(0:nc-1,0:nr-1)
    z(nc,*)=z(0,*)
    p=0.*fltarr(nc+1,nr)
    p(0:nc-1,0:nr-1)=p1(0:nc-1,0:nr-1)
    p(nc,*)=p(0,*)
    pv=0.*fltarr(nc+1,nr)
    pv(0:nc-1,0:nr-1)=pv1(0:nc-1,0:nr-1)*1.e6
    pv(nc,*)=pv(0,*)
    qdf=fltarr(nc+1,nr)
    qdf(0:nc-1,0:nr-1)=qdf1(0:nc-1,0:nr-1)
    qdf(nc,*)=qdf(0,*)
    mark=0.*fltarr(nc+1,nr)
    mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
    mark(nc,*)=mark(0,*)
    sf=0.*fltarr(nc+1,nr)
    sf(0:nc-1,0:nr-1)=sf1(0:nc-1,0:nr-1)
    sf(nc,*)=sf(0,*)
    temp=theta*((p/1000.)^(.286))
    syz=fltarr(nr,nth)
    uyz=fltarr(nr,nth)
    tyz=fltarr(nr,nth)
    zyz=fltarr(nr,nth)
    markyz=fltarr(nr,nth)
    pvyz=fltarr(nr,nth)
    for k=0,nth-1 do begin
        uyz(0:nr/2-1,k)=u2(nr/2:nr-1,ilon1,k)
        uyz(nr/2:nr-1,k)=reverse(u2(nr/2:nr-1,ilon2,k))
        syz(0:nr/2-1,k)=s2(nr/2:nr-1,ilon1,k)
        syz(nr/2:nr-1,k)=reverse(s2(nr/2:nr-1,ilon2,k))
        tyz(0:nr/2-1,k)=t2(nr/2:nr-1,ilon1,k)
        tyz(nr/2:nr-1,k)=reverse(t2(nr/2:nr-1,ilon2,k))
        zyz(0:nr/2-1,k)=z2(nr/2:nr-1,ilon1,k)
        zyz(nr/2:nr-1,k)=reverse(z2(nr/2:nr-1,ilon2,k))
        markyz(0:nr/2-1,k)=marksf2(nr/2:nr-1,ilon1,k)
        markyz(nr/2:nr-1,k)=reverse(marksf2(nr/2:nr-1,ilon2,k))
    endfor

    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/color,/landscape,bits=8,filename='waccm4_'+ifile+'_'+sth+'_'+slon1+'.ps'
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
    endif

; plot
    !noeras=1
    !type=2^2+2^3
    !p.thick=1
    erase
    !psym=0
    ipan=0
    xmn=xorig(ipan)
    xmx=xorig(ipan)+xlen
    ymn=yorig(ipan)
    ymx=yorig(ipan)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    mtitle=sth+' K WACCM4 PV on '+sdate
    !psym=0
    !type=2^2+2^3
    MAP_SET,90,0,-90,/ortho,/noeras,title=mtitle,color=lc
    index=where(y2d ge 20.)
    pvmin=0.
    pvmax=max(pv(index))
    pvint=(pvmax-pvmin)/(nlvls-1)
    pvlevel=pvmin+pvint*findgen(nlvls)
    contour,pv,x,alat,levels=pvlevel,/noeras,/overplot,$
            c_color=col1,/cell_fill
    contour,pv,x,alat,levels=pvlevel,/noeras,/overplot,$
            color=0,/follow,c_labels=0+intarr(nlvls)
    contour,mark,x,alat,levels=[0.1],/noeras,/overplot,$
            color=mcolor,/follow,thick=3,c_labels=[0]
    contour,mark,x,alat,levels=[-0.1],/noeras,/overplot,$
            color=mcolor*.9,/follow,thick=3,c_labels=[0]
    MAP_SET,90,0,-90,/ortho,/grid,/contin,/noeras,/noborder,color=0
    oplot,rlon1+0.*alat,alat,color=icolmax,linestyle=2
    oplot,rlon2+0.*alat,alat,color=icolmax,linestyle=2
    ipan=1
    xmn=xorig(ipan)
    xmx=xorig(ipan)+xlen
    ymn=yorig(ipan)
    ymx=yorig(ipan)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    level=5*findgen(nlvls)
    contour,syz,alat,th,levels=level,/fill,/cell_fill,c_color=col1,yrange=[500.,10000.],$
            xtitle='!6'+slon1+'       Latitude      '+slon2,ytitle='!6Theta',$
            xtickname=['Eq','30','60','NP','60','30','Eq'],xticks=6
    contour,syz,alat,th,levels=level,/noeras,/overplot,$
            color=0,/follow,c_labels=0+intarr(nlvls)
    contour,markyz,alat,th,levels=[0.1],/noeras,/overplot,$
            color=mcolor,/follow,thick=3,c_labels=[0]
    contour,markyz,alat,th,levels=[-0.1],/noeras,/overplot,$
            color=0.9*mcolor,/follow,thick=3,c_labels=[0]
    oplot,alat,theta+0.*alat,color=icolmax,linestyle=2
    imin=min(level)
    imax=max(level)
    ymnb=ymn -cbaryoff
    ymxb=ymnb+cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras
    ybox=[0,10,10,0,0]
    x2=imin
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor

    ipan=2
    xmn=xorig(ipan)
    xmx=xorig(ipan)+xlen
    ymn=yorig(ipan)
    ymx=yorig(ipan)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    imin=min(tyz)
    imax=max(tyz)
imax=300.
    if n eq 0 then mpvlevel=imin+((imax-imin)/nlvls)*findgen(nlvls)
    contour,tyz,alat,th,levels=mpvlevel,/fill,/cell_fill,c_color=col1,yrange=[500.,10000.],$
            xtitle='!6'+slon1+'       Latitude      '+slon2,ytitle='!6Theta',$
            xtickname=['Eq','30','60','NP','60','30','Eq'],xticks=6
    contour,tyz,alat,th,levels=mpvlevel,/noeras,/overplot,$
            color=0,/follow,c_labels=0+intarr(nlvls)
    tlevel=[185.,190.,195.,200.]
    contour,tyz,alat,th,levels=tlevel,/noeras,/overplot,$
            color=mcolor,/follow,c_labels=0+intarr(nlvls)
    contour,markyz,alat,th,levels=[0.1],/noeras,/overplot,$
            color=mcolor,/follow,thick=3,c_labels=[0]
    contour,markyz,alat,th,levels=[-0.1],/noeras,/overplot,$
            color=0.9*mcolor,/follow,thick=3,c_labels=[0]
    oplot,alat,theta+0.*alat,color=icolmax,linestyle=2
    imin=min(mpvlevel)
    imax=max(mpvlevel)
    ymnb=ymn -cbaryoff
    ymxb=ymnb+cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras
    ybox=[0,10,10,0,0]
    x2=imin
    dx=(imax-imin)/(float(nlvls)-1)
    for j=1,nlvls-1 do begin
        xbox=[x2,x2,x2+dx,x2+dx,x2]
        polyfill,xbox,ybox,color=col1(j)
        x2=x2+dx
    endfor

; Close PostScript file and return control to X-windows
    if setplot ne 'ps' then wait,1
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim waccm4_'+ifile+'_'+sth+'_'+slon1+'.ps -rotate -90 waccm4_'+ifile+'_'+sth+'_'+slon1+'.jpg'
    endif
    jump:
endfor		; loop over days
end
