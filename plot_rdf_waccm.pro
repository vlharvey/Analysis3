; 
; plot RDF 4 panel. WACCM
; 
@gcm_panels

loadct,39
device,decompose=0
mcolor=byte(!p.color)
!p.background=mcolor
!p.color=0
icmm1=mcolor-1B
icmm2=mcolor-2B
nxdim=750
nydim=750
xorig=[0.1,0.55,0.1,0.55]
yorig=[0.55,0.55,0.1,0.1]
xlen=0.35
ylen=0.35
cbaryoff=0.01
cbarydel=0.01
nlev=21
col1=1.+indgen(nlev)*mcolor/nlev
!noeras=1
a=findgen(8)*(2*!pi/8.)
usersym,.5*cos(a),.5*sin(a),/fill
nh=1
ifilet=['../WACCM_20090115_10day_rdf_800-5000.traj']
nruns=n_elements(ifilet)
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
!NOERAS=-1
setplot='ps'
read,'enter setplot',setplot
if setplot ne 'ps' then $
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
for nn=0L,nruns-1L do begin
    close,12
    openr,12,ifilet(nn),/f77
    print,ifilet(nn)
    nmax=600000l
      nrmax=181L
      ncmax=361L
      x=fltarr(ncmax)
      y=fltarr(nrmax)
      charexp='                                   '
      nthp=0L
      theta=0.
      ukmo=' '
      nmc=' '
      ecmwf=' '
      restart=' '
      rfile='                                     '
      nrday=0L
      dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
      nfile=0L
      wfile='mee00fpl_FW2.cam2.h3.dyns.20090115_3D_dyn.nc3'
      istime=0l
      ictime=0l
      dtflow=0.
      dt=0.
      igw=' '
      stream=' '
      nstrm=0L
      ds1=0.
      strm0=0.
      dstrm=0.
      pviso=' '
      npviso=0L
      ds2=0.
      pv0=0.
      dpv=0.
      tiso=' '
      ntiso=0L
      ds3=0.
      temp0=0.
      dtemp=0.
      space=' '
      dxs=0.
      dys=0.
      range_fill=' '
      stlat=0.
      stlon=0.
      er2_range=' '
      nrings=0L
      npts=0L
      dial=' '
      dird='/aura3/data/DIAL_data/Datfiles/'
      ndial=0l
      dfile='dial_o3_20000315.diag'
      er2=' '
      nleg=0L
      xleg0=0.
      yleg0=0.
      xleg1=0.
      yleg1=0.
      dc8=' '
      dirdc8='/aura3/data/DC8_data/Datfiles/'
      ndc8=0L
      dc8file='dial_o3_20000309.diag.dc8'
      dtout=0.
      ofile='                                          '
      nr=0L
      nc=0L
      time=0.
      ntraj=0l
    char1='xtraj'
    char2='ytraj'
    char3='ztraj'
    char4='pvtrj'
    char5='ptraj'
    char6='fdtraj'
    char7='fntraj'
    char8='qtraj'
    char9='qdftraj'
    char10='xmsftraj'
    char11='mark_traj'
    char12='t0_traj'
    char13='age_traj'
    char14='mint_traj'
    char15='no_traj'
    char16='co_traj'
    char17='e_traj'
    readu,12,charexp
    readu,12,nthp
    thlevp=fltarr(nthp)
    readu,12,thlevp
    readu,12,ukmo
    readu,12,nmc
    readu,12,ecmwf
    readu,12,restart
    readu,12,rfile
    readu,12,nrday
    readu,12,dir
    readu,12,nfile
    wfiles=strarr(nfile)
    for n=0,nfile-1 do begin
        readu,12,wfile
        wfiles(n)=strtrim(wfile)
    endfor
    readu,12,istime
    readu,12,dtflow                          
    readu,12,dt                              
    readu,12,igw                             
    readu,12,stream                          
    readu,12,nstrm                           
    readu,12,ds1                             
    readu,12,strm0                           
    readu,12,dstrm                 
    readu,12,pviso                           
    readu,12,npviso                          
    readu,12,ds2                             
    readu,12,pv0                             
    readu,12,dpv                             
    readu,12,tiso                            
    readu,12,ntiso                           
    readu,12,ds3                             
    readu,12,temp0                           
    readu,12,dtemp                           
    readu,12,space                           
    readu,12,dxs                             
    readu,12,dys                             
    readu,12,range_fill
    readu,12,stlat
    readu,12,stlon
    readu,12,er2_range
    readu,12,nrings
    readu,12,npts
    readu,12,dial
    readu,12,dird
    readu,12,ndial
    for n=1,ndial do begin 
        readu,12,dfile
        print , n, dfile 
    endfor 
    readu,12,er2                             
    readu,12,nleg                            
    for n=1,nleg do begin                             
        readu,12,xleg0
        readu,12,yleg0
        readu,12,xleg1
        readu,12,yleg1
    endfor                                    
    readu,12,dc8
    readu,12,dirdc8
    readu,12,ndc8
    for n=1,ndc8 do begin
        readu,12,dc8file
        print , n,dc8file
    endfor
    readu,12,dtout                           
    readu,12,ofile
    print,ofile
    print,'    Header read ok'
    nfiles=long(abs(dtflow)/dtout)*(nfile-1L)-4L
    for n=0,nfiles-1 do begin
        READU,12,istime,ictime,time,ntraj
        print,n,istime,ictime,time,ntraj
        stime=strcompress(string(ictime),/remove_all)
        mdate=long(strmid(stime,0,8))
        xn=fltarr(ntraj)
        yn=fltarr(ntraj)
        zn=fltarr(ntraj)
        pvn=fltarr(ntraj)
        pn=fltarr(ntraj)
        frday=fltarr(ntraj)
        frnght=fltarr(ntraj)
        qn=fltarr(ntraj)
        qdfn=fltarr(ntraj)
        xmsfn=fltarr(ntraj)
        xmrksfn=fltarr(ntraj)
        t0n=fltarr(ntraj)
        agen=fltarr(ntraj)
        xmintn=fltarr(ntraj)
        xnon=fltarr(ntraj)
        xcon=fltarr(ntraj)
        xen=fltarr(ntraj)
        readu,12,char1
        READU,12,xn
        readu,12,char2
        READU,12,yn
        readu,12,char3
        READU,12,zn
        readu,12,char4
        READU,12,pvn
        readu,12,char5
        READU,12,pn
        readu,12,char6
        READU,12,frday
        readu,12,char7
        READU,12,frnght
        readu,12,char8
        READU,12,qn
        readu,12,char9
        READU,12,qdfn
        readu,12,char10
        READU,12,xmsfn 
        readu,12,char11
        READU,12,xmrksfn
        index=where(xmrksfn lt 0.)
        if index(0) ne -1 then xmrksfn(index)=xmrksfn(index)/abs(xmrksfn(index))
        readu,12,char12
        READU,12,t0n
        readu,12,char13
        READU,12,agen
        readu,12,char14
        READU,12,xmintn
        readu,12,char15
        READU,12,xnon
        readu,12,char16
        READU,12,xcon
        readu,12,char17
        READU,12,xen

;       if ictime eq istime then begin
        if n eq 0 then begin
           etimes=fltarr(nfiles)
           mdates=lonarr(nfiles)
           xmrkave=xmrksfn
           xnoave=xnon
           xcoave=xcon
           xeave=xen
           qdfave=qdfn
           qave=qn
           pvave=pvn
           index=where(pvn eq 99999.)
           if index(0) ne -1 then begin
              xmrkave(index)=0.
              qdfave(index)=0.
              qave(index)=0.
              pvave(index)=0.
              xnoave(index)=0.
              xcoave(index)=0.
              xeave(index)=0.
           endif
           x0=xn
           y0=yn
           z0=zn
           xsave=fltarr(nfiles,ntraj)
           ysave=fltarr(nfiles,ntraj)
           zsave=fltarr(nfiles,ntraj)
           pvsave=fltarr(nfiles,ntraj)
           psave=fltarr(nfiles,ntraj)
           qsave=fltarr(nfiles,ntraj)
           qdfsave=fltarr(nfiles,ntraj)
           xmsfsave=fltarr(nfiles,ntraj)
           xmrksfsave=fltarr(nfiles,ntraj)
           agesave=fltarr(nfiles,ntraj)
           xnosave=fltarr(nfiles,ntraj)
           xcosave=fltarr(nfiles,ntraj)
           xesave=fltarr(nfiles,ntraj)
           qdfavesave=fltarr(nfiles,ntraj)
           pvavesave=fltarr(nfiles,ntraj)
           xmrksfavesave=fltarr(nfiles,ntraj)
           qavesave=fltarr(nfiles,ntraj)
        endif
         if (n gt 0) then begin
           index=where(pvn ne 99999.)
           if index(0) ne -1 then begin
              xmrkave(index)=((n-1)*xmrkave(index)+xmrksfn(index))/n
              qdfave(index)=((n-1)*qdfave(index)+qdfn(index))/n
              pvave(index)=((n-1)*pvave(index)+pvn(index))/n
              qave(index)=((n-1)*qave(index)+qn(index))/n
              xcoave(index)=((n-1)*xcoave(index)+xcon(index))/n
              xeave(index)=((n-1)*xeave(index)+xen(index))/n
           endif
         endif
        etimes(n)=time
        mdates(n)=mdate
;
; Save daily trajectory information
;
        xsave(n,0:ntraj-1)=xn
        ysave(n,0:ntraj-1)=yn
        zsave(n,0:ntraj-1)=zn
        pvsave(n,0:ntraj-1)=pvn
        psave(n,0:ntraj-1)=pn
        qsave(n,0:ntraj-1)=qn
        qdfsave(n,0:ntraj-1)=qdfn
        xmsfsave(n,0:ntraj-1)=xmsfn
        xmrksfsave(n,0:ntraj-1)=xmrksfn
        agesave(n,0:ntraj-1)=agen
        qavesave(n,0:ntraj-1)=qave
        xcosave(n,0:ntraj-1)=xcon
        xesave(n,0:ntraj-1)=xen
        qdfavesave(n,0:ntraj-1)=qdfave
        pvavesave(n,0:ntraj-1)=pvave
        xmrksfavesave(n,0:ntraj-1)=xmrkave
    endfor              ; end of loop over days

    imax=long(360./dxs)     ; initialize global grid
    jmax=long(180./dys+1)
    xnd=360.*findgen(imax+1)/imax
    ynd=-90.+180.*findgen(jmax)/(jmax-1)
    xmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    ymap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    zmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    pvmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    pmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    qmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    qdfmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    xmrksfmap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
;   agemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    qdfavemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    pvavemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    xmrksfavemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    qavemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    xcomap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
    xemap=9999.+fltarr(imax+1,jmax,nthp,nfiles)
;
; build mapped arrays
;
    for n=0,nfiles-1 do begin
        ncount=0l
        for i=0,imax-1 do begin
            for j=0,jmax-1 do begin
                for k=0,nthp-1 do begin
                    xmap(i,j,k,n)=xsave(n,ncount)       ; mapped positions
                    ymap(i,j,k,n)=ysave(n,ncount)
                    zmap(i,j,k,n)=zsave(n,ncount)
                    pvmap(i,j,k,n)=pvsave(n,ncount)
                    pmap(i,j,k,n)=psave(n,ncount)
                    qmap(i,j,k,n)=qsave(n,ncount)
                    qavemap(i,j,k,n)=qavesave(n,ncount)
                    qdfmap(i,j,k,n)=qdfsave(n,ncount)
                    xmrksfmap(i,j,k,n)=xmrksfsave(n,ncount)
;                   agemap(i,j,k,n)=agesave(n,ncount)
                    xcomap(i,j,k,n)=xcosave(n,ncount)
                    xemap(i,j,k,n)=xesave(n,ncount)
                    qdfavemap(i,j,k,n)=qdfavesave(n,ncount)
                    pvavemap(i,j,k,n)=pvavesave(n,ncount)
                    xmrksfavemap(i,j,k,n)=xmrksfavesave(n,ncount)
                    ncount=ncount+1l
                endfor
            endfor
        endfor
    endfor
;
; initialize wrap around point
;
    xmap(imax,*,*,*)=xmap(0,*,*,*)
    ymap(imax,*,*,*)=ymap(0,*,*,*)
    zmap(imax,*,*,*)=zmap(0,*,*,*)
    pvmap(imax,*,*,*)=pvmap(0,*,*,*)
    pmap(imax,*,*,*)=pmap(0,*,*,*)
    qmap(imax,*,*,*)=qmap(0,*,*,*)
    qavemap(imax,*,*,*)=qavemap(0,*,*,*)
    qdfmap(imax,*,*,*)=qdfmap(0,*,*,*)
    xmrksfmap(imax,*,*,*)=xmrksfmap(0,*,*,*)
;   agemap(imax,*,*,*)=agemap(0,*,*,*)
    xcomap(imax,*,*,*)=xcomap(0,*,*,*)
    xemap(imax,*,*,*)=xemap(0,*,*,*)
    qdfavemap(imax,*,*,*)=qdfavemap(0,*,*,*)
    pvavemap(imax,*,*,*)=pvavemap(0,*,*,*)
    xmrksfavemap(imax,*,*,*)=xmrksfavemap(0,*,*,*)

    for n=0,nfiles-1 do begin
    stime0=strcompress(mdates(0),/remove_all)
    stime1=strcompress(mdates(n),/remove_all)
    setime=strcompress(long(abs(etimes(n))),/remove_all)       ; elapsed hours

syr=strmid(stime1,0,4)
smn=strmid(stime1,4,2)
sdy=strmid(stime1,6,2)
;
; read daily data
;
    ncfile0=dir+'mee00fpl_FW2.cam2.h3.dyns.'+stime1+'_3D_dyn.nc3'
    dum=findfile(ncfile0)
;   if dum(0) eq '' then goto,jump
    print,ncfile0
    ncid=ncdf_open(ncfile0)
    result0=ncdf_inquire(ncid)
    for idim=0,result0.ndims-1 do begin
        ncdf_diminq,ncid,idim,name,dim
        if name eq 'number_of_latitudes' then nr=dim
        if name eq 'number_of_longitudes' then nc=dim
        if name eq 'number_of_levels' then inth=dim
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
        if result.name eq 'MARK' then mark2=data
        print,ivar,result.name,min(data),max(data)
    endfor
    ncdf_close,ncid
;
; loop over RDF theta surfaces
;
    for k=0,0 do begin	;nthp-2 do begin   
        theta=thlevp(k)
        kp=where(thlevp eq theta)
        kp=kp(0)
        stheta=strcompress(string(long(thlevp(kp))),/remove_all)+'K'
;
; extract correct DAO theta surface
;
        index=where(th eq theta)
        print,thlevp(kp),th(index(0))
        mark1=transpose(mark2(*,*,index(0)))
        q1=transpose(q2(*,*,index(0)))
        pv1=transpose(pv2(*,*,index(0)))
        xx=fltarr(nc+1)
        xx(0:nc-1)=alon
        xx(nc)=xx(0)
        mark=fltarr(nc+1,nr)
        mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
        mark(nc,*)=mark(0,*)
        q=fltarr(nc+1,nr)
        q(0:nc-1,0:nr-1)=q1(0:nc-1,0:nr-1)
        q(nc,*)=q(0,*)
        pv=fltarr(nc+1,nr)
        pv(0:nc-1,0:nr-1)=pv1(0:nc-1,0:nr-1)
        pv(nc,*)=pv(0,*)
        lon=0.*pv
        lat=0.*pv
        for i=0,nc   do lat(i,*)=alat
        for j=0,nr-1 do lon(*,j)=xx

        if setplot eq 'ps' then begin
           set_plot,'ps'
           xsize=nxdim/100.
           ysize=nydim/100.
           !psym=0
           !p.font=0
           device,font_size=9
           device,/color,/landscape,bits=8,filename=$
                  'Figures/rdf_4pan_'+stime1+'_'+setime+'hrs_'+stheta+'_waccm4.ps'
           device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                  xsize=xsize,ysize=ysize
        endif
        erase
        xmn=xorig(1)
        xmx=xorig(1)+xlen
        ymn=yorig(1)
        ymx=yorig(1)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        MAP_SET,90,-90,0,/stereo,/GRID,/noborder,charsize=2,title='RDF PV',/noeras,limit=[30.,0.,90.,360.]
        oplot,findgen(361),.5+0.*findgen(361),psym=8
        dum=reform(pvmap(*,*,kp,n),imax+1,jmax)
        xday=reform(xmap(*,*,kp,0),imax+1,jmax)
        yday=reform(ymap(*,*,kp,0),imax+1,jmax)
        index=where(abs(dum) lt 9999. and yday gt 30.)
        zmin=min(dum(index))-0.1*min(dum(index))
        zmax=max(dum(index))+0.1*max(dum(index))
        zint=(zmax-zmin)/float(nlev)
        level=zmin+zint*findgen(nlev)
        contour,dum,xday,yday,/cell_fill,c_colors=col1,/overplot,levels=level,/noeras
        contour,dum,xday,yday,/follow,color=0,/overplot,levels=[0],/noeras,thick=3
        contour,mark,xx,alat,levels=[-.1],thick=4,/overplot,color=mcolor,/noeras
        contour,mark,xx,alat,levels=[.1],thick=4,/overplot,color=0,/noeras
        MAP_SET,90,-90,0,/stereo,/contin,/grid,/noeras,charsize=2,limit=[30.,0.,90.,360.]
        oplot,findgen(361),0.*findgen(361),psym=3,color=mcolor
        xyouts,.25,.95,'WACCM4 '+stheta+' '+stime1+'    ('+setime+' hours)',/normal,charsize=2
        !psym=0
        kmin=min(level)
        kmax=max(level)
        ymnb=ymn -.5*cbaryoff
        ymxb=ymnb+cbarydel
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[kmin,kmax],[0,0],yrange=[0,10],xrange=[kmin,kmax],/noeras
        ybox=[0,10,10,0,0]
        x2=kmin
        dx=(kmax-kmin)/float(nlev)
        for j=0,nlev-1 do begin
            xbox=[x2,x2,x2+dx,x2+dx,x2]
            polyfill,xbox,ybox,color=col1(j)
            x2=x2+dx
        endfor

        xmn=xorig(0)
        xmx=xorig(0)+xlen
        ymn=yorig(0)
        ymx=yorig(0)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        MAP_SET,90,-90,0,/stereo,/GRID,/noborder,charsize=2,title='RDF Latitude',/noeras,limit=[30.,0.,90.,360.]
        oplot,findgen(361),.5+0.*findgen(361),psym=8
        dum=reform(ymap(*,*,kp,n),imax+1,jmax)
        xday=reform(xmap(*,*,kp,0),imax+1,jmax)
        yday=reform(ymap(*,*,kp,0),imax+1,jmax)
        index=where(dum ne 9999. and yday gt 30.)
        zmin=min(dum(index))-0.1*min(dum(index))
        zmax=max(dum(index))+0.1*max(dum(index))
        zint=(zmax-zmin)/float(nlev)
        level=zmin+zint*findgen(nlev)
        contour,dum,xday,yday,/cell_fill,c_colors=col1,/overplot,levels=level,/noeras
        contour,dum,xday,yday,/follow,color=0,/overplot,levels=[60],/noeras,thick=3
        contour,mark,xx,alat,levels=[-.1],thick=4,/overplot,color=mcolor,/noeras
        contour,mark,xx,alat,levels=[.1],thick=4,/overplot,color=0,/noeras
        MAP_SET,90,-90,0,/stereo,/contin,/grid,/noeras,charsize=2,limit=[30.,0.,90.,360.]
        oplot,findgen(361),0.*findgen(361),psym=3,color=mcolor
        !psym=0
        kmin=min(level)
        kmax=max(level)
        ymnb=ymn -.5*cbaryoff
        ymxb=ymnb+cbarydel
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[kmin,kmax],[0,0],yrange=[0,10],xrange=[kmin,kmax],/noeras
        ybox=[0,10,10,0,0]
        x2=kmin
        dx=(kmax-kmin)/float(nlev)
        for j=0,nlev-1 do begin
            xbox=[x2,x2,x2+dx,x2+dx,x2]
            polyfill,xbox,ybox,color=col1(j)
            x2=x2+dx
        endfor

        xmn=xorig(2)
        xmx=xorig(2)+xlen
        ymn=yorig(2)
        ymx=yorig(2)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        MAP_SET,90,-90,0,/stereo,/GRID,/noborder,charsize=2,title='RDF Mark',/noeras,limit=[30.,0.,90.,360.]
        oplot,findgen(361),.5+0.*findgen(361),psym=8
        dum=reform(xmrksfmap(*,*,kp,n),imax+1,jmax)
        xday=reform(xmap(*,*,kp,0),imax+1,jmax)
        yday=reform(ymap(*,*,kp,0),imax+1,jmax)
        index=where(dum ne 9999. and lat gt 30.)
        zmin=-1.
        zmax=1.
        zint=(zmax-zmin)/float(nlev)
        level=zmin+zint*findgen(nlev)
        contour,dum,xday,yday,levels=level,/cell_fill,c_colors=col1,/overplot,/noeras
        index=where(level gt 0.)
        if index(0) ne -1 then contour,dum,xday,yday,levels=level(index),/follow,color=0,/overplot,/noeras
        index=where(level lt 0.)
        if index(0) ne -1 then contour,dum,xday,yday,levels=level(index),/follow,color=mcolor,c_linestyle=5,/overplot,/noeras
        contour,mark,xx,alat,levels=[-.1],thick=4,/overplot,color=mcolor,/noeras
        contour,mark,xx,alat,levels=[.1],thick=4,/overplot,color=0,/noeras
        MAP_SET,90,-90,0,/stereo,/contin,/grid,/noeras,charsize=2,limit=[30.,0.,90.,360.]
        oplot,findgen(361),0.*findgen(361),psym=3,color=mcolor
        !psym=0
        kmin=min(level)
        kmax=max(level)
        ymnb=ymn -.5*cbaryoff
        ymxb=ymnb+cbarydel
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[kmin,kmax],[0,0],yrange=[0,10],xrange=[kmin,kmax],/noeras,xtitle='(K/day)'
        ybox=[0,10,10,0,0]
        x2=kmin
        dx=(kmax-kmin)/float(nlev)
        for j=0,nlev-1 do begin
            xbox=[x2,x2,x2+dx,x2+dx,x2]
            polyfill,xbox,ybox,color=col1(j)
            x2=x2+dx
        endfor

        xmn=xorig(3)
        xmx=xorig(3)+xlen
        ymn=yorig(3)
        ymx=yorig(3)+ylen
        set_viewport,xmn,xmx,ymn,ymx
        MAP_SET,90,-90,0,/stereo,/GRID,/noborder,charsize=2,title='RDF QDF',/noeras,limit=[30.,0.,90.,360.]
        oplot,findgen(361),.5+0.*findgen(361),psym=8
        dum=reform(qdfmap(*,*,kp,n),imax+1,jmax)
;       dum=reform(zmap(*,*,kp,n),imax+1,jmax)
        xday=reform(xmap(*,*,kp,0),imax+1,jmax)
        yday=reform(ymap(*,*,kp,0),imax+1,jmax)
        index=where(dum ne 9999. and yday gt 0.)
        zmin=min(dum(index))	;-0.1*min(dum(index))
        zmax=max(dum(index))	;+0.1*max(dum(index))
        zint=(zmax-zmin)/float(nlev)
        level=zmin+zint*findgen(nlev)
        contour,dum,xday,yday,/cell_fill,c_colors=col1,/overplot,levels=level,/noeras
;       contour,dum,xday,yday,/follow,color=0,/overplot,levels=[0],/noeras,thick=3
        contour,mark,xx,alat,levels=[-.1],thick=4,/overplot,color=mcolor,/noeras
        contour,mark,xx,alat,levels=[.1],thick=4,/overplot,color=0,/noeras
        MAP_SET,90,-90,0,/stereo,/contin,/grid,/noeras,charsize=2,limit=[30.,0.,90.,360.]
        oplot,findgen(361),0.*findgen(361),psym=3,color=mcolor
        !psym=0
        kmin=min(level)
        kmax=max(level)
        ymnb=ymn -.5*cbaryoff
        ymxb=ymnb+cbarydel
        set_viewport,xmn,xmx,ymnb,ymxb
        !type=2^2+2^3+2^6
        plot,[kmin,kmax],[0,0],yrange=[0,10],xrange=[kmin,kmax],/noeras,xtitle='s!u-1!n'
        ybox=[0,10,10,0,0]
        x2=kmin
        dx=(kmax-kmin)/float(nlev)
        for j=0,nlev-1 do begin
            xbox=[x2,x2,x2+dx,x2+dx,x2]
            polyfill,xbox,ybox,color=col1(j)
            x2=x2+dx
        endfor

        if setplot ne 'ps' then stop
        if setplot eq 'ps' then begin
           device,/close 
           spawn,'convert Figures/rdf_4pan_'+stime1+'_'+setime+'hrs_'+stheta+'_waccm4.ps'+$
                 ' -rotate -90 Figures/rdf_4pan_'+stime1+'_'+setime+'hrs_'+stheta+'_waccm4.jpg'
;          spawn,'rm -f Figures/rdf_4pan_'+stime1+'_'+setime+'hrs_'+stheta+'_waccm4.ps'
        endif
    endfor	; loop over theta
    endfor	; loop over days
endfor		; loop over RDF runs
end
