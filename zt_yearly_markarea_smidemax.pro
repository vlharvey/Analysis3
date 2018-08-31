;
; loop over raw 3d daily files
; plot annual zt of SF mark area in % of the hemisphere
; SmidEmax
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
xorig=[0.15,0.15]
yorig=[0.6,0.15]
xlen=0.7
ylen=0.35
cbaryoff=0.05
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)
;
; area stuff
;
re=40000./2./!pi
earth_area=4.*!pi*re*re
hem_area=earth_area/2.0
rtd=double(180./!pi)
dtr=1./rtd
nrr=91L
yeq=findgen(nrr)
latcircle=fltarr(nrr)
hem_frac=fltarr(nrr)
for j=0,nrr-2 do begin
    hy=re*dtr
    dx=re*cos(yeq(j)*dtr)*360.*dtr
    latcircle(j)=dx*hy
endfor
for j=0,nrr-1 do begin
    if yeq(j) ge 0. then index=where(yeq ge yeq(j))
    if index(0) ne -1 then hem_frac(j)=100.*total(latcircle(index))/hem_area
    if yeq(j) eq 0. then hem_frac(j)=100.
endfor
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
; loop over years
;
icount=0L
for iyear=1L,300L do begin
    syear=string(format='(i3.3)',iyear)
    ofile=dir+'markarea_'+syear+'.sav'
    dum=findfile(ofile)
    if dum(0) ne '' then goto,quick
;
; loop over days of the year
;
    for iday=0,ndays-1 do begin
        print,syear,' ',mmdd(iday)
        filename=file_search(dir+syear+mmdd(iday)+'.nc3')
;
        ncfile0=filename(0)
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
;           if result.name eq 'IPV' then ipv=data
            if result.name eq 'P' then p=data
;           if result.name eq 'U' then u=data
;           if result.name eq 'V' then v=data
;           if result.name eq 'QDF' then qdf=data
;           if result.name eq 'CO' then co=data*1.e6
            if result.name eq 'GPH' then z=data/1000.
;           if result.name eq 'SF' then sf=data
            if result.name eq 'MARK' then mark=data
;           print,ivar,result.name,min(data),max(data)
        endfor
        ncdf_close,ncid
        index=where(mark lt 0.)
        if index(0) ne -1L then mark(index)=-1.
;
        if icount eq 0L and iday eq 0L then begin
           markarea_on_th_2d_nh=fltarr(ndays,nth)
           z_on_th_2d_nh=fltarr(ndays,nth)
           p_on_th_2d_nh=fltarr(ndays,nth)
           pw1_on_th_2d_nh=fltarr(ndays,nth)
           markarea_on_th_2d_sh=fltarr(ndays,nth)
           z_on_th_2d_sh=fltarr(ndays,nth)
           p_on_th_2d_sh=fltarr(ndays,nth)
           pw1_on_th_2d_sh=fltarr(ndays,nth)
;
; area
;
           lon=0.*fltarr(nc,nr)
           lat=0.*fltarr(nc,nr)
           for i=0,nc-1 do lat(i,*)=alat
           for j=0,nr-1 do lon(*,j)=alon
           area=0.*lat
           deltax=alon(1)-alon(0)
           deltay=alat(1)-alat(0)
           for j=0,nr-1 do begin
               hy=re*deltay*dtr
               dx=re*cos(alat(j)*dtr)*deltax*dtr
               area(*,j)=dx*hy    ; area of each grid point
           endfor
           x2d=fltarr(nc,nr)
           y2d=fltarr(nc,nr)
           for i=0,nc-1 do y2d(i,*)=alat
           for j=0,nr-1 do x2d(*,j)=alon

           nhindex=where(alat ge 60.)
           shindex=where(alat le -60.)
        endif
;
; PW1 at 60N/60S
; at each latitude and level
;
        nh60=where(abs(alat-60.) eq min(abs(alat-60.)))
        sh60=where(abs(alat+60.) eq min(abs(alat+60.)))
        for kk = 0L, nth-1L do begin
            level  = reform(z[nh60(0),*,kk])		; GPH at 60N
            x = where(level le 0.,nx)
            if nx gt 0L then level[x] = !values.f_nan
            goodindex = where(level gt 0. and level lt 100000., nz)
            if nz gt 10. then begin
               X = alon[goodindex]
               Y = level[goodindex]
               weights = Y*0. + 1. ; Define a vector of weights.
               levelmean = mean(Y,/nan)
               A0guess = max(smooth(y,5,/nan,/edge_truncate),/nan) - levelmean
               ysmooth = smooth(y,7,/nan,/edge_truncate)
               dum = min(ysmooth,/nan,A1guess_index)
               A1guess = (!dtor*X[A1guess_index[0]] + !Pi/2.) mod (2.*!Pi) ; guess the phase in radians, set equal to where the minimum in GPH is plus pi/2 radians. (- n_elements(longrid)) makes use of negative indices
               A = [A0guess,A1guess, levelmean] ; Provide an initial guess of the function's parameters.
               yfit = CURVEFIT(X, Y, weights, A, SIGMA, FUNCTION_NAME='wave1')
               pw1_on_th_2d_nh(iday,kk) = (abs(A[0]))
            endif

            level  = reform(z[sh60(0),*,kk])            ; GPH at 60S
            x = where(level le 0.,nx)
            if nx gt 0L then level[x] = !values.f_nan
            goodindex = where(level gt 0. and level lt 100000., nz)
            if nz gt 10. then begin
               X = alon[goodindex]
               Y = level[goodindex]
               weights = Y*0. + 1. ; Define a vector of weights.
               levelmean = mean(Y,/nan)
               A0guess = max(smooth(y,5,/nan,/edge_truncate),/nan) - levelmean
               ysmooth = smooth(y,7,/nan,/edge_truncate)
               dum = min(ysmooth,/nan,A1guess_index)
               A1guess = (!dtor*X[A1guess_index[0]] + !Pi/2.) mod (2.*!Pi) ; guess the phase in radians, set equal to where the minimum in GPH is plus pi/2 radians. (- n_elements(longrid)) makes use of negative indices
               A = [A0guess,A1guess, levelmean] ; Provide an initial guess of the function's parameters.
               yfit = CURVEFIT(X, Y, weights, A, SIGMA, FUNCTION_NAME='wave1')
               pw1_on_th_2d_sh(iday,kk) = (abs(A[0]))
            endif
;print,pw1_on_th_2d_nh(iday,kk),pw1_on_th_2d_sh(iday,kk)
        endfor
;
; loop over altitude and calculate area of the vortex in both hemispheres
;
        for kk=0L,nth-1L do begin
            marklev=transpose(MARK(*,*,kk))
            index=where(marklev gt 0. and y2d gt 0.)
            varea=0.
            if index(0) ne -1L then markarea_on_th_2d_nh(iday,kk)=100.*total(area(index))/hem_area
        endfor

        for kk=0L,nth-1L do begin
            marklev=transpose(MARK(*,*,kk))
            index=where(marklev gt 0. and y2d lt 0.)
            varea=0.
            if index(0) ne -1L then markarea_on_th_2d_sh(iday,kk)=100.*total(area(index))/hem_area
        endfor
;
; corresponding polar cap average P and Z
;
           dum=mean(z(nhindex,*,*),dim=1)
           z_on_th_2d_nh(iday,*)=mean(dum,dim=1)
           dum=mean(p(nhindex,*,*),dim=1)
           p_on_th_2d_nh(iday,*)=mean(dum,dim=1)

           dum=mean(z(shindex,*,*),dim=1)
           z_on_th_2d_sh(iday,*)=mean(dum,dim=1)
           dum=mean(p(shindex,*,*),dim=1)
           p_on_th_2d_sh(iday,*)=mean(dum,dim=1)
        endfor	; loop over files
;
; save daily mean of all years
;
    print,'saving '+ofile
    save,filename=ofile,nth,th,ndays,mmdd,markarea_on_th_2d_nh,z_on_th_2d_nh,p_on_th_2d_nh,markarea_on_th_2d_sh,z_on_th_2d_sh,p_on_th_2d_sh,pw1_on_th_2d_nh,pw1_on_th_2d_sh
    quick:
    restore,ofile

    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       !p.font=0
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='zt_NH_SH_vortexarea_smidemax_2pan_'+syear+'.ps'
       !p.charsize=1
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif

mnh=markarea_on_th_2d_nh
znh=z_on_th_2d_nh
znhshift=0.*znh
mnhshift=0.*mnh
znhshift(0:183,*)=reform(znh(181:364,*))        ; July-Dec
znhshift(184:364,*)=reform(znh(0:180,*))        ; Jan-July
mnhshift(0:183,*)=reform(mnh(181:364,*))        ; July-Dec
mnhshift(184:364,*)=reform(mnh(0:180,*))        ; Jan-July
markarea_on_th_2d_nh=mnhshift
z_on_th_2d_nh=znhshift

    smon=strmid(mmdd,0,2)
    sday=strmid(mmdd,2,2)
    xindex=where(sday eq '15',nxticks)
    xlabs=smon(xindex)
    xlabshift=[xlabs(6:-1),xlabs(0:5)]
    erase
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    tlevel=[0.01,0.1,0.5,1,1.5,2,3,4,5+5.*findgen(6)]
    nlvls=n_elements(tlevel)
    col1=(findgen(nlvls)/float(nlvls-1))*mcolor
    col1(-1)=col1(-1)-1
    index=where(markarea_on_th_2d_nh eq 0.)
    if index(0) ne -1L then markarea_on_th_2d_nh(index)=0./0.
markarea_on_th_2d_nh=smooth(markarea_on_th_2d_nh,5,/edge_truncate,/Nan)
pw1_on_th_2d_nh=smooth(pw1_on_th_2d_nh,5,/edge_truncate,/Nan)
    contour,markarea_on_th_2d_nh,findgen(ndays),z_on_th_2d_nh,/noera,/cell_fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,110],ytitle='Altitude (km)',charsize=1,charthick=2,title='NH',$
            xticks=nxticks-1,xtickname=xlabshift,xtickv=xindex
    contour,markarea_on_th_2d_nh,findgen(ndays),z_on_th_2d_nh,/noera,/follow,color=0,levels=10+10.*findgen(9),/overplot,c_labels=[0,1,1,1,1,1,1,1,1]
    contour,pw1_on_th_2d_nh,findgen(ndays),z_on_th_2d_nh,/noera,/follow,c_color=mcolor*[.1,.3,.5,.7,.9],levels=[0.5,1,2,3,4],/overplot,c_labels=[0,1,1,1,1,1,1,1,1],thick=2  ; in km

    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    index=where(markarea_on_th_2d_sh eq 0.)
    if index(0) ne -1L then markarea_on_th_2d_sh(index)=0./0.
markarea_on_th_2d_sh=smooth(markarea_on_th_2d_sh,5,/edge_truncate,/Nan)
pw1_on_th_2d_sh=smooth(pw1_on_th_2d_sh,5,/edge_truncate,/Nan)
    contour,markarea_on_th_2d_sh,findgen(ndays),z_on_th_2d_sh,/noera,/cell_fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,110],ytitle='Altitude (km)',charsize=1,charthick=2,title='SH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    contour,markarea_on_th_2d_sh,findgen(ndays),z_on_th_2d_sh,/noera,/follow,color=0,levels=10+10.*findgen(9),/overplot,c_labels=[0,1,1,1,1,1,1,1,1]
    contour,pw1_on_th_2d_sh,findgen(ndays),z_on_th_2d_nh,/noera,/follow,c_color=mcolor*[.1,.3,.5,.7,.9],levels=[0.5,1,2,3,4],/overplot,c_labels=[0,1,1,1,1,1,1,1,1],thick=2  ; in km
;
    imin=min(tlevel)
    imax=max(tlevel)
    ymnb=min(yorig) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='Polar Vortex Area (% Hem)',/noeras,charsize=1.5,charthick=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim zt_NH_SH_vortexarea_smidemax_2pan_'+syear+'.ps -rotate -90 zt_NH_SH_vortexarea_smidemax_2pan_'+syear+'.jpg
       spawn,'rm -f zt_NH_SH_vortexarea_smidemax_2pan_'+syear+'.ps'
    endif
    icount=icount+1L
endfor		; loop over years
end
