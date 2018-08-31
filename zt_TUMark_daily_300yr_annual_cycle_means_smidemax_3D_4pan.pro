;
; superimpose areas
; plot average annual cycle of T, U, Mark in the polar regions
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
xorig=[0.15,0.15,0.55,0.55]
yorig=[0.6,0.15,0.6,0.15]
xlen=0.35
ylen=0.35
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)

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

goto,quick
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
; restore daily mean of all years
;
    ofile=dir+mmdd(iday)+'.sav'
    print,'reading '+ofile
    restore,ofile	;,nc,nr,nth,alon,alat,th,ipvavg,pavg,uavg,vavg,qdfavg,coavg,zavg,sfavg,mavg,$
	                   ;	ipvsig,psig,usig,vsig,qdfsig,cosig,zsig,sfsig,msig
    if iday eq 0L then begin
       tnh=fltarr(ndays,nth)
       unh=fltarr(ndays,nth)
       mnh=fltarr(ndays,nth)
       harea_ztnh=fltarr(ndays,nth)
       varea_ztnh=fltarr(ndays,nth)
       znh=fltarr(ndays,nth)
       tsh=fltarr(ndays,nth)
       ush=fltarr(ndays,nth)
       msh=fltarr(ndays,nth)
       harea_ztsh=fltarr(ndays,nth)	; anticyclone area poleward of 60S
       varea_ztsh=fltarr(ndays,nth)	; vortex area poleward of 60S
       zsh=fltarr(ndays,nth)
       rlat=60.
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       nhlat=index(0)
       rlat=-60.
       index=where(abs(alat-rlat) eq min(abs(alat-rlat)))
       shlat=index(0)

       dum=transpose(mavg(*,*,0))
       lon=0.*dum
       lat=0.*dum
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
    endif
;
; calculate temp
;
    tavg=0*pavg
    for k=0,nth-1 do tavg(*,*,k) = th(k)*( (pavg(*,*,k)/1000.)^(.286) )
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
;
; extract daily means in the polar regions
;
    tnh(iday,*)=mean(tyz(nhlat:-1,*),dim=1)
    unh(iday,*)=reform(uyz(nhlat,*))
    mnh(iday,*)=mean(myz(nhlat:-1,*),dim=1)
    znh(iday,*)=mean(zyz(nhlat:-1,*),dim=1)
    tsh(iday,*)=mean(tyz(0:shlat,*),dim=1)
    ush(iday,*)=reform(uyz(shlat,*))
    msh(iday,*)=mean(myz(0:shlat,*),dim=1)
    zsh(iday,*)=mean(zyz(0:shlat,*),dim=1)
endfor
;
; save file
;
save,file='zt_TUMark_NH_SH_smidemax.sav',th,mmdd,tnh,unh,mnh,znh,tsh,ush,msh,zsh,ndays,nth	;,varea_ztnh,harea_ztnh,varea_ztsh,harea_ztsh
quick:
restore,'zt_TUMark_NH_SH_smidemax.sav'
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='zt_TUMark_NH_SH_smidemax_4pan.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; shift NH to put winter in the middle
;
tnhshift=0.*tnh
znhshift=0.*znh
unhshift=0.*unh
mnhshift=0.*mnh
tnhshift(0:183,*)=reform(tnh(181:364,*))	; July-Dec
tnhshift(184:364,*)=reform(tnh(0:180,*))	; Jan-July
znhshift(0:183,*)=reform(znh(181:364,*))        ; July-Dec
znhshift(184:364,*)=reform(znh(0:180,*))        ; Jan-July
unhshift(0:183,*)=reform(unh(181:364,*))        ; July-Dec
unhshift(184:364,*)=reform(unh(0:180,*))        ; Jan-July
mnhshift(0:183,*)=reform(mnh(181:364,*))        ; July-Dec
mnhshift(184:364,*)=reform(mnh(0:180,*))        ; Jan-July
;
; plot
;
    x2d=0.*tnh
    for k=0L,nth-1L do x2d(*,k)=findgen(ndays)
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
    nlvls=27
    tlevel=[130+5*findgen(nlvls),280,300,320,350,400,450,500]
    nlvls=n_elements(tlevel)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,tnhshift,findgen(ndays),znhshift,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='NH',$
            xticks=nxticks-1,xtickname=xlabshift,xtickv=xindex
    contour,tnhshift,findgen(ndays),znhshift,/noera,/follow,color=0,levels=tlevel(0:-1:2),/overplot
mnhshift=smooth(mnhshift,3,/edge_truncate)
    contour,mnhshift,findgen(ndays),znhshift,/noera,/foll,color=0,thick=5,levels=[0.1,0.3,0.5,0.7,0.9],/overplot
    contour,mnhshift,findgen(ndays),znhshift,/noera,/foll,color=mcolor,thick=5,levels=[-0.9,-0.7,-0.5,-0.3,-0.1],/overplot

    !type=2^2+2^3
    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,tsh,findgen(ndays),zsh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='SH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    contour,tsh,findgen(ndays),zsh,/noera,/follow,color=0,levels=tlevel(0:-1:2),/overplot
msh=smooth(msh,3,/edge_truncate)
    contour,msh,findgen(ndays),zsh,/noera,/foll,color=0,thick=5,levels=[0.1,0.3,0.5,0.7,0.9],/overplot
    contour,msh,findgen(ndays),zsh,/noera,/foll,color=mcolor,thick=5,levels=[-0.9,-0.7,-0.5,-0.3,-0.1],/overplot
;
; horizontal color bar
;
!type=2^2+2^3+2^6
x0=xmn
x1=xmx
y0=ymn-0.1
y1=ymn-0.05
level=tlevel
nlvls  = n_elements(level)

col1 = (1 + indgen(nlvls)) * 255. / nlvls    ; define colors
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,(x0+x1)/3.,y0-0.03,'60-90 Temperature (K)',color=0,charsize=1.5,charthick=2,/normal
slab=strcompress(string(format='(i3)',long(level)),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[0:10] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
    if j mod 2 eq 0 then xyouts,x1-dx/2.,5,slab(i),charsize=1.2,charthick=2,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor
;
; plot Ubar
;
    !type=2^2+2^3
    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    nlvls=25
    tlevel=-120+10*findgen(nlvls)
    nlvls=n_elements(tlevel)
    col1=(findgen(nlvls)/float(nlvls))*mcolor
    contour,unhshift,findgen(ndays),znhshift,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],charsize=1.5,charthick=2,title='NH',$
            xticks=nxticks-1,xtickname=xlabshift,xtickv=xindex
    index=where(tlevel gt 0.)
    contour,unhshift,findgen(ndays),znhshift,/noera,/follow,color=0,levels=tlevel(index),/overplot
    index=where(tlevel lt 0.)
    contour,unhshift,findgen(ndays),znhshift,/noera,/follow,color=mcolor,levels=tlevel(index),/overplot,c_linestyle=5
    contour,mnhshift,findgen(ndays),znhshift,/noera,/foll,color=0,thick=5,levels=[0.1,0.3,0.5,0.7,0.9],/overplot
    contour,mnhshift,findgen(ndays),znhshift,/noera,/foll,color=mcolor,thick=5,levels=[-0.9,-0.7,-0.5,-0.3,-0.1],/overplot

    !type=2^2+2^3
    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    contour,ush,findgen(ndays),zsh,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],charsize=1.5,charthick=2,title='SH',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
    index=where(tlevel gt 0.)
    contour,ush,findgen(ndays),zsh,/noera,/follow,color=0,levels=tlevel(index),/overplot
    index=where(tlevel lt 0.)
    contour,ush,findgen(ndays),zsh,/noera,/follow,color=mcolor,levels=tlevel(index),/overplot,c_linestyle=5
    contour,msh,findgen(ndays),zsh,/noera,/foll,color=0,thick=5,levels=[0.1,0.3,0.5,0.7,0.9],/overplot
    contour,msh,findgen(ndays),zsh,/noera,/foll,color=mcolor,thick=5,levels=[-0.9,-0.7,-0.5,-0.3,-0.1],/overplot
;
; horizontal color bar
;
!type=2^2+2^3+2^6
x0=xmn
x1=xmx
y0=ymn-0.1
y1=ymn-0.05
level=tlevel
nlvls  = n_elements(level)

col1 = (1 + indgen(nlvls)) * 255. / nlvls    ; define colors
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,x0,y0-0.03,'    60 degree Zonal Wind (m/s)',color=0,charsize=1.5,charthick=2,/normal
slab=strcompress(string(format='(i4)',long(level)),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[0:10] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
    if j mod 2 eq 0 then xyouts,x1-dx/2.,5,slab(i),charsize=1.2,charthick=2,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor

;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim zt_TUMark_NH_SH_smidemax_4pan.ps -rotate -90 zt_TUMark_NH_SH_smidemax_4pan.jpg
;      spawn,'rm -f zt_TUMark_NH_SH_smidemax_4pan.ps'
    endif

end
