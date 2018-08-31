;
; ZT 300-year average annual cycle of vortex and anticyclone areas (poleward of 30 latitude)
; superimpose stratopause and mesopause
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
xorig=[0.1,0.4,0.7,0.1,0.4,0.7]
yorig=[0.575,0.575,0.575,0.15,0.15,0.15]
xlen=0.25
ylen=0.3
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
dira='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/AREA_CO2x1SmidEmax_yBWCN_'
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
    restore,dira+mmdd(iday)+'.sav'

    if iday eq 0L then begin
       tnh=fltarr(ndays,nth)
       unh=fltarr(ndays,nth)
       mnh=fltarr(ndays,nth)
       hareamean_ztnh=fltarr(ndays,nth)	; anticyclone area poleward of 30N
       vareamean_ztnh=fltarr(ndays,nth)	; Arctic vortex area 
       znh=fltarr(ndays,nth)
       tsh=fltarr(ndays,nth)
       ush=fltarr(ndays,nth)
       msh=fltarr(ndays,nth)
       hareamean_ztsh=fltarr(ndays,nth)	; anticyclone area poleward of 30S
       vareamean_ztsh=fltarr(ndays,nth)	; Antarctic vortex area 
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
; means of areas
;
    index=where(harea_ztnh eq 0.)
    if index(0) ne -1L then harea_ztnh(index)=0./0.
    index=where(harea_ztsh eq 0.)
    if index(0) ne -1L then harea_ztsh(index)=0./0.
    index=where(varea_ztnh eq 0.)
    if index(0) ne -1L then varea_ztnh(index)=0./0.
    index=where(varea_ztsh eq 0.)
    if index(0) ne -1L then varea_ztsh(index)=0./0.

    hareamean_ztnh(iday,*)=median(harea_ztnh,dim=1)
    hareamean_ztsh(iday,*)=median(harea_ztsh,dim=1)
    vareamean_ztnh(iday,*)=median(varea_ztnh,dim=1)
    vareamean_ztsh(iday,*)=median(varea_ztsh,dim=1)
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
save,file='zt_TUMarkarea_NH_SH_smidemax.sav',th,mmdd,tnh,unh,mnh,znh,tsh,ush,msh,zsh,ndays,nth,vareamean_ztnh,hareamean_ztnh,vareamean_ztsh,hareamean_ztsh
quick:
restore,'zt_TUMarkarea_median_NH_SH_smidemax.sav'
restore,'zt_RCetal_NH_SH_smidemax.sav'		; ,th,mmdd,tnh,unh,mnh,znh,tsh,ush,msh,zsh,ndays,nth,ekgwspecnh,omeganh,qjoulenh,qaurnh,$
                                                ;  ttgwnh,utgwspecnh,wstarnh,vstarnh,wstarznh,ekgwspecsh,omegash,qjoulesh,qaursh,ttgwsh,utgwspecsh,wstarsh,vstarsh,wstarzsh
restore,'zt_ZMCHEM_NH_SH_smidemax.sav'		; mmdd,ndays,clono2nh,conh,noxnh,noynh,nonh,o3nh,z3nh,clono2sh,cosh0,noxsh,noysh,nosh,o3sh,z3sh
;
; ignore areas that are smaller than 85 degrees elat
;
index=where(vareamean_ztnh gt 85.)
if index(0) ne -1L then vareamean_ztnh(index)=0./0.
index=where(hareamean_ztnh gt 85.)
if index(0) ne -1L then hareamean_ztnh(index)=0./0.
index=where(vareamean_ztsh gt 85.)
if index(0) ne -1L then vareamean_ztsh(index)=0./0.
index=where(hareamean_ztsh gt 85.)
if index(0) ne -1L then hareamean_ztsh(index)=0./0.
;
; smooth
;
vareamean_ztnh=smooth(vareamean_ztnh,1,/Nan,/edge_truncate)
hareamean_ztnh=smooth(hareamean_ztnh,1,/Nan,/edge_truncate)
vareamean_ztsh=smooth(vareamean_ztsh,1,/Nan,/edge_truncate)
hareamean_ztsh=smooth(hareamean_ztsh,1,/Nan,/edge_truncate)
;
; postscript file
;
    if setplot eq 'ps' then begin
       !p.font=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='zt_RC-CO+pauses_NH_SH_smidemax.ps'
       !p.charsize=1.1
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; shift NH winter to middle of the plot
;
vstarnh_shift=0*vstarnh
wstarnh_shift=0*wstarnh
wstarznh_shift=0*wstarznh
tnh_shift=0*tnh
znh_shift=0*znh
vareamean_ztnh_shift=0*vareamean_ztnh
hareamean_ztnh_shift=0*hareamean_ztnh
conh_shift=0*conh
z3nh_shift=0*z3nh

index0=where(mmdd ge '0701')
index1=where(mmdd lt '0701')

vstarnh_shift(0:n_elements(index0)-1,*)=vstarnh(index0,*)
vstarnh_shift(n_elements(index0):-1,*)=vstarnh(index1,*)
wstarnh_shift(0:n_elements(index0)-1,*)=wstarnh(index0,*)
wstarnh_shift(n_elements(index0):-1,*)=wstarnh(index1,*)
wstarznh_shift(0:n_elements(index0)-1,*)=wstarznh(index0,*)
wstarznh_shift(n_elements(index0):-1,*)=wstarznh(index1,*)
tnh_shift(0:n_elements(index0)-1,*)=tnh(index0,*)
tnh_shift(n_elements(index0):-1,*)=tnh(index1,*)
znh_shift(0:n_elements(index0)-1,*)=znh(index0,*)
znh_shift(n_elements(index0):-1,*)=znh(index1,*)
vareamean_ztnh_shift(0:n_elements(index0)-1,*)=vareamean_ztnh(index0,*)
vareamean_ztnh_shift(n_elements(index0):-1,*)=vareamean_ztnh(index1,*)
hareamean_ztnh_shift(0:n_elements(index0)-1,*)=hareamean_ztnh(index0,*)
hareamean_ztnh_shift(n_elements(index0):-1,*)=hareamean_ztnh(index1,*)
conh_shift(0:n_elements(index0)-1,*)=conh(index0,*)
conh_shift(n_elements(index0):-1,*)=conh(index1,*)
z3nh_shift(0:n_elements(index0)-1,*)=z3nh(index0,*)
z3nh_shift(n_elements(index0):-1,*)=z3nh(index1,*)

vstarnh=vstarnh_shift
wstarnh=wstarnh_shift
wstarznh=wstarznh_shift
tnh=tnh_shift
znh=znh_shift
vareamean_ztnh=vareamean_ztnh_shift
hareamean_ztnh=hareamean_ztnh_shift
conh=conh_shift
z3nh=z3nh_shift

mmdd0=strarr(ndays)
mmdd0(0:n_elements(index0)-1)=mmdd(index0)
mmdd0(n_elements(index0):-1)=mmdd(index1)
;
; plot
;
x2d=0.*tnh
for k=0L,nth-1L do x2d(*,k)=findgen(ndays)
smon0=strmid(mmdd0,0,2)
sday0=strmid(mmdd0,2,2)
xindex0=where(long(smon0) mod 2 eq 0 and sday0 eq '15',nxticks0)
xlabs0=smon0(xindex0)

smon=strmid(mmdd,0,2)
sday=strmid(mmdd,2,2)
xindex=where(long(smon) mod 2 eq 0 and sday eq '15',nxticks)
xlabs=smon(xindex)
erase
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlvls=21
tlevel=4*findgen(nlvls)
tlevel(0)=0.1
nlvls1=n_elements(tlevel)
col2=12+(findgen(nlvls1)/float(nlvls1))*mcolor
;
; superimpose stratopause and mesopause
;
; interpolate temp to constant height
;
altitude=30+findgen(96)
nz=n_elements(altitude)
tnhz=fltarr(ndays,nz)
for i=0L,ndays-1L do tnhz(i,*)=interpol(reform(tnh(i,*)),reform(znh(i,*)),altitude)
contour,conh*1.e6,findgen(ndays),z3nh,/noera,/fill,color=0,c_color=col2,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.25,charthick=2,title='NH CO',$
        xticks=nxticks0-1,xtickname=xlabs0,xtickv=xindex0
contour,conh*1.e6,findgen(ndays),z3nh,/noera,/foll,color=mcolor,levels=[1,5,25],/overplot,thick=3
;contour,reform(wstarnh(*,0:-2))*1000.,findgen(ndays),wstarznh,/noera,/foll,levels=[0],color=0,/overplot

zstratnh=fltarr(ndays)
index=where(altitude lt 95.)
alt0=reform(altitude(index))
for i=0L,ndays-1L do begin
    tprof=smooth(reform(tnhz(i,*)),7,/edge_truncate)
    index=where(altitude lt 95.)
    tprof0=reform(tprof(index))
    index=where(tprof0 eq max(tprof0))
    if index(0) ne -1L then zstratnh(i)=alt0(index(0))
endfor

zmesonh=fltarr(ndays)
index=where(altitude gt 50.)
alt0=reform(altitude(index))
for i=0L,ndays-1L do begin
    tprof=smooth(reform(tnhz(i,*)),3,/edge_truncate)
    index=where(altitude gt 50.)
    tprof0=reform(tprof(index))
    index=where(tprof0 eq min(tprof0))
    if index(0) ne -1L then zmesonh(i)=alt0(index(0))
endfor

loadct,0
oplot,findgen(ndays),zstratnh,color=0,psym=8
oplot,findgen(ndays),zmesonh,color=200,psym=8
loadct,39

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
vlevel=-20+2*findgen(20)
vnlvls=n_elements(vlevel)
vcol1=(findgen(vnlvls)/float(vnlvls))*mcolor

wlevel=-50+5.*findgen(20)
wnlvls=n_elements(wlevel)
wcol1=(findgen(wnlvls)/float(wnlvls))*mcolor

contour,reform(vstarnh(*,0:-2)),findgen(ndays),wstarznh,/noera,/cell_fill,color=0,c_color=vcol1,levels=vlevel,thick=10,xrange=[0,ndays-1],yrange=[30,125],charsize=1.25,charthick=2,title='NH Vstar',$
        xticks=nxticks0-1,xtickname=xlabs0,xtickv=xindex0
contour,reform(vstarnh(*,0:-2)),findgen(ndays),wstarznh,/noera,/foll,levels=1+findgen(10),color=0,/overplot
contour,reform(vstarnh(*,0:-2)),findgen(ndays),wstarznh,/noera,/foll,levels=-20+2*findgen(10),color=mcolor,/overplot

loadct,0
;contour,vareamean_ztnh,findgen(ndays),znh,/noera,/foll,color=150,levels=[80],/overplot,thick=5
oplot,findgen(ndays),zstratnh,color=0,psym=8
oplot,findgen(ndays),zmesonh,color=200,psym=8
loadct,39

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,reform(wstarnh(*,0:-2))*1000.,findgen(ndays),wstarznh,/noera,/cell_fill,color=0,c_color=wcol1,levels=wlevel,thick=10,xrange=[0,ndays-1],yrange=[30,125],charsize=1.25,charthick=2,title='NH Wstar',$
        xticks=nxticks0-1,xtickname=xlabs0,xtickv=xindex0
contour,reform(wstarnh(*,0:-2))*1000.,findgen(ndays),wstarznh,/noera,/foll,levels=5+5*findgen(20),color=0,/overplot
contour,reform(wstarnh(*,0:-2))*1000.,findgen(ndays),wstarznh,/noera,/foll,levels=-100+5*findgen(20),color=mcolor,/overplot

loadct,0
;contour,hareamean_ztnh,findgen(ndays),znh,/noera,/foll,color=150,levels=[80],/overplot,thick=5
oplot,findgen(ndays),zstratnh,color=0,psym=8
oplot,findgen(ndays),zmesonh,color=200,psym=8
loadct,39


xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
;
; superimpose stratopause and mesopause
;
; interpolate temp to constant height
;
altitude=30+findgen(96)
nz=n_elements(altitude)
tshz=fltarr(ndays,nz)
for i=0L,ndays-1L do tshz(i,*)=interpol(reform(tsh(i,*)),reform(zsh(i,*)),altitude)
contour,cosh0*1.e6,findgen(ndays),z3sh,/noera,/fill,color=0,c_color=col2,levels=tlevel,xrange=[0,ndays-1],yrange=[30,125],ytitle='Altitude (km)',charsize=1.25,charthick=2,title='SH CO',$
        xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
contour,cosh0*1.e6,findgen(ndays),z3sh,/noera,/foll,color=mcolor,levels=[1,5,25],/overplot,thick=3

zstratsh=fltarr(ndays)
index=where(altitude lt 95.)
alt0=reform(altitude(index))
for i=0L,ndays-1L do begin
    tprof=smooth(reform(tshz(i,*)),7,/edge_truncate)
    index=where(altitude lt 95.)
    tprof0=reform(tprof(index))
    index=where(tprof0 eq max(tprof0))
    if index(0) ne -1L then zstratsh(i)=alt0(index(0))
endfor

zmesosh=fltarr(ndays)
index=where(altitude gt 50.)
alt0=reform(altitude(index))
for i=0L,ndays-1L do begin
    tprof=smooth(reform(tshz(i,*)),3,/edge_truncate)
    index=where(altitude gt 50.)
    tprof0=reform(tprof(index))
    index=where(tprof0 eq min(tprof0))
    if index(0) ne -1L then zmesosh(i)=alt0(index(0))
endfor
;zstratsh=smooth(zstratsh,15,/edge_truncate)
;zmesosh=smooth(zmesosh,15,/edge_truncate)

loadct,0
oplot,findgen(ndays),zstratsh,color=0,psym=8
oplot,findgen(ndays),zmesosh,color=200,psym=8
loadct,39

xmn=xorig(4)
xmx=xorig(4)+xlen
ymn=yorig(4)
ymx=yorig(4)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,reform(vstarsh(*,0:-2)),findgen(ndays),wstarzsh,/noera,/cell_fill,color=0,c_color=vcol1,levels=vlevel,thick=10,xrange=[0,ndays-1],yrange=[30,125],charsize=1.25,charthick=2,title='SH Vstar',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
contour,reform(vstarsh(*,0:-2)),findgen(ndays),wstarzsh,/noera,/foll,levels=1+findgen(10),color=0,/overplot
contour,reform(vstarsh(*,0:-2)),findgen(ndays),wstarzsh,/noera,/foll,levels=-20+2*findgen(10),color=mcolor,/overplot
loadct,0
;contour,vareamean_ztsh,findgen(ndays),zsh,/noera,/foll,color=150,levels=[80],/overplot,thick=5
oplot,findgen(ndays),zstratsh,color=0,psym=8
oplot,findgen(ndays),zmesosh,color=200,psym=8
loadct,39

xmn=xorig(5)
xmx=xorig(5)+xlen
ymn=yorig(5)
ymx=yorig(5)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,reform(wstarsh(*,0:-2))*1000.,findgen(ndays),wstarzsh,/noera,/cell_fill,color=0,c_color=wcol1,levels=wlevel,thick=10,xrange=[0,ndays-1],yrange=[30,125],charsize=1.25,charthick=2,title='SH Wstar',$
            xticks=nxticks-1,xtickname=xlabs,xtickv=xindex
contour,reform(wstarsh(*,0:-2))*1000.,findgen(ndays),wstarzsh,/noera,/foll,levels=5+5*findgen(20),color=0,/overplot
contour,reform(wstarsh(*,0:-2))*1000.,findgen(ndays),wstarzsh,/noera,/foll,levels=-100+5*findgen(20),color=mcolor,/overplot

loadct,0
;contour,hareamean_ztsh,findgen(ndays),zsh,/noera,/foll,color=150,levels=[80],/overplot,thick=5
oplot,findgen(ndays),zstratsh,color=0,psym=8
oplot,findgen(ndays),zmesosh,color=200,psym=8
loadct,39
;
; horizontal color bars
;
!type=2^2+2^3+2^6
x0=xorig(0)
x1=xorig(0)+xlen
y0=ymn-0.1
y1=ymn-0.05
slab=' '+strarr(nlvls1)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls1-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,(x0+x1)/4.,y0-0.03,'Polar Cap CO (ppmv)',color=0,charsize=1.25,charthick=2,/normal
slab=strcompress(string(format='(f4.1)',tlevel),/remove_all)
slabcolor = fltarr(nlvls1)*0.
slabcolor[0:8] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls1-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls1-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col2[j]
    x2=x2+dx
endfor
for i=0L,nlvls1-2 do begin
    if i mod 3 eq 0 then xyouts,x1-dx/2.,5,slab(i),charsize=1,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor

nlvls=vnlvls
col1=vcol1
x0=xorig(1)
x1=xorig(1)+xlen
y0=ymn-0.1
y1=ymn-0.05
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,x0+0.02,y0-0.03,'60N/S Vstar (m/s)',color=0,charsize=1.25,charthick=2,/normal
slab=strcompress(string(format='(i3)',long(vlevel)),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[-4:-1] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
    xyouts,x1-dx/2.,5,slab(i),charsize=1,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor

nlvls=wnlvls
col1=wcol1
x0=xorig(2)
x1=xorig(2)+xlen
y0=ymn-0.1
y1=ymn-0.05
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,x0+0.02,y0-0.03,'60-90 Wstar (mm/s)',color=0,charsize=1.25,charthick=2,/normal
slab=strcompress(string(format='(i3)',long(wlevel)),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[-4:-1] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
   xyouts,x1-dx/2.,5,slab(i),charsize=1,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor


;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim zt_RC-CO+pauses_NH_SH_smidemax.ps -rotate -90 zt_RC-CO+pauses_NH_SH_smidemax.jpg
;      spawn,'rm -f zt_RC-CO+pauses_NH_SH_smidemax.ps'
    endif

end
