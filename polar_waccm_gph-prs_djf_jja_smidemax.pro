;
; WACCM seasonal mean GPH on PRESSURE
;
!noeras=-1
loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,2*cos(a),2*sin(a),/fill
xorig=[0.05,0.55,0.05,0.55]
yorig=[0.55,0.55,0.1,0.1]
xlen=0.4
ylen=0.4
cbaryoff=0.02
cbarydel=0.01
nxdim=800
nydim=800
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
RADG = !PI / 180.
FAC20 = 1.0 / TAN(45.*RADG)

goto,quick
;
; restore monthly means
;
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
spawn,'ls '+dir+'???12_prs.sav',decfiles
spawn,'ls '+dir+'???01_prs.sav',janfiles
spawn,'ls '+dir+'???02_prs.sav',febfiles
ifiles=[decfiles,janfiles,febfiles]
nfiles=n_elements(ifiles)
for ii=0L,nfiles-1L do begin
    restore,ifiles(ii)
    print,ifiles(ii)
    if ii eq 0L then begin
       co_djf=COAVG
       t_djf=TAVG
       u_djf=UAVG
       v_djf=VAVG
       z_djf=ZAVG
    endif
    if ii gt 0L then begin
       co_djf=co_djf+COAVG
       t_djf=t_djf+TAVG
       u_djf=u_djf+UAVG
       v_djf=v_djf+VAVG
       z_djf=z_djf+ZAVG
    endif
endfor
co_djf=co_djf/float(nfiles)
t_djf=t_djf/float(nfiles)
u_djf=u_djf/float(nfiles)
v_djf=v_djf/float(nfiles)
z_djf=z_djf/float(nfiles)

spawn,'ls '+dir+'???06_prs.sav',junfiles
spawn,'ls '+dir+'???07_prs.sav',julfiles
spawn,'ls '+dir+'???08_prs.sav',augfiles
ifiles=[junfiles,julfiles,augfiles]
nfiles=n_elements(ifiles)
for ii=0L,nfiles-1L do begin
    restore,ifiles(ii)
    print,ifiles(ii)
    if ii eq 0L then begin
       co_jja=COAVG
       t_jja=TAVG
       u_jja=UAVG
       v_jja=VAVG
       z_jja=ZAVG
    endif
    if ii gt 0L then begin
       co_jja=co_jja+COAVG
       t_jja=t_jja+TAVG
       u_jja=u_jja+UAVG
       v_jja=v_jja+VAVG
       z_jja=z_jja+ZAVG
    endif
endfor
co_jja=co_jja/float(nfiles)
t_jja=t_jja/float(nfiles)
u_jja=u_jja/float(nfiles)
v_jja=v_jja/float(nfiles)
z_jja=z_jja/float(nfiles)

save,file='smidemax_300-year_TUVZ_djf_jja.sav',nc,nr,nl,alon,alat,lev,co_djf,t_djf,u_djf,v_djf,z_djf,co_jja,t_jja,u_jja,v_jja,z_jja
quick:
restore,'smidemax_300-year_TUVZ_djf_jja.sav
x=fltarr(nc+1)
x(0:nc-1)=alon(0:nc-1)
x(nc)=alon(0)+360.
y2d=fltarr(nc+1,nr)
for i=0L,nc do y2d(i,*)=alat
;
; select theta levels to plot
; loop over levels
;
for kk=0L,nl-1L do begin

rlev=lev(kk)	;0.00145846
;print,lev
;read,'Enter desired pressure level ',rlev
index=where(abs(lev-rlev) eq min(abs(lev-rlev)))
ilev=index(0)
slev=strcompress(rlev,/r)+'hPa'
;
; save postscript version
;
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_waccm_gph-prs_djf_jja_smidemax_'+slev+'.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif

erase
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlvls=21
col1=10+(indgen(nlvls)/float(nlvls))*mcolor
dum1=reform(co_jja(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)

udum1=reform(u_jja(*,*,ilev))
udum2=fltarr(nc+1,nr)
udum2(0:nc-1,0:nr-1)=udum1
udum2(nc,*)=udum2(0,*)

zdum1=reform(z_jja(*,*,ilev))			; strip out level
zdum2=fltarr(nc+1,nr)
zdum2(0:nc-1,0:nr-1)=zdum1
zdum2(nc,*)=zdum2(0,*)
index=where(finite(zdum2) eq 1 and y2d lt 0)
imin=min(zdum2(index))
imax=max(zdum2(index))
plevel=imin+((imax-imin)/float(nlvls-1))*findgen(nlvls)
ihem=-1
irot=135
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder,title='JJA'
contour,zdum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,udum2,x,alat,levels=10+10*findgen(20),/foll,c_color=0,/noeras,/overplot,thick=3
contour,udum2,x,alat,levels=-200+10*findgen(20),/foll,c_color=mcolor,/noeras,/overplot,thick=3
map_set,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(0) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=slev+' GPH (km)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
dum1=reform(co_djf(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)

udum1=reform(u_djf(*,*,ilev))
udum2=fltarr(nc+1,nr)
udum2(0:nc-1,0:nr-1)=udum1
udum2(nc,*)=udum2(0,*)

zdum1=reform(z_djf(*,*,ilev))
zdum2=fltarr(nc+1,nr)
zdum2(0:nc-1,0:nr-1)=zdum1
zdum2(nc,*)=zdum2(0,*)
;zdum2=smooth(zdum2,3,/Nan,/edge_truncate)
index=where(zdum2 eq 0.)
if index(0) ne -1L then zdum2(index)=0./0.
index=where(finite(zdum2) eq 1 and y2d gt 0)
ihem=1
imin=min(zdum2(index))
imax=max(zdum2(index))
plevel=imin+((imax-imin)/float(nlvls))*findgen(nlvls)
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder,title='DJF'
contour,zdum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,udum2,x,alat,levels=10+10*findgen(20),/foll,c_color=0,/noeras,/overplot,thick=3
contour,udum2,x,alat,levels=-200+10*findgen(20),/foll,c_color=mcolor,/noeras,/overplot,thick=3
;contour,dum2,x,alat,levels=10+findgen(20),/foll,c_color=mcolor*.9,/noeras,/overplot,thick=3
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(0) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=slev+' GPH (km)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlvls=21
col1=10+(indgen(nlvls)/float(nlvls))*mcolor
dum1=reform(co_jja(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)
index=where(finite(dum2) eq 1 and y2d lt 0)
imin=min(dum2(index))
imax=max(dum2(index))
plevel=imin+((imax-imin)/float(nlvls-1))*findgen(nlvls)
ihem=-1
irot=135
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder,title='JJA'
contour,dum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,dum2,x,alat,levels=plevel,/foll,c_color=0,/noeras,/overplot,thick=1
map_set,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(2) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=slev+' CO (ppmv)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
dum1=reform(co_djf(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)
index=where(dum2 eq 0.)
if index(0) ne -1L then dum2(index)=0./0.
index=where(finite(dum2) eq 1 and y2d gt 0)
ihem=1
imin=min(dum2(index))
imax=max(dum2(index))
plevel=imin+((imax-imin)/float(nlvls))*findgen(nlvls)
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder,title='DJF'
contour,dum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,dum2,x,alat,levels=plevel,/foll,c_color=0,/noeras,/overplot,thick=1
MAP_SET,ihem*90,0,irot,/stereo,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(3) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=slev+' CO (ppmv)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device,/close
       spawn,'convert -trim polar_waccm_gph-prs_djf_jja_smidemax_'+slev+'.ps -rotate -90 polar_waccm_gph-prs_djf_jja_smidemax_'+slev+'.jpg'
;      spawn,'rm -f polar_waccm_gph-prs_djf_jja_smidemax_'+slev+'.ps'
    endif

endfor	; loop over levels
end
