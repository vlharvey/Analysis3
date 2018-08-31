;
; WACCM seasonal mean GPH of THETA 
;
!noeras=-1
loadct,39
mcolor=byte(!p.color)
icolmax=byte(!p.color)
icolmax=fix(icolmax)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,2*cos(a),2*sin(a),/fill
xorig=[0.05,0.55]
yorig=[0.3,0.3]
xlen=0.425
ylen=0.425
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
;
; restore 300-year seasonal averages
; ALAT            FLOAT     = Array[96]
; ALON            FLOAT     = Array[144]
; DJF_MARK        FLOAT     = Array[96, 144, 22]
; DJF_PRESS       FLOAT     = Array[22]
; DJF_TEMP        FLOAT     = Array[96, 144, 22]
; DJF_U           FLOAT     = Array[96, 144, 22]
; DJF_Z           FLOAT     = Array[96, 144, 22]
; JJA_MARK        FLOAT     = Array[96, 144, 22]
; JJA_PRESS       FLOAT     = Array[22]
; JJA_TEMP        FLOAT     = Array[96, 144, 22]
; JJA_U           FLOAT     = Array[96, 144, 22]
; JJA_Z           FLOAT     = Array[96, 144, 22]
; NC              LONG      =          144
; NR              LONG      =           96
; NTH             LONG      =           22
; TH              FLOAT     = Array[22]
;
restore,'smidemax_300-year_TUmark_djf_jja.sav
x=fltarr(nc+1)
x(0:nc-1)=alon(0:nc-1)
x(nc)=alon(0)+360.
y2d=fltarr(nc+1,nr)
for i=0L,nc do y2d(i,*)=alat
;
; select theta levels to plot
;
rth=80000.
print,th
read,'Enter desired theta level ',rth
index=where(th eq rth)
ilev=index(0)
sth=strcompress(long(rth),/r)+'K'
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
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_waccm_zth_djf_jja_smidemax.ps'
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
dum1=transpose(jja_mark(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)

udum1=transpose(jja_u(*,*,ilev))
udum2=fltarr(nc+1,nr)
udum2(0:nc-1,0:nr-1)=udum1
udum2(nc,*)=udum2(0,*)

zdum1=transpose(jja_z(*,*,ilev))			; strip out level
zdum2=fltarr(nc+1,nr)
zdum2(0:nc-1,0:nr-1)=zdum1
zdum2(nc,*)=zdum2(0,*)
index=where(finite(zdum2) eq 1 and y2d lt 0)
imin=min(zdum2(index))
imax=max(zdum2(index))
plevel=imin+((imax-imin)/float(nlvls-1))*findgen(nlvls)
ihem=-1
irot=135
MAP_SET,ihem*90,0,irot,/ortho,/contin,/grid,/noeras,color=0,/noborder,title='JJA'
contour,zdum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,udum2,x,alat,levels=10+10*findgen(20),/foll,c_color=0,/noeras,/overplot,thick=3
contour,udum2,x,alat,levels=-200+10*findgen(20),/foll,c_color=mcolor,/noeras,/overplot,thick=3
contour,dum2,x,alat,levels=[0.1],/foll,c_color=0,/noeras,/overplot,thick=10
contour,dum2,x,alat,levels=[-0.1],/foll,c_color=mcolor,/noeras,/overplot,thick=10
map_set,ihem*90,0,irot,/ortho,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(0) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=sth+' GPH (km)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
dum1=transpose(djf_mark(*,*,ilev))
dum2=fltarr(nc+1,nr)
dum2(0:nc-1,0:nr-1)=dum1
dum2(nc,*)=dum2(0,*)

udum1=transpose(djf_u(*,*,ilev))
udum2=fltarr(nc+1,nr)
udum2(0:nc-1,0:nr-1)=udum1
udum2(nc,*)=udum2(0,*)

zdum1=transpose(djf_z(*,*,ilev))
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
MAP_SET,ihem*90,0,irot,/ortho,/contin,/grid,/noeras,color=0,/noborder,title='DJF'
contour,zdum2,x,alat,levels=plevel,/cell_fill,c_color=col1,/noeras,/overplot
contour,udum2,x,alat,levels=10+10*findgen(20),/foll,c_color=0,/noeras,/overplot,thick=3
contour,udum2,x,alat,levels=-200+10*findgen(20),/foll,c_color=mcolor,/noeras,/overplot,thick=3
contour,dum2,x,alat,levels=[0.1],/foll,c_color=0,/noeras,/overplot,thick=10
contour,dum2,x,alat,levels=[-0.1],/foll,c_color=mcolor,/noeras,/overplot,thick=10
MAP_SET,ihem*90,0,irot,/ortho,/contin,/grid,/noeras,color=0,/noborder
ymnb=yorig(0) -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.04,xmx-0.04,ymnb,ymxb
!type=2^2+2^3+2^6
imin=min(plevel)
imax=max(plevel)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,charsize=1,xtitle=sth+' GPH (km)'
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
       spawn,'convert -trim polar_waccm_zth_djf_jja_smidemax.ps -rotate -90 polar_waccm_zth_djf_jja_smidemax.jpg'
;      spawn,'rm -f polar_waccm_zth_djf_jja_smidemax.ps'
    endif
end
