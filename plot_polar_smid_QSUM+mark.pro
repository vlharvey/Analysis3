;
; plot auroral oval and mean vortex edge
;
;
@vortexshape

loadct,39
mcolor=byte(!p.color)
icmm1=mcolor-1B
icmm2=mcolor-2B
device,decompose=0
a=findgen(4)*(2*!pi/4.)
usersym,cos(a),sin(a),/fill
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.125,0.125,0.525,0.525]
yorig=[0.55,0.15,0.55,0.15]
xlen=0.375
ylen=0.375
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
if setplot eq 'ps' then begin
   lc=0
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_smid_QSUM+mark.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=2
   !y.thick=2
   !x.thick=2
endif
;
; read in the location of the N and S magnetic poles since 1590 from NOAA model
;
close,1
openr,1,'npole-mag.txt'
count=0
dum=' '
while not eof(1) do begin
readf,1,dum
result=strsplit(dum,' ',/extract)
if count eq 1 then begin
   nhx=[nhx,float(result(0))]
   nhy=[nhy,float(result(1))]
   nhyear=[nhyear,float(result(2))]
endif
if count eq 0 then begin
   nhx=float(result(0))
   nhy=float(result(1))
   nhyear=float(result(2))
   count=1
endif
endwhile
close,1
;
count=0
openr,1,'spole-mag.txt'
while not eof(1) do begin
readf,1,dum
result=strsplit(dum,' ',/extract)
if count eq 1 then begin
   shx=[shx,float(result(0))]
   shy=[shy,float(result(1))]
   shyear=[shyear,float(result(2))]
endif
if count eq 0 then begin
   shx=float(result(0))
   shy=float(result(1))
   shyear=float(result(2))
   count=1
endif
endwhile
close,1

restore,'smidemax_300-year_TUmark_djf_jja.sav
mark2=DJF_MARK
marker_USLM = make_array(nc,nr,nth)
for k=0,nth-1 do marker_USLM(*,*,k) = transpose(mark2(*,*,k))
shape = vortexshape(marker_USLM, alat, alon)
centroid=shape.nhcentroid
centroidx=reform(centroid(0,*))
centroidy=reform(centroid(1,*))
index=where(th eq 2000.)
djfy=centroidy(index(0)) & djfx=centroidx(index(0))
; SH currently undefined

djfmark0=transpose(DJF_MARK(*,*,index(0)))
djfmark=fltarr(nc+1,nr)
djfmark(0:nc-1,0:nr-1)=djfmark0
djfmark(nc,*)=djfmark(0,*)
jjamark0=transpose(JJA_MARK(*,*,index(0)))
jjamark=fltarr(nc+1,nr)
jjamark(0:nc-1,0:nr-1)=jjamark0
jjamark(nc,*)=jjamark(0,*)
x=fltarr(nc+1)
x(0:nc-1)=alon
x(nc)=x(0)+360.

highnh='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/QSUM_CO2x1SmidEmax_yBWCN.0101.sav'
restore,highnh
dum=reform(qsum(*,*,4))	; 4.4x10-5 ~112 km
plotarray=fltarr(nc+1,nr)
plotarray(0:nc-1,0:nr-1)=dum/1000.
plotarray(nc,*)=plotarray(0,*)
erase
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
map_set,90,0,0,/ortho,/contin,/noeras,color=0
nlvls=15
level=findgen(nlvls)
level(0)=0.1
nlvls=n_elements(level)
col1=(findgen(nlvls)/float(nlvls))*mcolor
;col1(0)=mcolor
contour,plotarray,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,/overplot
contour,djfmark,x,alat,levels=0.1,thick=20,color=0,/overplot,/noeras
map_set,90,0,0,/ortho,/contin,/noeras,color=0
a=findgen(4)*(2*!pi/4.)
usersym,cos(a),sin(a),/fill
oplot,[djfx,djfx],[djfy,djfy],psym=8,symsize=3,color=0
a=findgen(5)*(2*!pi/4.)
usersym,cos(a),sin(a)
oplot,[djfx,djfx],[djfy,djfy],psym=8,symsize=3,color=mcolor*.9,thick=3
a=findgen(4)*(2*!pi/4.)
usersym,cos(a),sin(a),/fill
oplot,nhx,nhy,psym=3,color=195
index=where(nhyear eq 2000.)
oplot,nhx(index),nhy(index),psym=8,color=195,symsize=3

highsh='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/QSUM_CO2x1SmidEmax_yBWCN.0701.sav'
restore,highsh
dum=reform(qsum(*,*,4)) ; 4.4x10-5 ~112 km
plotarray=fltarr(nc+1,nr)
plotarray(0:nc-1,0:nr-1)=dum/1000.
plotarray(nc,*)=plotarray(0,*)
!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
map_set,-90,0,0,/ortho,/contin,/noeras,color=0
contour,plotarray,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,/overplot
contour,jjamark,x,alat,levels=0.1,thick=20,color=0,/overplot,/noeras
map_set,-90,0,0,/ortho,/contin,/noeras,color=0
oplot,shx,shy,psym=3,color=195
stop
index=where(shyear eq 2000.)
oplot,shx(index),shy(index),psym=8,color=195,symsize=3

lownh='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmin_yBWCN/QSUM_CO2x1SmidEmin_yBWCN.0101.sav'
restore,lownh
dum=reform(qsum(*,*,4)) ; 4.4x10-5 ~112 km
plotarray=fltarr(nc+1,nr)
plotarray(0:nc-1,0:nr-1)=dum/1000.
plotarray(nc,*)=plotarray(0,*)
!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
map_set,90,0,0,/ortho,/contin,/noeras,color=0
contour,plotarray,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,/overplot
contour,djfmark,x,alat,levels=0.1,thick=20,color=0,/overplot,/noeras
map_set,90,0,0,/ortho,/contin,/noeras,color=0
a=findgen(4)*(2*!pi/4.)
usersym,cos(a),sin(a),/fill
oplot,[djfx,djfx],[djfy,djfy],psym=8,symsize=3,color=0
a=findgen(5)*(2*!pi/4.)
usersym,cos(a),sin(a)
oplot,[djfx,djfx],[djfy,djfy],psym=8,symsize=3,color=mcolor*.9,thick=3
a=findgen(4)*(2*!pi/4.)
usersym,cos(a),sin(a),/fill

lowsh='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmin_yBWCN/QSUM_CO2x1SmidEmin_yBWCN.0701.sav'
restore,lowsh
dum=reform(qsum(*,*,4)) ; 4.4x10-5 ~112 km
plotarray=fltarr(nc+1,nr)
plotarray(0:nc-1,0:nr-1)=dum/1000.
plotarray(nc,*)=plotarray(0,*)
!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
map_set,-90,0,0,/ortho,/contin,/noeras,color=0
contour,plotarray,x,alat,levels=level,/cell_fill,c_color=col1,/noeras,/overplot
contour,jjamark,x,alat,levels=0.1,thick=20,color=0,/overplot,/noeras
map_set,-90,0,0,/ortho,/contin,/noeras,color=0

!type=2^2+2^3+2^6
x0=xorig(0)
x1=xorig(2)+xlen
y0=ymn-0.05
y1=ymn-0.01
nlvls  = n_elements(level)
col1 = (1 + indgen(nlvls)) * 255. / nlvls    ; define colors
slab=' '+strarr(nlvls)
plot,[0,0],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls-1L,$
        position = [x0,y0,x1,y1],xstyle=1,xtickname=slab,/nodata,color=0
xyouts,(x0+x1)/3.,y0-0.04,'Total ion production rate (cm!u3!ns!u-1!n)',color=0,charsize=1.25,charthick=2,/normal
slab=strcompress(string(format='(f4.1)',level),/remove_all)
slabcolor = fltarr(nlvls)*0.
slabcolor[0:4] = 255        ; set first few labels to white so they are visible
ybox=[0,10,10,0,0]
x2=0
dx= 1./(nlvls-1.)
x1=dx/2 ; center of first color level
for j=0,nlvls-2 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1[j]
    x2=x2+dx
    i=j
    xyouts,x1-dx/2.,5,slab(i),charsize=1.3,/data,color=slabcolor[i], orientation= -90.,align = .5 ; This should place the label on the left side of each color level
    x1=x1+dx
endfor


;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim polar_smid_QSUM+mark.ps -rotate -90 polar_smid_QSUM+mark.jpg'
;      spawn,'rm -f polar_smid_QSUM+mark.ps'
    endif
end
