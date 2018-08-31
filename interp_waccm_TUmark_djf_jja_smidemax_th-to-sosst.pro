;
; interpolate WACCM means from theta to SOSST-type altitude grid (but with 141 levels)
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

restore,'smidemax_300-year_TUmark_djf_jja.sav'		; nc,nr,nth,alon,alat,th,djf_mark,jja_mark,djf_press,jja_press,djf_u,jja_u,djf_temp,jja_temp,djf_z,jja_z
nz=141
altitude=1+findgen(nz)
t_jja_z=fltarr(nc,nr,nz)
u_jja_z=fltarr(nc,nr,nz)
mark_jja_z=fltarr(nc,nr,nz)
t_djf_z=fltarr(nc,nr,nz)
u_djf_z=fltarr(nc,nr,nz)
mark_djf_z=fltarr(nc,nr,nz)
for j=0L,nr-1L do begin
for i=0L,nc-1L do begin
    tprof=reform(jja_temp(j,i,*))
    uprof=reform(jja_u(j,i,*))
    markprof=reform(jja_mark(j,i,*))
    zprof=reform(jja_z(j,i,*))
    t_jja_z(i,j,*)=interpol(tprof,zprof,altitude)
    u_jja_z(i,j,*)=interpol(uprof,zprof,altitude)
    mark_jja_z(i,j,*)=interpol(markprof,zprof,altitude)
bad=where(altitude gt max(zprof) or altitude lt min(zprof))
if bad(0) ne -1L then begin
   t_jja_z(i,j,bad)=0./0.
   u_jja_z(i,j,bad)=0./0.
   mark_jja_z(i,j,bad)=0./0.
endif
;erase
;plot,tprof,zprof,thick=3,color=0,yrange=[0,140],xrange=[0,1000]
;oplot,t_jja_z(i,j,*),altitude,psym=4,color=250
;stop
    tprof=reform(djf_temp(j,i,*))
    uprof=reform(djf_u(j,i,*))
    markprof=reform(djf_mark(j,i,*))
    zprof=reform(djf_z(j,i,*))
    t_djf_z(i,j,*)=interpol(tprof,zprof,altitude)
    u_djf_z(i,j,*)=interpol(uprof,zprof,altitude)
    mark_djf_z(i,j,*)=interpol(markprof,zprof,altitude)
bad=where(altitude gt max(zprof) or altitude lt min(zprof))
if bad(0) ne -1L then begin
   t_djf_z(i,j,bad)=0./0.
   u_djf_z(i,j,bad)=0./0.
   mark_djf_z(i,j,bad)=0./0.
endif

endfor
endfor
;
; save
;
save,file='smidemax_300-year_TUmark_djf_jja_sosst.sav',nc,nr,nz,alon,alat,altitude,mark_djf_z,t_djf_z,u_djf_z,mark_jja_z,t_jja_z,u_jja_z

erase
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
nlvls=21
col1=10+(indgen(nlvls)/float(nlvls))*mcolor
tdum=mean(jja_temp,dim=2)
mdum=mean(jja_mark,dim=2)
zdum=mean(jja_z,dim=2)
tlevel=min(tdum)+((max(tdum)-min(tdum))/nlvls)*findgen(nlvls)
contour,tdum,alat,zdum,levels=tlevel,c_color=col1,/noeras,xrange=[-90,90],yrange=[0,140],ytitle='Altitude (km)',/fill,color=0
contour,mdum,alat,zdum,levels=0.1*findgen(10),/follow,color=255,thick=3,/overplot

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
tdum=mean(t_jja_z,dim=1)
mdum=mean(mark_jja_z,dim=1)
contour,tdum,alat,altitude,levels=tlevel,c_color=col1,/noeras,xrange=[-90,90],yrange=[0,140],ytitle='Altitude (km)',/cell_fill,color=0
contour,mdum,alat,altitude,levels=0.1*findgen(10),/follow,color=255,thick=3,/overplot

end
