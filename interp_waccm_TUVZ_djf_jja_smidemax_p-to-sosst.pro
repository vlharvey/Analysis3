;
; interpolate WACCM means from pressure to SOSST-type altitude grid (but with 141 levels)
;
loadct,39

restore,'smidemax_300-year_TUVZ_djf_jja.sav
nz=141
altitude=1+findgen(nz)
t_jja_z=fltarr(nc,nr,nz)
u_jja_z=fltarr(nc,nr,nz)
v_jja_z=fltarr(nc,nr,nz)
co_jja_z=fltarr(nc,nr,nz)
t_djf_z=fltarr(nc,nr,nz)
u_djf_z=fltarr(nc,nr,nz)
v_djf_z=fltarr(nc,nr,nz)
co_djf_z=fltarr(nc,nr,nz)
for j=0L,nr-1L do begin
for i=0L,nc-1L do begin
    tprof=reform(t_jja(i,j,*))
    uprof=reform(u_jja(i,j,*))
    vprof=reform(v_jja(i,j,*))
    coprof=reform(co_jja(i,j,*))
    zprof=reform(z_jja(i,j,*))
    t_jja_z(i,j,*)=interpol(tprof,zprof,altitude)
    u_jja_z(i,j,*)=interpol(uprof,zprof,altitude)
    v_jja_z(i,j,*)=interpol(vprof,zprof,altitude)
    co_jja_z(i,j,*)=interpol(coprof,zprof,altitude)

    tprof=reform(t_djf(i,j,*))
    uprof=reform(u_djf(i,j,*))
    vprof=reform(v_djf(i,j,*))
    coprof=reform(co_djf(i,j,*))
    zprof=reform(z_djf(i,j,*))
    t_djf_z(i,j,*)=interpol(tprof,zprof,altitude)
    u_djf_z(i,j,*)=interpol(uprof,zprof,altitude)
    v_djf_z(i,j,*)=interpol(vprof,zprof,altitude)
    co_djf_z(i,j,*)=interpol(coprof,zprof,altitude)
endfor
endfor
;
; save
;
save,file='smidemax_300-year_TUVZ_djf_jja_sosst.sav',nc,nr,nz,alon,alat,altitude,co_djf_z,t_djf_z,u_djf_z,v_djf_z,co_jja_z,t_jja_z,u_jja_z,v_jja_z

end
