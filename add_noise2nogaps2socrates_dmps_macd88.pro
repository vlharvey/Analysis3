;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; add noise to NOGAPS ozone dmp file
;
; Profile OUTPUT:
;       => number of occultations
;       => time, latitude, longitude, tropopause diagnostics
;	=> number of theta levels
;       => vertical profiles of pressure, temperature, z, PV, O3, H2O on theta
;
; VLH 8/15/2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(!pi/8.)
usersym,cos(a),sin(a),/fill

ifile='/Volumes/earth/aura6/data/NOGAPS_Alpha/Datfiles_SOSST/dmps_socrates.nogaps.2008'
ofile='/Volumes/earth/aura6/data/NOGAPS_Alpha/Datfiles_SOSST/dmps_socrates.nogaps+noise.2008'
; id,date,time,longitude,latitude,th,p_prof,tp_prof,z_prof,pv_prof,o3_prof,h2o_prof,dyntrop,zthermtrop,pthermtrop,ththermtrop
restore,ifile
nth=n_elements(th)
nprof=n_elements(latitude)
o3_prof_noise=0.*o3_prof
h2o_prof_noise=0.*h2o_prof
;
; initialize
;
seed=5
q=randomn(seed)
for i=0L,nprof-1L do begin
for k=0L,nth-1L do begin
    if h2o_prof(i,k) ne -99. and o3_prof(i,k) ne -99. then begin
       if h2o_prof(i,k) lt 10. then o3_prof_noise(i,k)=o3_prof(i,k)+0.01*randomn(seed)		; 1%
       if h2o_prof(i,k) ge 10. then o3_prof_noise(i,k)=o3_prof(i,k)+0.1*randomn(seed)		; 10%
       h2o_prof_noise(i,k)=h2o_prof(i,k)+0.01*randomn(seed)					; 1%
    endif
endfor
endfor
;
erase
!type=2^2+2^3
plot,h2o_prof(*,33),o3_prof(*,33),psym=8,title='350 K',ytitle='Ozone',xtitle='Water Vapor',/noeras,yrange=[0.,2.],xrange=[0.1,1000.],/xlog
oplot,h2o_prof_noise(*,33),o3_prof_noise(*,33),psym=8,symsize=0.5,color=mcolor*.9

o3_prof=o3_prof_noise
h2o_prof=h2o_prof_noise
save,file=ofile,id,date,time,longitude,latitude,th,p_prof,tp_prof,z_prof,pv_prof,o3_prof,h2o_prof,dyntrop,zthermtrop,pthermtrop,ththermtrop

end
