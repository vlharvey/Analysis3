;
; calculate mean and standard dev of the perturbation albedo percent per SZA bin
;
loadct,39
mcolor=!p.color
icolmax=byte(!p.color)
mcolor=icolmax
icmm1=icolmax-1B
icmm2=icolmax-2B
device,decompose=0
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=700
nydim=700
xorig=[0.25]
yorig=[0.2]
xlen=0.7
ylen=0.7
cbaryoff=0.04
cbarydel=0.03
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.2*cos(a),.2*sin(a),/fill

;goto,quick

ALBLIM=1.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ALBLIM=0.       ;LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.^M
ERRLIM=1.0      ;MAXIMUM ALLOWED RATIO OF ALBEDO_ERR/ALBEDO^M
SZALIM_HI=88.   ;DATA WITH SZA > SZALIM_HI ARE SUSPECT^M
SZALIM_LO=50.   ;DATA WITH SZA < SZALIM_LO ARE SUSPECT^M
;
; level 3 variance grid
;
;nlat=171L
;nlon=360L
;latgrid=-85+findgen(nlat)
;longrid=findgen(nlon)
nlat=86L
;nlon=180L
latgrid=-85+2.*findgen(nlat)
;longrid=2.*findgen(nlon)
nsza=55L
szabin=50+findgen(55)

nlon=90L
longrid=4.*findgen(nlon)		; AIRS level3 is 4x2 degree

pth='/atmos/harvey/CIPS_data/Datfiles/GW_Carstens/ray_gw_dat/cips_ray_gw_north_'
orbitlist=['49914','49873','49873','49861','49899','49884','49884','49883','49873','49903','49904','49908','49909',$
           '49914','49923','49923','49924','49928','49940','49955','49958','49914','49873','49858','49885']
iscene = [14,12,13,14,13,13,14,14,13,13,13,13,12,14,12,13,13,14,13,13,14,14,12,13,7]

;
; loop over cases
;
syear='2016'
;spawn,'ls '+pth+syear+'_?????_*.sav',fnames
norbit=n_elements(orbitlist)

nsza=55L
szabin=50+findgen(55)
cips_mean=fltarr(nsza,norbit)
cips_sigma=fltarr(nsza,norbit)
cips_counts=fltarr(nsza,norbit)
sorbit_all=strarr(norbit)
;
; loop over orbits
;
for iorbit=0L,norbit-1L do begin

    spawn,'ls '+pth+syear+'_'+orbitlist(iorbit)+'_*.sav',fnames
    restore,fnames(0)
    print,fnames(0)
    dum=strsplit(fnames(0),'_',/extract)
    sorbit_all(iorbit)=dum(-3)
;
; extract scenes for this orbit

    latitude_scene=scene.lat
    longitude_scene=scene.lon
    alb_scene=scene.alb
    sza_scene=scene.sza*180./!pi		; convert to degrees

    alb_gw=reform(alb_scene(*,*,iscene(iorbit)))	; scene with GWs
    sza_gw=reform(sza_scene(*,*,iscene(iorbit)))
;
; remove Nans
;
    good=where(finite(ALB_SCENE) and finite(SZA_SCENE))                
    alb_scene=alb_scene(good)
    sza_scene=sza_scene(good)

    good=where(finite(ALB_GW) and finite(SZA_GW))
    alb_gw=alb_gw(good)
    sza_gw=sza_gw(good)


    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       !p.font=0
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='mean_albpert_vs_sza_gws_'+sorbit_all(iorbit)+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=5
       !y.thick=2
       !x.thick=2
    endif

ylim=2.e2
erase
plot,sza_scene,alb_scene,xrange=[50,105],yrange=[-1*ylim,ylim],xtitle='SZA',ytitle='Albedo perturbation (%)',color=0,psym=1,charsize=2,charthick=2,title=sorbit_all(iorbit)
oplot,sza_gw,alb_gw,psym=1,color=250

     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim mean_albpert_vs_sza_gws_'+sorbit_all(iorbit)+'.ps -rotate -90 mean_albpert_vs_sza_gws_'+sorbit_all(iorbit)+'.jpg'
        spawn,'rm -f mean_albpert_vs_sza_gws_'+sorbit_all(iorbit)+'.ps
     endif

;
; bin albedo in SZA to explore proper cutoff
;
     dsza=(szabin(1)-szabin(0))/2.
     FOR J=0l,NSZA-1l DO BEGIN                                                                        ; loop over latitudes
         index=where(sza_scene ge szabin(j)-dsza and sza_scene lt szabin(j)+dsza,npoints)
         if n_elements(index) lt 3 then goto,jumpsza
         result=moment(alb_scene(index))
         cips_mean(j,iorbit)=result(0)
         cips_sigma(j,iorbit)=sqrt(result(1))
         cips_counts(j,iorbit)=npoints
         jumpsza:
     endfor
ENDFOR	; loop over orbits
;
; save
;
save,file='mean_albedo_vs_sza_gws.sav',nsza,szabin,norbit,sorbit_all,cips_mean,cips_sigma,cips_counts
quick:
restore,'mean_albedo_vs_sza_gws.sav'	;,nsza,szabin,norbit,sorbit_all,cips_mean,cips_sigma,cips_counts
;
; postscript file
;
       if setplot eq 'ps' then begin
          lc=0
          xsize=nxdim/100.
          ysize=nydim/100.
          set_plot,'ps'
          !p.font=0
          device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                 /bold,/color,bits_per_pixel=8,/helvetica,filename='mean_albpert_vs_sza_gws.ps'
          !p.charsize=1.25
          !p.thick=2
          !p.charthick=5
          !y.thick=2
          !x.thick=2
       endif

erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
erase
!type=2^2+2^3
ylim=5.e0
plot,szabin,reform(cips_mean(*,0)),yrange=[-1*ylim,ylim],xtitle='SZA',ytitle='Albedo perturbation (%)',color=0,thick=5,/nodata,charsize=2,charthick=2
oplot,szabin,mean(cips_mean,dim=2),color=0,thick=15
oplot,szabin,mean(cips_mean,dim=2)+mean(cips_sigma,dim=2),color=250,thick=10
oplot,szabin,mean(cips_mean,dim=2)-mean(cips_sigma,dim=2),color=250,thick=10

plots,85,-1*ylim
plots,85,ylim,/continue,color=0,thick=3
plots,86,-1*ylim
plots,86,ylim,/continue,color=0,thick=3
plots,87,-1*ylim
plots,87,ylim,/continue,color=0,thick=3
plots,88,-1*ylim
plots,88,ylim,/continue,color=0,thick=3
plots,89,-1*ylim
plots,89,ylim,/continue,color=0,thick=3
plots,90,-1*ylim
plots,90,ylim,/continue,color=0,thick=3


;for iorbit=0L,norbit-1L do begin
;    oplot,szabin,reform(cips_mean(*,iorbit)),color=0,thick=2
;    oplot,szabin,reform(cips_mean(*,iorbit))+reform(cips_sigma(*,iorbit)),color=250
;    oplot,szabin,reform(cips_mean(*,iorbit))-reform(cips_sigma(*,iorbit)),color=250
;endfor

; Close PostScript file and return control to X-windows
     if setplot ne 'ps' then stop
     if setplot eq 'ps' then begin
        device, /close
        spawn,'convert -trim mean_albpert_vs_sza_gws.ps -rotate -90 mean_albpert_vs_sza_gws.jpg'
;       spawn,'rm -f mean_albpert_vs_sza_gws.ps
     endif

;goto,jump
end
