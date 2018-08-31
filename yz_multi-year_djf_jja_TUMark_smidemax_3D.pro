;
; multi-year DJF and JJA YZ of T, U, Mark
; SmidEmax 300 years
; monthly mean of daily averages data 
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
xorig=[0.15,0.55,0.15,0.55]
yorig=[0.55,0.55,0.1,0.1]
xlen=0.325
ylen=0.375
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)

goto,quick

;
; loop over years and months
;
icount=0L
for iyear=1,300 do begin
    syear=string(FORMAT='(I3.3)',iyear)
    for imonth=0L,nmonth-1L do begin
        imon=imonth+1
        smon=string(FORMAT='(I2.2)',imon)
        if imon ne 1 and imon ne 2 and imon ne 12 and imon ne 6 and imon ne 7 and imon ne 8 then goto,skipmonth		; DJF and JJA only
;
; restore monthly mean
;
        ofile=dir+syear+smon+'.sav'
        print,'saving '+ofile
        restore,ofile	;,nc,nr,nth,alon,alat,th,ipvavg2,pavg2,uavg2,vavg2,qdfavg2,coavg2,zavg2,sfavg2,mavg2
; 
; declare DJF and JJA arrays
;
        if icount eq 0L then begin
           djf_mark=0.*mavg2
           jja_mark=0.*mavg2
           jja_pressure=0.*pavg2
           djf_pressure=0.*pavg2
           jja_u=0.*pavg2
           djf_u=0.*pavg2
           jja_z=0.*pavg2
           djf_z=0.*pavg2
           ndjf_mark=0.*mavg2
           njja_mark=0.*mavg2
           icount=1L
        endif
;
; DJF
;
    if smon eq '12' or smon eq '01' or smon eq '02' then begin
       djf_mark=djf_mark+mavg2
       djf_pressure=djf_pressure+pavg2
       djf_u=djf_u+uavg2
       djf_z=djf_z+zavg2
       ndjf_mark=ndjf_mark+1.
    endif
;
; JJA
;
    if smon eq '06' or smon eq '07' or smon eq '08' then begin
       jja_mark=jja_mark+mavg2
       jja_pressure=jja_pressure+pavg2
       jja_u=jja_u+uavg2
       jja_z=jja_z+zavg2
       njja_mark=njja_mark+1.
    endif

skipmonth:
endfor  ; loop over months
endfor  ; loop over years

djf_mark=djf_mark/ndjf_mark
djf_pressure=djf_pressure/ndjf_mark
djf_u=djf_u/ndjf_mark
djf_z=djf_z/ndjf_mark
jja_mark=jja_mark/njja_mark
jja_pressure=jja_pressure/njja_mark
jja_u=jja_u/njja_mark
jja_z=jja_z/njja_mark
;
; calculate temperature
;
djf_temp=0.*djf_mark
jja_temp=0.*jja_mark
for k=0,nth-1 do djf_temp(*,*,k) = th(k)*( (djf_pressure(*,*,k)/1000.)^(.286) )
for k=0,nth-1 do jja_temp(*,*,k) = th(k)*( (jja_pressure(*,*,k)/1000.)^(.286) )

djf_press=fltarr(nth)
jja_press=fltarr(nth)
for kk=0,nth-1 do djf_press(kk)=mean(djf_pressure(nr/2:nr-1,*,kk))  ; NH
for kk=0,nth-1 do jja_press(kk)=mean(jja_pressure(0:nr/2-1,*,kk))  ; SH
;
; save 3d DJF and JJA T, U, Z, Mark
;
save,file='smidemax_300-year_TUmark_djf_jja.sav',nc,nr,nth,alon,alat,th,djf_mark,jja_mark,djf_press,jja_press,djf_u,jja_u,djf_temp,jja_temp,djf_z,jja_z
quick:
restore,'smidemax_300-year_TUmark_djf_jja.sav'
;
; calculate zonal means
;
djf_tyz=mean(djf_temp,dim=2)
djf_uyz=mean(djf_u,dim=2)
djf_markyz=mean(djf_mark,dim=2)
djf_zyz=mean(djf_z,dim=2)

jja_tyz=mean(jja_temp,dim=2)
jja_uyz=mean(jja_u,dim=2)
jja_markyz=mean(jja_mark,dim=2)
jja_zyz=mean(jja_z,dim=2)

;
; plot
;
if setplot eq 'ps' then begin
   lc=0
   !p.font=0
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
           /bold,/color,bits_per_pixel=8,/helvetica,filename='yz_multi-year_djf_jja_TUMark_smidemax.ps'
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !y.thick=2
   !x.thick=2
endif
;
; DJF
;
x2d=0.*djf_tyz
for k=0L,nth-1L do x2d(*,k)=alat
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
zyz=jja_zyz
contour,jja_tyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[-90,90],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,title='JJA'
contour,jja_tyz,alat,zyz,/noera,/foll,color=0,levels=tlevel,/overplot
myz=jja_markyz
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=8,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=8,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5

!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
zyz=djf_zyz
contour,djf_tyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=tlevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,title='DJF'
contour,djf_tyz,alat,zyz,/noera,/foll,color=0,levels=tlevel,/overplot
myz=djf_markyz
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=8,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=8,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(1)+0.01,yorig(1)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(tlevel)
imax=max(tlevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Temperature (K)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor

nlvls=21
ulevel=-100+10.*findgen(nlvls)
col1=(findgen(nlvls)/float(nlvls))*mcolor
!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
zyz=jja_zyz
contour,jja_uyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=ulevel,xrange=[-90,90],yrange=[30,125],ytitle='Altitude (km)',charsize=1.5,charthick=2,xtitle='Latitude'
index=where(ulevel gt 0.)
contour,jja_uyz,alat,zyz,/noera,/foll,color=0,levels=ulevel(index),/overplot
index=where(ulevel lt 0.)
contour,jja_uyz,alat,zyz,/noera,/foll,color=mcolor,levels=ulevel(index),/overplot,c_linestyle=5
myz=jja_markyz
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=8,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=8,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5

!type=2^2+2^3
xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
zyz=djf_zyz
contour,djf_uyz,alat,zyz,/noera,/fill,color=0,c_color=col1,levels=ulevel,xrange=[-90,90],yrange=[30,125],charsize=1.5,charthick=2,xtitle='Latitude'
index=where(ulevel gt 0.)
contour,djf_uyz,alat,zyz,/noera,/foll,color=0,levels=ulevel(index),/overplot
index=where(ulevel lt 0.)
contour,djf_uyz,alat,zyz,/noera,/foll,color=mcolor,levels=ulevel(index),/overplot,c_linestyle=5
myz=djf_markyz
contour,myz,alat,zyz,/noeras,/foll,color=0,thick=8,levels=[0.1],/overplot
index=where(myz ge 0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=0,symsize=0.5
contour,myz,alat,zyz,/noeras,/foll,color=mcolor,thick=8,levels=[-0.1],/overplot
index=where(myz le -0.1)
if index(0) ne -1L then oplot,x2d(index),zyz(index),psym=8,color=mcolor,symsize=0.5
xmnb=xmx +cbaryoff
xmxb=xmnb+cbarydel
set_viewport,xmnb,xmxb,yorig(3)+0.01,yorig(3)+ylen-0.01
!type=2^2+2^3+2^5
imin=min(ulevel)
imax=max(ulevel)
plot,[0,0],[imin,imax],xrange=[0,10],yrange=[imin,imax],/noeras,color=0,charsize=1,ytitle='Ubar (m/s)'
xbox=[0,10,10,0,0]
y2=imin
dy=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    ybox=[y2,y2,y2+dy,y2+dy,y2]
    polyfill,xbox,ybox,color=col1(j)
    y2=y2+dy
endfor
;
; Close PostScript file and return control to X-windows
;
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim yz_multi-year_djf_jja_TUMark_smidemax.ps -rotate -90 yz_multi-year_djf_jja_TUMark_smidemax.jpg'
;  spawn,'rm -f yz_multi-year_djf_jja_TUMark_smidemax.ps'
endif

end
