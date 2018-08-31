;
; View longitude-altitude cross sections of latitude of origin
; after running parcel back trajectories sampled on pday at y0
; by the SAM II satellite.  Programmed by RBP & VLH 12/18/97.

; set color table
loadct,38
mcolor=!p.color
icmm1=fix(byte(mcolor))-1
icmm2=fix(byte(mcolor))-2
colbw='col'

SETPLOT='x'
read,'setplot?',setplot

; plotting windows
nxdim=750
nydim=750
xorig=[0.1]
yorig=[0.15]
xlen=0.8
ylen=0.8
cbaryoff=0.03
cbarydel=0.02

; plotting crap
if setplot ne 'ps' then $
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=mcolor

; more plotting crap
if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='xz_lat0.ps'
   if colbw ne 'bw' and colbw ne 'gs' then device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
   !p.thick=2.0                   ;Plotted lines twice as thick
   !p.charsize=1.0
endif

dirt='/usr34/users/harvey/solve/airmass/simulations/'
thlab=['450k','475k','500k','525k','550k','600k','650k','700k','750k']
thetalev=[450,475,500,525,550,600,650,700,750]

nday=23l
nout=4l
nwrite=nday*nout
norbit=30l
noccul=nday*norbit
ntheta=9
nfile=nday*nout
for nplot=0,nday-1 do begin
; 
; plot day of back trajectory
pday=nfile/nout-nplot

; location of all particles by day
msave=fltarr(ntheta,nwrite,noccul)
xsave=fltarr(ntheta,nwrite,noccul)
ysave=fltarr(ntheta,nwrite,noccul)
zsave=fltarr(ntheta,nwrite,noccul)

; original locations of all occultations
x0save=fltarr(ntheta,noccul)
y0save=fltarr(ntheta,noccul)
z0save=fltarr(ntheta,noccul)
age=9999+fltarr(ntheta,noccul)
starttime=9999+fltarr(ntheta,noccul)

; original location of one occultation
x0=9999+fltarr(ntheta,15)
y0=9999+fltarr(ntheta,15)
z0=9999+fltarr(ntheta,15)

; final location of x0, y0, z0 (use zfinal later when diabatic)
xfinal=9999+fltarr(ntheta,15)
yfinal=9999+fltarr(ntheta,15)
zfinal=9999+fltarr(ntheta,15)

for k=0,ntheta-1 do begin
      ifile='ukmo_sage2_96010112-96033100_3d_'+thlab(k)+'.airm'
      dir=dirt
      close,12
      openr,12,dir+ifile,/f77
      charexp=' '                         
      theta=0.
      ukmo=' '
      nmc=' '
      ecmwf=' '
      restart=' '
      rfile=' '
      irtime=0L
      istime=0L
      dir=' '                             
      nfiles=0
      wfile=' '
      dtflow=0.                          
      dtout=0.                          
      dt=0.                              
      igw=' '                             
      ofile=' '
      nr=0
      nc=0
      time=0.
      ntraj=0l
      char1=' '
      char2=' '
      char3=' '
      char4=' '
      char5=' '
      char6=' '
      char7=' '
      char8=' '
      char9=' '
      char10=' '
      char11=' '
      char12=' '
      readu,12,charexp                         
      readu,12,theta
      readu,12,ukmo                            
      readu,12,nmc                             
      readu,12,ecmwf                           
      readu,12,restart
      readu,12,rfile
      readu,12,irtime
      readu,12,dir                             
      readu,12,nfiles
      for n=1,nfiles do begin
      readu,12,wfile
      endfor                                    
      readu,12,dir                             
      readu,12,nfiles
      for n=1,nfiles do begin
      readu,12,wfile
      endfor         
      readu,12,istime                           
      readu,12,dtflow                          
      readu,12,dt                              
      readu,12,igw                             
      readu,12,dtout
      readu,12,ofile
      print,ofile
      print,'    Header read ok'

ntrajold=0
for n=0,nfile-1 do begin
      READU,12,irtime,istime,time,ntraj
      print,irtime,istime,time,ntraj
      xn=fltarr(ntraj)
      yn=fltarr(ntraj)
      zn=fltarr(ntraj)
      t0n=fltarr(ntraj)
      x0n=fltarr(ntraj)
      y0n=fltarr(ntraj)
      z0n=fltarr(ntraj)
      aern=fltarr(ntraj)
      pvn=fltarr(ntraj)
      pn=fltarr(ntraj)
      qdfn=fltarr(ntraj)
      rhn=fltarr(ntraj)
;
; --- Read in x,y,z,t0,x0,y0,z0,sad,pv,p,qdf,rh
      readu,12,char1
      READU,12,xn
      readu,12,char2
      READU,12,yn
      readu,12,char3
      READU,12,zn
      readu,12,char4
      READU,12,t0n
      readu,12,char5
      READU,12,x0n
      readu,12,char6
      READU,12,y0n
      readu,12,char7
      READU,12,z0n
      readu,12,char8
      READU,12,aern
      readu,12,char9
      READU,12,pvn
      readu,12,char10
      READU,12,pn
      readu,12,char11
      READU,12,qdfn
      readu,12,char12
      READU,12,rhn
      xsave(k,n,0:ntraj-1)=xn
      ysave(k,n,0:ntraj-1)=yn
      zsave(k,n,0:ntraj-1)=zn
      msave(k,n,0:ntraj-1)=aern

      if ntraj gt ntrajold then begin
         x0save(k,ntrajold:ntraj-1)=x0n(ntrajold:ntraj-1)
         y0save(k,ntrajold:ntraj-1)=y0n(ntrajold:ntraj-1)
         z0save(k,ntrajold:ntraj-1)=zn(ntrajold:ntraj-1)
         starttime(k,ntrajold:ntraj-1)=t0n(ntrajold:ntraj-1)
         ntrajold=ntraj
      endif

; Age is in days.
      age(k,0:ntraj-1)=(time-starttime(k,0:ntraj-1))/24.

endfor  ; end of loop over output times

; plot final latitude of parcels originating at a certain latitude
; for each level...first sort initial longitude for contouring routine

; create array of all original latitudes and times for each level
dummy1=reform(y0save(k,*))
dummy2=reform(age(k,*))

; choose only parcels in the NH from the initial date (age = -pday)
index1=where(dummy1 gt 0. AND dummy2 lt -(pday-1) and dummy2 ge -pday)
np=n_elements(index1)
x0(k,0:np-1)=x0save(k,index1)
y0(k,0:np-1)=y0save(k,index1)
z0(k,0:np-1)=z0save(k,index1)

; chose same parcels final location
xfinal(k,0:np-1)=xsave(k,nfile-1,index1)
yfinal(k,0:np-1)=ysave(k,nfile-1,index1)
zfinal(k,0:np-1)=zsave(k,nfile-1,index1)

; ascend longitude for plotting xz cross sections
dummy1=reform(x0(k,*))
index2=sort(dummy1)
x0(k,*)=x0(k,index2)
y0(k,*)=y0(k,index2)
z0(k,*)=z0(k,index2)

; ascend longitude same as initial 
xfinal(k,*)=xfinal(k,index2)
yfinal(k,*)=yfinal(k,index2)
zfinal(k,*)=zfinal(k,index2)
endfor  ; end of loop over theta levels
; contour final latitude wrt initial longitude-altitude
x0=transpose(x0)
y0=transpose(y0)
z0=transpose(z0)

xfinal=transpose(xfinal)
yfinal=transpose(yfinal)
zfinal=transpose(zfinal)

; PLOT
nx=intarr(ntheta)
for k=0,ntheta-1 do begin
    nx(k)=n_elements(where(x0(k,*) ne 9999.))
endfor
nxmax=0
nxmax=max(nx)
erase
level=-20+5*findgen(23)
col1=indgen(23)*icmm1/23
col1=reverse(col1)

trajlen=string(FORMAT='(I2)',pday)+' day trajectories'
xyouts,65,15,trajlen,color=mcolor
set_viewport,.1,.9,.15,.95
!p.charsize=7./5.
;
; Find mean latitude of origin (not including 9999. values)
result=moment( y0(where(y0(*,4) ne 9999.),4) )
if result(0) lt 0. then $
lat=strcompress(string(FORMAT='(F5.1,A2)',result(0),' S'))
if result(0) gt 0. then $
lat=strcompress(string(FORMAT='(F5.1,A2)',result(0),' N'))
;
; Determine date back trajectory started
stime=''
stime=string(FORMAT='(I8)',assimtime(nplot))
yy=''
mm=''
dd=''
yy=strmid(stime,0,2)
mm=strmid(stime,2,2)
dd=strmid(stime,4,2)
date=''
date=mm+'/'+dd+'/'+yy
!p.title='!6Latitude of origin of parcels arriving at '+$
          lat+' on '+date
!type=2^2+2^3
!noeras=1

; Draw frame
contour,yfinal,x0,z0,levels=level,/nodata,/follow,$
;       max_val=9999.,xrange=[x0(0,4),x0(nxmax-1,4)],yrange=[450,750],$
        max_val=9999.,xrange=[0,360],yrange=[450,750],$
;       xticks=(nxmax-1),xtitle='!6Longitude',ytitle='!6Altitude (km)'
        xticks=6,xtitle='!6Longitude',ytitle='!6Altitude (km)'
!type=96
; Color fill
contour,yfinal,x0,z0,levels=level,max_val=9999.,/cell_fill,$
        c_color=col1,xstyle=1,ystyle=1,/overplot,/fill

label=1+0*level
contour,yfinal,x0,z0,levels=level,c_charsize=1.0,$
        c_labels=1+0*level,max_val=9999.,/follow,$
        thick=2.,/overplot,xstyle=1,ystyle=1,$
        c_linestyle= (level lt 0.0),c_color=0
!psym=1
oplot,x0,z0

; Draw color bar
      imin=min(level)
      imax=max(level)
      set_viewport,.1,.9,.04,.07
      !p.title=' '
      !type=2^2+2^3+2^6
      plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax]
      ybox=[0,10,10,0,0]
      x1=imin
      dx=(imax-imin)/float(icmm1)
      for j=icmm1,1,-1 do begin
          xbox=[x1,x1,x1+dx,x1+dx,x1]
          polyfill,xbox,ybox,color=j
          x1=x1+dx
      endfor

; Close PostScript file and return control to X-windows
      if setplot eq 'ps' then begin
         device, /close
         set_plot, 'x'
         !p.font=0
         !p.thick=1.0
      endif

print,pday
;stop
endfor 		; loop over days	
end
