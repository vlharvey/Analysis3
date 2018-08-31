;
; View longitude-altitude cross sections of latitude of origin
; after running parcel back trajectories sampled on pday at y0
; by the SAGE II satellite.  Programmed by VLH 3/3/98.
;
; **Note: Set nday to the number of days of the longest trajectory.

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
   device,/landscape,bits=8,filename='sage_backtrj_lat_origin_xz.ps'
   if colbw ne 'bw' and colbw ne 'gs' then device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
   !p.thick=2.0                   ;Plotted lines twice as thick
   !p.charsize=1.0
endif

thlab=[$
'450k',$
'475k',$
'500k',$
'525k',$
'550k',$
'600k',$
'650k',$
'700k',$
'750k',$
'800k',$
'850k',$
'900k',$
'950k',$
'1000k']
thetalev=[$
450,$
475,$
500,$
525,$
550,$
600,$
650,$
700,$
750,$
800,$
850,$
900,$
1000]

pecmwf_asm='n'
pukmo_ops='n'
pgsfc_asm='n'

nday=9l
nout=4l
nwrite=nday*nout
norbit=30l
nsam=nday*norbit
ntheta=13
nfile=nday*nout
for nplot=0,nday-1 do begin
; 
; plot day of back trajectory
pday=nfile/nout-nplot

; location of all particles by day
msave=fltarr(ntheta,nwrite,nsam)
xsave=fltarr(ntheta,nwrite,nsam)
ysave=fltarr(ntheta,nwrite,nsam)
zsave=fltarr(ntheta,nwrite,nsam)

; original locations of all occultations
x0save=fltarr(ntheta,nsam)
y0save=fltarr(ntheta,nsam)
z0save=fltarr(ntheta,nsam)
age=9999+fltarr(ntheta,nsam)
starttime=9999+fltarr(ntheta,nsam)

; original location of one occultation
x0=9999+fltarr(ntheta,15)
y0=9999+fltarr(ntheta,15)
z0=9999+fltarr(ntheta,15)

; final location of x0, y0, z0 (use zfinal later when diabatic)
xfinal=9999+fltarr(ntheta,15)
yfinal=9999+fltarr(ntheta,15)
zfinal=9999+fltarr(ntheta,15)

for k=0,ntheta-1 do begin
ifile='ecmwf_sage2_96043000-96040100_v18_3d_'+thlab(k)+'_back.airm'
dirt='~harvey/airmass/sage/'
dirw='/usr20/ecmdb/ecm96/'
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
      char13=' '
      char14=' '
;
;  read input parameters for this experiment to output file
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
ifile4=[$
'EC86047.0.th',$  ; Feb.16th
'EC86046.0.th',$  ; Feb.15th
'EC86045.0.th',$
'EC86044.0.th',$
'EC86043.0.th',$
'EC86042.0.th',$
'EC86041.0.th',$
'EC86040.0.th',$
'EC86039.0.th',$
'EC86038.0.th']

ofile=ifile4+'.ps'
iplot=1+0*findgen(nfile)
assimtime=[$
86021500l,$  ; Feb.15th (day of interest)
86021400l,$
86021300l,$
86021200l,$
86021100l,$
86021000l,$
86020900l,$
86020800l,$
86020700l,$
86020600l]  ; not blown

if pecmwf_asm eq 'y' then begin
x4=-180.+360.*findgen(145)/144.
xm4=x4
y4=-90+180.*findgen(73)/73.
endif

if pgsfc_asm eq 'y' then begin
x4=-180.+360.*findgen(145)/144.
xm4=x4
y4=-90+180.*findgen(91)/91.
endif
;
if pukmo_ops eq 'y' then begin
x4=-180.+360.*findgen(289)/288.
xm4=x4
y4=-90+180.*findgen(108)/108.
endif 

m=0
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
      o3n=fltarr(ntraj)
      no2n=fltarr(ntraj)
      aern=fltarr(ntraj)
      pvn=fltarr(ntraj)
      pn=fltarr(ntraj)
      qdfn=fltarr(ntraj)
      rhn=fltarr(ntraj)
;
; --- Read in x,y,z,t,o3,no2,sad,pv,p,qdf,rh
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
      READU,12,o3n
      readu,12,char9
      READU,12,no2n
      readu,12,char10
      READU,12,aern
      readu,12,char11
      READU,12,pvn
      readu,12,char12
      READU,12,pn
      readu,12,char13
      READU,12,qdfn
      readu,12,char14
      READU,12,rhn

      xsave(k,n,0:ntraj-1)=xn
      ysave(k,n,0:ntraj-1)=yn
      zsave(k,n,0:ntraj-1)=zn
      msave(k,n,0:ntraj-1)=aern

      if ntraj gt ntrajold then begin
;        x0save(k,ntrajold:ntraj-1)=xn(ntrajold:ntraj-1)
         x0save(k,ntrajold:ntraj-1)=x0n(ntrajold:ntraj-1)
;        y0save(k,ntrajold:ntraj-1)=yn(ntrajold:ntraj-1)
         y0save(k,ntrajold:ntraj-1)=y0n(ntrajold:ntraj-1)
         z0save(k,ntrajold:ntraj-1)=zn(ntrajold:ntraj-1)
         starttime(k,ntrajold:ntraj-1)=t0n(ntrajold:ntraj-1)
         ntrajold=ntraj
      endif

; Age is in days.
      age(k,0:ntraj-1)=(time-starttime(k,0:ntraj-1))/24.

close,6
;print,'Times ',istime,assimtime(m)
if istime eq assimtime(m) then begin

if pecmwf_asm eq 'y' then begin
nc=144
nr=73
nth=20
x1=fltarr(144)
dummy=-180.+180.*findgen(72)/72.
x1(0:71)=dummy
x1(72:143)=-reverse(dummy+2.5)
y1=-90.+2.5*findgen(73)
xlon=fltarr(nc)
xlat=fltarr(nr)
th=fltarr(nth)
close,1
      openr,1,dirw+ifile4(m),/f77
      print,'open ',ifile4(m)
 
      pv4=fltarr(73,144,20)
      q4=fltarr(73,144,20)
      qdf4=fltarr(73,144,20)
      rh4=fltarr(73,144,20)
      p4=fltarr(73,144,20)
      u4=fltarr(73,144,20)
      v4=fltarr(73,144,20)
 
      inr=nr
      inc=nc
      inth=nth
      readu,1,inr,inc,inth
      readu,1,xlon,xlat,th
      readu,1,pv4,p4,u4,v4,q4,qdf4,rh4
      thlev=1
      index=where(th eq thetalev(k))
      if index(0) ne -1 then thlev=index(0)+1
      theta=thetalev(k)
      if theta ge 700. then pv4=transpose(pv4(*,*,thlev-1))*1000.
      if theta lt 700. then pv4=transpose(pv4(*,*,thlev-1))*100.
      p4=transpose(p4(*,*,thlev-1))
      q4=transpose(q4(*,*,thlev-1))
      qdf4=transpose(qdf4(*,*,thlev-1))
      rh4=transpose(rh4(*,*,thlev-1))
      u4=transpose(u4(*,*,thlev-1))
      v4=transpose(v4(*,*,thlev-1))
      s4=sqrt(u4^2+v4^2)
      t4=theta*(p4/1000.)^.286
  ;
  ;  Create data array with 1st dimension being longitude varying from
  ;  -180 to 180 degrees dimension being latitude
  ;
dummy1=pv4(0:71,*)
dummy2=pv4(72:143,*)
pv4(0:71,*)=dummy2
pv4(72:143,*)=dummy1
;
dummy1=u4(0:71,*)
dummy2=u4(72:143,*)
u4(0:71,*)=dummy2
u4(72:143,*)=dummy1
;
dummy1=v4(0:71,*)
dummy2=v4(72:143,*)
v4(0:71,*)=dummy2
v4(72:143,*)=dummy1
;
dummy1=s4(0:71,*)
dummy2=s4(72:143,*)
s4(0:71,*)=dummy2
s4(72:143,*)=dummy1
;
dummy1=t4(0:71,*)
dummy2=t4(72:143,*)
t4(0:71,*)=dummy2
t4(72:143,*)=dummy1
;
dummy1=p4(0:71,*)
dummy2=p4(72:143,*)
p4(0:71,*)=dummy2
p4(72:143,*)=dummy1
;
dummy1=q4(0:71,*)
dummy2=q4(72:143,*)
q4(0:71,*)=dummy2
q4(72:143,*)=dummy1
;
;
dummy1=qdf4(0:71,*)
dummy2=qdf4(72:143,*)
qdf4(0:71,*)=dummy2
qdf4(72:143,*)=dummy1

dummy1=rh4(0:71,*)
dummy2=rh4(72:143,*)
rh4(0:71,*)=dummy2
rh4(72:143,*)=dummy1

        pvm=fltarr(145,73)
        pvm(0:143,*)=pv4(0:143,*)
        pvm(144,*)=pvm(0,*)

        pm=fltarr(145,73)
        pm(0:143,*)=p4(0:143,*)
        pm(144,*)=pm(0,*)

        um=fltarr(145,73)
        um(0:143,*)=u4(0:143,*)
        um(144,*)=um(0,*)

        vm=fltarr(145,73)
        vm(0:143,*)=v4(0:143,*)
        vm(144,*)=vm(0,*)

        qm=fltarr(145,73)
        qm(0:143,*)=q4(0:143,*)
        qm(144,*)=qm(0,*)

        qdfm=fltarr(145,73)
        qdfm(0:143,*)=qdf4(0:143,*)
        qdfm(144,*)=qdfm(0,*)
 
        rhm=fltarr(145,73)
        rhm(0:143,*)=rh4(0:143,*)
        rhm(144,*)=rhm(0,*)
 
        tm=fltarr(145,73)
        tm(0:143,*)=t4(0:143,*)
        tm(144,*)=tm(0,*)
 
        sm=fltarr(145,73)
        sm(0:143,*)=s4(0:143,*)
        sm(144,*)=sm(0,*)
        print,max(sm),min(sm)
endif

if pgsfc_asm eq 'y' then begin

nc=144
nr=91
nth=23
x1=fltarr(144)
dummy=-180.+180.*findgen(72)/72.
x1(0:71)=dummy
x1(72:143)=-reverse(dummy+2.5)
y1=-90.+2.*findgen(91)
xlon=fltarr(nc)
xlat=fltarr(nr)
th=fltarr(nth)
close,1
      openr,1,dirw+ifile4(m),/f77

      pv4=fltarr(91,144,23)
      q4=fltarr(91,144,23)
      qdf4=fltarr(91,144,23)
      p4=fltarr(91,144,23)
      u4=fltarr(91,144,23)
      v4=fltarr(91,144,23)
;
      inr=nr
      inc=nc
      inth=nth
      readu,1,inr,inc,inth
      readu,1,xlon,xlat,th
      readu,1,pv4,p4,u4,v4,q4,qdf4 
      thlev=1
      index=where(th eq thetalev(k))
      if index(0) ne -1 then thlev=index(0)+1
      theta=thetalev(k)
      pv4=transpose(pv4(*,*,thlev-1))*100. 
      p4=transpose(p4(*,*,thlev-1))
      q4=transpose(q4(*,*,thlev-1))
      qdf4=transpose(qdf4(*,*,thlev-1))
      u4=transpose(u4(*,*,thlev-1))
      v4=transpose(v4(*,*,thlev-1))
      s4=sqrt(u4^2+v4^2)
      t4=theta*(p4/1000.)^.286
  ;
  ;  Create data array with 1st dimension being longitude varying from
  ;  -180 to 180 degrees dimension being latitude 
  ;
dummy1=pv4(0:71,*)
dummy2=pv4(72:143,*)
pv4(0:71,*)=dummy2
pv4(72:143,*)=dummy1
;
dummy1=u4(0:71,*)
dummy2=u4(72:143,*)
u4(0:71,*)=dummy2
u4(72:143,*)=dummy1
;
dummy1=v4(0:71,*)
dummy2=v4(72:143,*)
v4(0:71,*)=dummy2
v4(72:143,*)=dummy1
;
dummy1=s4(0:71,*)
dummy2=s4(72:143,*)
s4(0:71,*)=dummy2
s4(72:143,*)=dummy1
;
dummy1=t4(0:71,*)
dummy2=t4(72:143,*)
t4(0:71,*)=dummy2
t4(72:143,*)=dummy1
;
dummy1=p4(0:71,*)
dummy2=p4(72:143,*)
p4(0:71,*)=dummy2
p4(72:143,*)=dummy1
;
dummy1=q4(0:71,*)
dummy2=q4(72:143,*)
q4(0:71,*)=dummy2
q4(72:143,*)=dummy1
;
;
dummy1=qdf4(0:71,*)
dummy2=qdf4(72:143,*)
qdf4(0:71,*)=dummy2
qdf4(72:143,*)=dummy1
;

        pvm=fltarr(145,91)
        pvm(0:143,*)=pv4(0:143,*)
        pvm(144,*)=pvm(0,*)
;
        qm=fltarr(145,91)
        qm(0:143,*)=q4(0:143,*)
        qm(144,*)=qm(0,*)
;
        qdfm=fltarr(145,91)
        qdfm(0:143,*)=qdf4(0:143,*)
        qdfm(144,*)=qdfm(0,*)
;
        tm=fltarr(145,91)
        tm(0:143,*)=t4(0:143,*)
        tm(144,*)=tm(0,*)
;
        sm=fltarr(145,91)
        sm(0:143,*)=s4(0:143,*)
        sm(144,*)=sm(0,*)
endif
;   

if pukmo_ops eq 'y' then begin
         openr,6,dirw+ifile4(m),/f77
         print,'open ',ifile4(m)

      pv4=fltarr(108,288,10)
      p4=fltarr(108,288,10)
      u4=fltarr(108,288,10)
      v4=fltarr(108,288,10)
      q4=fltarr(108,288,10)
;
nr=108
nc=288
nl=10
th=fltarr(nl)
        readu,6,th
        thlev=5
        readu,6,nr,nc
;        print,' reading pv file',nr,nc
        xlon=fltarr(nc)
        ylat=fltarr(nr)
        readu,6,xlon,ylat

      readu,6,pv4,p4,u4,v4,q4
      thlev=1
      index=where(th eq thetalev(k))
      if index(0) ne -1 then thlev=index(0)+1
      theta=thetalev(k)
      pv4=-transpose(reform(pv4(*,*,thlev-1)))*1.e3
      p4=transpose(reform(p4(*,*,thlev-1)))
      u4=transpose(reform(u4(*,*,thlev-1)))
      v4=transpose(reform(v4(*,*,thlev-1)))
      q4=transpose(reform(q4(*,*,thlev-1)))
      s4=sqrt(u4^2+v4^2)
      t4=theta*(p4/1000.)^.286
;
        pvm=fltarr(289,108)
        pvm(0:143,*)=pv4(144:287,*)
        pvm(144:287,*)=pv4(0:143,*)
        pvm(288,*)=pvm(0,*)
;
        sm=fltarr(289,108)
        sm(0:143,*)=s4(144:287,*)
        sm(144:287,*)=s4(0:143,*)
        sm(288,*)=sm(0,*)
;
        tm=fltarr(289,108)
        tm(0:143,*)=t4(144:287,*)
        tm(144:287,*)=t4(0:143,*)
        tm(288,*)=tm(0,*)
;
endif   
m=m+1
endif   ; end of if than for assimtime
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
trajlen=''
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
contour,yfinal,x0,z0,levels=level,/nodata,/follow,max_val=9999.,$
        xrange=[0,360],yrange=[thetalev(0),thetalev(ntheta-1)],$
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

; Try to animate
      save=assoc(3,bytarr(750,750))
      img=bytarr(750,750)
      img(0,0)=TVRD(0,0,750,750)
      openr,3,'sage_backtrj_lat_origin_xz.dat',error=err
      close,3
      if (err ne 0) then begin
          openw,3,'sage_backtrj_lat_origin_xz.dat',/fixed,4096L
          save(0)=img
          close,3
      endif else begin
          openu,3,'sage_backtrj_lat_origin_xz.dat',/fixed,4096L
          i=0
          while not eof(3) do begin
                xx=save(i)
                i=i+1
          endwhile
          save(i)=img
          close,3
      endelse

print,pday
stop
endfor 		; loop over days	
end
