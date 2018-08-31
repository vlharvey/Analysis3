@range_ring
xa=0
xb=90
ya=-180
yb=180
ashoe='           LARC V18 ECMWF-SAM II Airmass '
nh=1
parsym=8
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
ifile='86022300-86021500_700k.airm'
nsam=1500l
nday=9l
nout=4l
nfile=(nday-1)*nout
asave=9999.+fltarr(nfile,nsam)
xsave=9999.+fltarr(nfile,nsam)
ysave=9999.+fltarr(nfile,nsam)
age=9999+fltarr(nsam)
starttime=9999+fltarr(nsam)
SETPLOT='x'
read,'setplot?',setplot
loadct,38
mcolor=!p.color

; define viewport location
nxdim=750
nydim=750
xorig=[0.05]
yorig=[0.15]
xlen=0.8
ylen=0.8
cbaryoff=0.03
cbarydel=0.01

if setplot ne 'ps' then $
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162

if setplot eq 'ps' then begin
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='polar_airm.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
   !p.thick=2.0                   ;Plotted lines twice as thick
   !p.charsize=1.0
endif

!noeras=1
dirt='~harvey/airmass/sam/'
dir=dirt
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
'86053',$
'86052',$
'86051',$
'86050',$
'86049',$
'86048',$
'86047',$
'86046']

ofile=ifile4+'.ps'
iplot=0+0*findgen(nfile)
iplot(nfile-1)=1
ntrajold=0
for n=0,nfile-1 do begin
    READU,12,irtime,istime,time,ntraj
    print,irtime,istime,time,ntraj,ntrajold
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
;   print,'x,y,x0,y0 ',xn(0:ntraj-1),yn(0:ntraj-1),$
;                     x0n(0:ntraj-1),y0n(0:ntraj-1)
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

    if ntraj gt ntrajold then begin
       starttime(ntrajold:ntraj-1)=t0n(ntrajold:ntraj-1)
    endif
    asave(n,0:ntraj-1)=aern(0:ntraj-1)
    xsave(n,0:ntraj-1)=xn(0:ntraj-1)
    ysave(n,0:ntraj-1)=yn(0:ntraj-1)

; Age was in hours- now in days.  Restrict parcels to < 14 days
    age(0:ntraj-1)=(time-starttime(0:ntraj-1))/24.

    if n lt (nfile-1) then goto, jumpfile
;   if n lt 3*nout then goto, jumpfile
;
; aerosol
;
;   val=['0.0','0.2','0.4','0.6','0.8','1.0','1.2','1.4','1.6']
;   index0=where((aern gt 0.0) and (aern le 0.2) and (age le 14.))
;   index1=where((aern gt 0.2) and (aern le 0.4) and (age le 14.))
;   index2=where((aern gt 0.4) and (aern le 0.6) and (age le 14.))
;   index3=where((aern gt 0.6) and (aern le 0.8) and (age le 14.))
;   index4=where((aern gt 0.8) and (aern le 1.0) and (age le 14.))
;   index5=where((aern gt 1.0) and (aern le 1.2) and (age le 14.))
;   index6=where((aern gt 1.2) and (aern le 1.4) and (age le 14.))
;   index7=where((aern gt 1.4) and (aern le 1.6) and (age le 14.))
;   index8=where((aern gt 1.6) and (aern le 1.8) and (age le 14.))
    val=['0.0','0.05','0.1','0.15','0.2','0.25','0.3','0.35','0.4']
    index0=where((aern gt 0.00) and (aern le 0.05) and (age le 14.))
    index1=where((aern gt 0.05) and (aern le 0.10) and (age le 14.))
    index2=where((aern gt 0.10) and (aern le 0.15) and (age le 14.))
    index3=where((aern gt 0.15) and (aern le 0.20) and (age le 14.))
    index4=where((aern gt 0.20) and (aern le 0.25) and (age le 14.))
    index5=where((aern gt 0.25) and (aern le 0.30) and (age le 14.))
    index6=where((aern gt 0.30) and (aern le 0.35) and (age le 14.))
    index7=where((aern gt 0.35) and (aern le 0.40) and (age le 14.))
    index8=where((aern gt 0.40) and (aern le 0.45) and (age le 14.))

; Plot polar projection
    erase
    !psym=0
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    if setplot eq 'ps' then !color=0.
    if setplot eq 'x' then !color=255.
    mtitle=string(theta)+'K '+'SAM-ECMWF Airmass '+ifile4(n/nout)
    if n ne nfile-1 then $
    MAP_SET,NH*90,0,-90*NH,/ORTHO,/GRID,/CONTIN,limit=[xa,ya,xb,yb],$
           /noeras,/noborder,title=mtitle
    if n eq nfile-1 then $
    MAP_SET,NH*90,0,-90*NH,/ORTHO,/GRID,/CONTIN,limit=[xa,ya,xb,yb],$
           /noeras,/noborder,title=mtitle
;   lindex=where(asave(nfile-1,*) lt 0.2)
;   hindex=where(asave(nfile-1,*) ge 0.2)
    lindex=where(asave(n,*) lt 0.2)
    hindex=where(asave(n,*) ge 0.2 and asave(n,*) ne 9999)
    n00=n-7.*nout
    n0=n-1.*nout
    n1=n
    if n0 lt 0 then n0=0
;   for np=0,ntraj-1 do begin
    for np=0,n_elements(lindex)-1 do begin
        !color=.2*mcolor
        oplot,xsave(n0:n1,lindex(np)),ysave(n0:n1,lindex(np))
    endfor
    for np=0,n_elements(hindex)-1 do begin
        !color=.9*mcolor
        oplot,xsave(n00:n1,hindex(np)),ysave(n00:n1,hindex(np))
    endfor

    !psym=8
    !color=.9*mcolor
    if index7(0) ne -1 then oplot,xn(index7),yn(index7)
    !color=.8*mcolor
    if index6(0) ne -1 then oplot,xn(index6),yn(index6)
    !color=.7*mcolor
    if index5(0) ne -1 then oplot,xn(index5),yn(index5)
    !color=.6*mcolor
    if index4(0) ne -1 then oplot,xn(index4),yn(index4)
    !color=.5*mcolor
    if index3(0) ne -1 then oplot,xn(index3),yn(index3)
    !color=.4*mcolor
    if index2(0) ne -1 then oplot,xn(index2),yn(index2)
    !color=.3*mcolor
    if index1(0) ne -1 then oplot,xn(index1),yn(index1)
    !color=.2*mcolor
    if index0(0) ne -1 then oplot,xn(index0),yn(index0)

    if setplot eq 'ps' then !color=0.
    if setplot eq 'x' then !color=255.
    color=mcolor*[.2,.3,.4,.5,.6,.7,.8,.9]
    yval= .35
    xval0= .92
    xval1= 1.0
    dy=.05
    for i=0,7 do begin
       if setplot eq 'ps' then box=0B*bytarr(2,1)
       if setplot eq 'x' then box=0B*bytarr(40,20)
       box=color(i)+box
       if setplot eq 'ps' then tv,box,xval0,yval,xsize=.1,ysize=.05,/normal
       if setplot eq 'x' then tv,box,xval0,yval,/normal
       xyouts,xval0-.05,yval,val(i),/normal
       yval=yval+dy
    endfor
    xyouts,xval1+.025,yval+.05,'aerosol SAD',/normal
    ntrajold=ntraj
stoP
jumpfile: 
endfor
end
