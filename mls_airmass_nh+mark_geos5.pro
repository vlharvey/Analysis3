;
; merged files have only CO in them
; NH polar projection with ozone and vortices superimposed
;
@read_mls_header
@read_mls_airmass
@rd_ukmo_nc3

SETPLOT='ps'
read,'setplot=',setplot
print,' '
ihem=1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
loadct,39
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.03
cbarydel=0.02
!NOERAS=-1
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mday =[31,28,31,30,31,30,31,31,30,31,30,31]
if setplot ne 'ps' then begin
   lc=0
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
nlvls=20
col1=5+indgen(nlvls)*mcolor/nlvls
dir='/aura7/harvey/GEOS5_data/Datfiles/'
dirw='/aura7/harvey/GEOS5_data/Datfiles/'
dirh='/aura2/harvey/Airmass/MLS/'
hfile=[$
'geos5_mls_200502_3d.airm']
kfile=n_elements(hfile)
for kk=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kk),/f77
read_mls_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
ntraj=0L
for n=0,nfiles-1 do begin
    if n eq nfiles-1 then goto,jumprun
;
; MLS airmass
;
    READU,11,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
    if ntraj eq 0L then goto,jumpday
    read_mls_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,$
         hno3n,h2on,xno2n,o3n,hno3n_err,h2on_err,xno2n_err,o3n_err,pvn,$
         pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn
;
; extract day, month, year information from trajectory file
    sictime=strcompress(ictime,/remove_all)
    idy=strmid(sictime,6,2)
    imn=strmid(sictime,4,2)
    iyr=strmid(sictime,0,4)
    ihr=strmid(sictime,8,2)
    date=sictime
;
;  read same day of UKMO isentropic data
;
    iflag=0
    stime0=string(FORMAT='(I4,I2.2,I2.2)',iyr,imn,idy)
    ufile=dir+'DAS.ops.asm.tavg3d_dyn_v.GEOS510.MetO.'+stime0+'_AVG.V01.nc3'
    rd_ukmo_nc3,ufile,nc,nr,nl,alon,alat,th,pv2,p2,msf2,$
             u2,v2,q2,qdf2,mark2,vp2,sf2,iflag
    if iflag eq 1 then goto,jumpday
    if n eq 0L then begin
       thlev=th
       thlab=strcompress(long(thlev),/remove_all)+'K'
       thval=4000.
;      print,thlev
;      read,'enter theta level ',thval
       k=where(thval eq thlev)
       k=k(0)
       stheta=thlab(k)
       itheta=k
    endif

; isolate UKMO in 2d on proper theta surface (ipv in PVU)
    sf=fltarr(nc+1,nr)
    mark=fltarr(nc+1,nr)
    y2d=fltarr(nc+1,nr)
    x2d=fltarr(nc+1,nr)
    x=fltarr(nc+1)
    x(0:nc-1)=alon
    x(nc)=x(0)+360.
    for i=0,nc-1 do y2d(i,*)=alat
    for j=0,nr-1 do x2d(*,j)=x
    sf1=transpose(sf2(*,*,itheta))
    mark1=transpose(mark2(*,*,itheta))
    sf=fltarr(nc+1,nr)
    sf(0:nc-1,0:nr-1)=sf1
    sf(nc,*)=sf(0,*)
    mark=fltarr(nc+1,nr)
    mark(0:nc-1,0:nr-1)=mark1(0:nc-1,0:nr-1)
    mark(nc,*)=mark(0,*)
    stime=strcompress(string(ictime),/remove_all)
;
; continue only if there are old trajectories
;
    index=where(th0n eq thval and abs(agen) ge 12.0 and yn ge 0. and abs(o3n) lt 1.,ntraj)
    if index(0) ne -1 then begin

; plot UKMO quantity in top panel
    if setplot eq 'ps' then begin
       lc=0
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='mls_o3_'+stheta+'_'+stime+'+mark_nh_geos5.ps'
       device,/color
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
    endif

    !type=2^2+2^3
    erase
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    o3max=9.
    o3min=-1.5
    smin=' ' & smax=' '
    xday=xn(index)
    yday=yn(index)
    x0day=x0n(index)
    y0day=y0n(index)
    ageday=agen(index)
    o3day=o3n(index)*1e6
print,'CO range ',min(o3day),max(o3day)
       smin=string(format='(f5.1)',min(agen)/24.)
       smax=string(format='(f5.1)',max(agen)/24.)
       xyouts,.35,.9,'MLS CO '+stheta,/normal,charsize=2
       MAP_SET,90,0,-90,/ortho,/noeras,charsize=1.5,color=0,$
               title=date+' Trajectories '+smin+' to '+smax+' days old'
       oplot,findgen(361),0.1+0.*findgen(361),psym=0,color=0
       for itraj=0L,ntraj-1L do $
           oplot,[xday(itraj),xday(itraj)],[yday(itraj),yday(itraj)],psym=8,$
                  color=((o3day(itraj)-o3min)/(o3max-o3min))*mcolor
;
; superimpose MetO
;
       index=where(y2d gt -10.)
       sfmin=min(sf(index))*1.e7
       sfmax=max(sf(index))*1.e7
       sfint=(sfmax-sfmin)/20.
       sfbin=sfmin+sfint*findgen(20)
       contour,sf*1.e7,x,alat,/overplot,levels=sfbin,/noeras,$
            c_labels=0*sfbin,max_value=9999.,c_charsize=2,color=0
       loadct,0
       contour,mark,x,alat,/overplot,levels=[-0.1],/noeras,color=0,c_labels=[0],thick=10
       contour,mark,x,alat,/overplot,levels=[0.1],/noeras,color=mcolor*.5,c_labels=[0],thick=10
       loadct,39
       MAP_SET,90,0,-90,/ortho,/noeras,/grid,/contin,color=0

; color bar
       imin=o3min
       imax=o3max
       ymnb=yorig(0) -cbaryoff
       ymxb=ymnb  +cbarydel
       set_viewport,xmn,xmx,ymnb,ymxb
       !type=2^2+2^3+2^6
       plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
             xtitle=stheta+' CO (ppmv)',charsize=2,color=0
       ybox=[0,10,10,0,0]
       x1=imin
       dx=(imax-imin)/float(nlvls)
       for j=0,nlvls-1 do begin
           xbox=[x1,x1,x1+dx,x1+dx,x1]
           polyfill,xbox,ybox,color=col1(j)
           x1=x1+dx
       endfor
       if setplot eq 'ps' then begin
          device, /close
          spawn,'convert -trim mls_o3_'+stheta+'_'+stime+'+mark_nh_geos5.ps -rotate -90 '+$
                ' mls_o3_'+stheta+'_'+stime+'+mark_nh_geos5.jpg'
          spawn,'/usr/bin/rm mls_o3_'+stheta+'_'+stime+'+mark_nh_geos5.ps'
       endif
       if ntraj gt 0 and setplot eq 'x' then stop
    endif	; if there are trajectories today
jumpday:
endfor		; loop over days
jumprun:
endfor		; loop over airmass runs
end
