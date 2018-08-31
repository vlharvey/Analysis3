;
; Plot SAGE II airmasses on an orthographic map colored by age
; contour airmass frequency per lat/lon bin
;
@read_sage2_header
@read_sage2_airmass
@rd_ukmo_nc3
@range_ring

SETPLOT='x'
read,'setplot=',setplot
loadct,38
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
npp=1
delta='n'
gcm_panels,npp,delta,nxdim,nydim,xorig,yorig,xlen,ylen,cbaryoff,cbarydel
!NOERAS=-1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
thlab=['2000k','1800k','1600k','1400k','1200k','1000k','900k',$
       '800k','700k','600k','550k','525k','500k','475k','450k',$
       '425k','400k','390k','380k','370k','360k','350k','340k','330k']
thlev=reverse([330.,340.,350.,360.,370.,380.,390.,400.,425.,450.,475.,$
       500.,525.,550.,600.,700.,800.,900.,1000.,1200.,1400.,$
       1600.,1800.,2000.])
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mday =[31,28,31,30,31,30,31,31,30,31,30,31]
print,thlev
thval=0
read,'enter theta level ',thval
k=where(thval eq thlev)
k=k(0)
stheta=thlab(k)

if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
nlvls=20
col1=5+indgen(nlvls)*mcolor/nlvls
dirw='/aura3/data/UKMO_data/Datfiles/ukmo_'
dirh='/aura2/harvey/Airmass/SAGEII/'
hfile=[$
'ukmo_sage2_oct_2002_forward.airm']
kfile=n_elements(hfile)
for kk=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kk),/f77
read_sage2_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
ntraj=0L
for n=0,nfiles-1 do begin
    if n eq nfiles-1 then goto,jumprun
;
; SAGE II airmass
;
    READU,11,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
    if ntraj gt 0 then $
       read_sage2_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,extn,$
          sadn,h2on,xno2n,o3n,extn_err,sadn_err,h2on_err,xno2n_err,o3n_err,pvn,$
          pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn
    if ntraj eq 0 then goto,jumpday

; extract day, month, year information from trajectory file
    idy=strmid(string(ictime),8,2)
    imn=strmid(string(ictime),6,2)
    iyr=strmid(string(ictime),4,2)
    ihr=strmid(string(ictime),10,2)
    if strmid(iyr,0,1) eq '9' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4,A1,I2.2,A1)',$
       month(imn-1),' ',idy,', ',iyr+1900,' ',ihr,'Z'))
    if strmid(iyr,0,1) eq '0' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4,A1,I2.2,A1)',$
       month(imn-1),' ',idy,', ',iyr+2000,' ',ihr,'Z'))
;
;  read same day of UKMO isentropic data
;
    iflag=0
    ufile=mon(imn-1)+(string(FORMAT='(I2.2,A1,I2.2)',idy,'_',iyr))
    rd_ukmo_nc3,dirw+ufile+'.nc3',nc,nr,nl,x,alat,th,pv2,p2,msf2,$
             u2,v2,q2,qdf2,mark2,vp2,sf2,iflag
    if iflag eq 1 then goto,jumpday

; isolate UKMO in 2d on proper theta surface (ipv in PVU)
    u=fltarr(nc+1,nr)
    sf=fltarr(nc+1,nr)
    pv=fltarr(nc+1,nr)
    qdf=fltarr(nc+1,nr)
    mark=fltarr(nc+1,nr)
    x2=fltarr(nc+1)
    x2(0:nc-1)=x
    x2(nc)=x2(0)+360.
    freq=fltarr(nc+1,nr)
    y2d=fltarr(nc+1,nr)
    x2d=fltarr(nc+1,nr)
    for i=0,nc do y2d(i,*)=alat
    for j=0,nr-1 do begin
        x2d(*,j)=x2
        for i=0,nc-1 do begin
            u(i,j)=u2(j,i,k)
            sf(i,j)=sf2(j,i,k)
            pv(i,j)=pv2(j,i,k)
            qdf(i,j)=qdf2(j,i,k)
            mark(i,j)=mark2(j,i,k)
        endfor
        u(nc,*)=u(0,*)
        sf(nc,*)=sf(0,*)
        pv(nc,*)=pv(0,*)
        qdf(nc,*)=qdf(0,*)
        mark(nc,*)=mark(0,*)
    endfor
    stime=strcompress(string(ictime),/remove_all)

; plot UKMO quantity in top panel
    if setplot eq 'ps' then begin
       lc=0
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='sage2_freq_'+stheta+'_'+stime+'.ps'
       device,/color
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
    endif

    index=where(th0n eq thval,ntraj)
    if index(0) eq -1 then goto,jumpday
    xn=xn(index)
    yn=yn(index)
    agen=agen(index)
;
; calculate airmass frequency : number of trajectories
; within x km
;
    range=500.
    re=6.37E3
    dtr=!pi/180.
    npts=360.
    for j=nr/2,nr-1 do begin
        for i=0,nc-1 do begin
;
; USE RANGE RING
;
            range_ring,y2d(i,j),x2d(i,j),range,npts,bearing,latp,lonp
;
; if range ring does not cross the GM
;
            if min(lonp) gt 5 or max(lonp) lt 355. then $
               index=where( xn ge min(lonp) and xn le max(lonp) and $
                            yn ge min(latp) and yn le max(latp),num )
;
; if range ring crosses the GM
;
            if min(lonp) le 5 and max(lonp) ge 355. then $
               index=where( (xn le min(lonp) or xn ge max(lonp)) and $
                             yn ge min(latp) and yn le max(latp),num )
;
; first cut accomodation of ring enclosing pole
;
            if max(latp) ge max(alat) then $
               index=where(yn ge min(latp),num)

            if index(0) ne -1 then freq(i,j)=float(num)
        endfor
    endfor
    freq(nc,*)=freq(0,*)
;
; smooth
;
    save_freq=freq
    for j=nr/2,nr-1 do begin
        jm1=j-1
        jp1=j+1
        if j eq nr-1 then jp1=j
        for i=1,nc-1 do begin
            im1=i-1
            ip1=i+1
            freq(i,j)=(save_freq(ip1,jp1)+save_freq(im1,jm1)+$
                       save_freq(i,jp1)+save_freq(ip1,j)+$
                       save_freq(i,jm1)+save_freq(im1,j)+$
                       save_freq(ip1,jm1)+save_freq(im1,jp1)+$
                       2.*save_freq(i,j))/10.
        endfor
        im1=nc-1
        ip1=1
        freq(0,j)=(save_freq(ip1,jp1)+save_freq(im1,jm1)+$
                   save_freq(0,jp1)+save_freq(ip1,j)+$
                   save_freq(0,jm1)+save_freq(im1,j)+$
                   save_freq(ip1,jm1)+save_freq(im1,jp1)+$
                   2.*save_freq(i,j))/10.
    endfor
    freq(nc,*)=freq(0,*)

    !type=2^2+2^3
    erase
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !psym=0
    MAP_SET,90,0,-90,/GRID,/CONTIN,/ortho,/noeras,latdel=10.,$
            /noborder,color=lc
    oplot,findgen(361),0.5+0.*findgen(361)
;   contour,sf,x2,y,nlevels=10,/overplot

    index=where(y2d gt 0. and mark eq 1.0)
    if index(0) ne -1 then begin
       smax=max(sf(index))
       index=where(y2d lt 30.)
       sf(index)=99999.
       contour,sf,x2,alat,levels=smax,thick=10,/overplot,$
               max_value=99999.
    endif

    level=[5.,20.,50.,100.,150.,200.,300.,400.,500.,750.,1000.]
    contour,freq,x2,alat,levels=level,/follow,c_colors=[lc],thick=2,$
           /overplot,c_labels=1+0*level,c_linestyle=level lt 0,min_value=-9999.
    contour,freq,x2,alat,levels=[100.,200.,300.,400.,500],/follow,thick=5,$
           /overplot,c_labels=0*findgen(5),c_colors=[lc],min_value=-9999.

    print,max(freq)
;
; color by ozone
;
    o3min=2.
    o3max=12.
    for itraj=0,ntraj-1 do $
        oplot,[xn(itraj),xn(itraj)],$
              [yn(itraj),yn(itraj)],$
               color=((1.e6*o3n(itraj)-o3min)/(o3max-o3min))*mcolor,psym=8

; color bar for ATMOS data
    imin=o3min
    imax=o3max
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
          xtitle='!6Trajectory Age (hours)',charsize=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
    xyouts,.22,.92,'SAGE II Airmass frequency',/normal,charsize=3
    xyouts,.28,.15,date+'     '+stheta,/normal,charsize=3

;stop
jumpday:
ntrajold=ntraj
if setplot eq 'ps' then device, /close
endfor		; loop over days
jumprun:
endfor		; loop over airmass runs
end
