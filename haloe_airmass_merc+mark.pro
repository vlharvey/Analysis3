;
; mercator projection with ozone and vortices superimposed
;
@read_haloe_header
@read_haloe_airmass
@rd_ukmo_nc3

SETPLOT='ps'
read,'setplot=',setplot
print,' '
ihem=1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
loadct,38
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.35]
xlen=0.7
ylen=0.4
cbaryoff=0.03
cbarydel=0.02
!NOERAS=-1

thlab=['2000K','1800K','1600K','1400K','1200K','1000K','900K',$
       '800K','700K','600K','550K','525K','500K','475K','450K',$
       '425K','400K','390K','380K','370K','360K','350K','340K','330K']
thlev=reverse([330.,340.,350.,360.,370.,380.,390.,400.,425.,450.,475.,$
               500.,525.,550.,600.,700.,800.,900.,1000.,1200.,1400.,$
               1600.,1800.,2000.])
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mday =[31,28,31,30,31,30,31,31,30,31,30,31]
thval=1000.
;print,thlev
;read,'enter theta level ',thval
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
dirh='/aura2/harvey/Airmass/HALOE/'
hfile=[$
'ukmo_haloe_oct_2002_forward.airm']
kfile=n_elements(hfile)
for kk=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kk),/f77
read_haloe_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
ntraj=0L
for n=0,nfiles-1 do begin
    if n eq nfiles-1 then goto,jumprun
;
; HALOE airmass
;
    READU,11,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
    if ntraj gt 0 then $
       read_haloe_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,$
          ch4n,hfn,h2on,o3n,hcln,xno2n,xnon,aern,halcompn,haldensn,$
          halmedrn,haldiswn,halconcn,halsurfn,halvolun,haleffrn,sage1n,$
          sage2n,sage3n,sage4n,ch4n_err,hfn_err,h2on_err,o3n_err,hcln_err,$
          xno2n_err,xnon_err,aern_err,halcompn_err,haldensn_err,halmedrn_err,$
          haldiswn_err,halconcn_err,halsurfn_err,halvolun_err,haleffrn_err,$
          sage1n_err,sage2n_err,sage3n_err,sage4n_err,pvn,pn,msfn,zn,tmpn,qn,$
          qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn

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
    u=fltarr(nc,nr)
    sf=fltarr(nc,nr)
    pv=fltarr(nc,nr)
    markl=fltarr(nc,nr)
    markh=fltarr(nc,nr)
    y2d=fltarr(nc,nr)
    x2d=fltarr(nc,nr)
    for i=0,nc-1 do y2d(i,*)=alat
    for j=0,nr-1 do x2d(*,j)=x
    for j=0,nr-1 do begin
        for i=0,nc-1 do begin
            u(i,j)=u2(j,i,k)
            sf(i,j)=sf2(j,i,k)
            pv(i,j)=pv2(j,i,k)
            markl(i,j)=mark2(j,i,k)
            markh(i,j)=mark2(j,i,k)
        endfor
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
       device,/landscape,bits=8,filename='haloe_o3_'+stheta+$
              '_'+stime+'+mark_merc.ps'
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
;    o3max=3.   ; 2000 K
;    o3min=2.
     o3max=12.	; 1000 K
     o3min=2.
;    o3max=9.   ; 700/1200 K
;    o3min=2.
;    o3max=2.	; 400 K
;    o3min=0.
    smin=' ' & smax=' '
    if ntraj gt 0L then begin
        index=where(th0n eq thval and abs(o3n) lt 1.,ntraj)
        if index(0) ne -1 then begin
           xn=xn(index)
           yn=yn(index)
           thn=thn(index)
           x0n=x0n(index)
           y0n=y0n(index)
           th0n=th0n(index)
           agen=agen(index)
           o3n=o3n(index)*1e6
        endif
        smin=string(format='(f5.2)',min(agen)/24.)
        smax=string(format='(f5.2)',max(agen)/24.)
    endif
    xyouts,.35,.8,'HALOE Ozone '+stheta,/normal,charsize=2
    MAP_SET,0,180,0,/noeras,charsize=1.5,$
            title=date+' Trajectories '+smin+' to '+smax+' days old'
    if ntraj gt 0 then begin
       if index(0) ne -1 then begin
          print,min(o3n),max(o3n),max(agen)
          for itraj=0,ntraj-1 do $
              oplot,[xn(itraj),xn(itraj)],[yn(itraj),yn(itraj)],psym=8,$
                    color=((o3n(itraj)-o3min)/(o3max-o3min))*mcolor
       endif
    endif
    index=where(y2d gt -100.)
    sfmin=min(sf(index))*1.e7
    sfmax=max(sf(index))*1.e7
    sfint=(sfmax-sfmin)/20.
    sfbin=sfmin+sfint*findgen(20)
    contour,sf*1.e7,x,alat,/overplot,levels=sfbin,/noeras,color=icolmax,$
            c_labels=0*sfbin,max_value=9999.,c_charsize=2
    contour,markl,x,alat,/overplot,levels=[-0.1,0.1],/noeras,color=lc,$
            c_labels=[0,0],c_charsize=2,thick=4
    MAP_SET,0,180,0,/noeras,/grid,/contin

; color bar for Ozone data
    imin=o3min
    imax=o3max
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
          xtitle='(ppmv)',charsize=2
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
       spawn,'convert haloe_o3_'+stheta+'_'+stime+'+mark_merc.ps -rotate -90 '+$
             ' haloe_o3_'+stheta+'_'+stime+'+mark_merc.jpg'
    endif
;   if ntraj gt 0 and setplot eq 'x' then stop
jumpday:
endfor		; loop over days
jumprun:
endfor		; loop over airmass runs
end
