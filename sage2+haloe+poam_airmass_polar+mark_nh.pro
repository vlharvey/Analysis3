;
; plot all trajectory air masses
;
@read_poam3_header
@read_poam3_airmass
@read_sage2_header
@read_sage2_airmass
@read_sage3_header
@read_sage3_airmass
@read_haloe_header
@read_haloe_airmass
@rd_ukmo_nc3

SETPLOT='x'
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
   lc=icolmax
   lc2=0
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
nlvls=20
col1=5+indgen(nlvls)*mcolor/nlvls
dirw='/usr72/users/ukmo/Datfiles/ukmo_'
dirp='/aura2/harvey/Airmass/POAM3/'
dirh='/aura2/harvey/Airmass/HALOE/'
dirs='/aura2/harvey/Airmass/SAGEII/'
dirs3='/aura2/harvey/Airmass/SAGEIII/'
pfile=[$
'ukmo_poam3_jan_2003_3d.airm.1']
sfile=[$
'ukmo_sage2_jan_2003_3d.airm']
s3file=[$
'ukmo_sage3_jan_2003_3d.airm']
hfile=[$
'ukmo_haloe_jan_2003_3d.airm']
kfile=n_elements(pfile)
for kk=0L,kfile-1 do begin

close,10,11,12,13
openr,10,dirs+sfile(kk),/f77
read_sage2_header,10,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
openr,11,dirh+hfile(kk),/f77
read_haloe_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
openr,12,dirp+pfile(kk),/f77
read_poam3_header,12,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
;openr,13,dirs3+s3file(kk),/f77
;read_sage3_header,13,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
;    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
dirw=strcompress(dirw,/remove_all)
ntraj=0L
ynp=[0.] & yns=[0.] & yns3=[0.] & ynh=[0.] 
th0np=[0.] & th0ns=[0.] & th0ns3=[0.] & th0nh=[0.] 
agenp=[0.] & agens=[0.] & agens3=[0.] & agenh=[0.] 
xnp=[0.] & xns=[0.] & xns3=[0.] & xnh=[0.]
o3np=[0.] & o3ns=[0.] & o3ns3=[0.] & o3nh=[0.]
noccul=30L*nfiles
icount=0L
for n=0L,nfiles-1 do begin

    READU,10,istime,ictime,time,ntraj
    print,'SAGE II ',istime,ictime,time,ntraj
    if ntraj gt 0L then $
       read_sage2_airmass,10,ntraj,xns,yns,thns,agens,x0ns,y0ns,th0ns,t0ns,z0ns,p0ns,extns,$
          sadns,h2ons,xno2ns,o3ns,extns_err,sadns_err,h2ons_err,xno2ns_err,o3ns_err,pvns,$
          pns,msfns,zns,tmpns,qns,qdfns,markns,frdays,frnghts,xmints,minttimes,vpns,sfns

    READU,11,istime,ictime,time,ntraj
    print,'HALOE ',istime,ictime,time,ntraj
    if ntraj gt 0L then $
       read_haloe_airmass,11,ntraj,xnh,ynh,thnh,agenh,x0nh,y0nh,th0nh,t0nh,z0nh,p0nh,$
          ch4nh,hfnh,h2onh,o3nh,hclnh,xno2nh,xnonh,aernh,halcompnh,haldensnh,$
          halmedrnh,haldiswnh,halconcnh,halsurfnh,halvolunh,haleffrnh,sage1nh,$
          sage2nh,sage3nh,sage4nh,ch4nh_err,hfnh_err,h2onh_err,o3nh_err,hclnh_err,$
          xno2nh_err,xnonh_err,aernh_err,halcompnh_err,haldensnh_err,halmedrnh_err,$
          haldiswnh_err,halconcnh_err,halsurfnh_err,halvolunh_err,haleffrnh_err,$
          sage1nh_err,sage2nh_err,sage3nh_err,sage4nh_err,pvnh,pnh,msfnh,znh,tmpnh,qnh,$
          qdfnh,marknh,frdayh,frnghth,xminth,minttimeh,vpnh,sfnh

    READU,12,istime,ictime,time,ntraj
    print,'POAM ',istime,ictime,time,ntraj
    if ntraj gt 0L then $
       read_poam3_airmass,12,ntraj,xnp,ynp,thnp,agenp,x0np,y0np,th0np,t0np,z0np,p0np,extnp,$
          sadnp,h2onp,xno2np,o3np,extnp_err,sadnp_err,h2onp_err,xno2np_err,o3np_err,pvnp,$
          pnp,msfnp,znp,tmpnp,qnp,qdfnp,marknp,frdayp,frnghtp,xmintp,minttimep,vpnp,sfnp

;   READU,13,istime,ictime,time,ntraj
;   print,'SAGE III ',istime,ictime,time,ntraj
;   if ntraj gt 0 then $
;      read_sage3_airmass,13,ntraj,xns3,yns3,thns3,agens3,x0ns3,y0ns3,th0ns3,t0ns3,z0ns3,p0ns3,extns3,$
;         sadns3,h2ons3,xno2ns3,o3ns3,extns3_err,sadns3_err,h2ons3_err,xno2ns3_err,o3ns3_err,pvns3,$
;         pns3,msfns3,zns3,tmpns3,qns3,qdfns3,markns3,frdays3,frnghts3,xmints3,minttimes3,vpns3,sfns3

; extract day, month, year information from trajectory file
    ihr=strmid(string(ictime),10,2)
    if ihr ne '12' then goto,jumpday
    idy=strmid(string(ictime),8,2)
    imn=strmid(string(ictime),6,2)
    iyr=strmid(string(ictime),4,2)
    if strmid(iyr,0,1) eq '9' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4)',$
       month(imn-1),' ',idy,', ',iyr+1900))
    if strmid(iyr,0,1) eq '0' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4)',$
       month(imn-1),' ',idy,', ',iyr+2000))
;
;  read same day of UKMO isentropic data
;
    iflag=0
    ufile=mon(imn-1)+(string(FORMAT='(I2.2,A1,I2.2)',idy,'_',iyr))
    print,ufile
    rd_ukmo_nc3,dirw+ufile+'.nc3',nc,nr,nl,x2,y,th,pv2,p2,msf2,$
             u2,v2,q2,qdf2,mark2,vp2,sf2,iflag
;
; isolate UKMO in 2d on proper theta surface (ipv in PVU)
;
;   if icount eq 0L then begin
       thval=800.
;      print,th
;      read,'enter theta level ',thval
       k=where(thval eq th)
       k=k(0)
       stheta=strcompress(string(long(th(k))),/remove_all)+'K'
;      icount=1L
;   endif

    u=fltarr(nc+1,nr)
    sf=fltarr(nc+1,nr)
    pv=fltarr(nc+1,nr)
    mark=fltarr(nc+1,nr)
    x=fltarr(nc+1)
    x(0:nc-1)=x2
    x(nc)=x2(0)+360.
    y2d=fltarr(nc+1,nr)
    x2d=fltarr(nc+1,nr)
    for i=0L,nc   do y2d(i,*)=y
    for j=0L,nr-1 do x2d(*,j)=x
    for j=0L,nr-1 do begin
        for i=0L,nc-1 do begin
            u(i,j)=u2(j,i,k)
            sf(i,j)=sf2(j,i,k)
            pv(i,j)=pv2(j,i,k)
            mark(i,j)=mark2(j,i,k)
        endfor
        u(nc,*)=u(0,*)
        sf(nc,*)=sf(0,*)
        pv(nc,*)=pv(0,*)
        mark(nc,*)=mark(0,*)
    endfor
    stime=strcompress(string(ictime),/remove_all)

; plot UKMO quantity in top panel
    if setplot eq 'ps' then begin
       lc=0
       lc2=icolmax
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,filename='sage2+haloe+poam_o3_'+stheta+$
              '_'+stime+'+mark_nh.ps'
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
    !psym=0
    MAP_SET,ihem*90,0,-90*ihem,/stereo,/noeras,/grid,/noborder,charsize=1.5,$
            title='!6'+date+' '+stheta+' Ozone',/contin
    oplot,findgen(361),0.*findgen(361),psym=3
    index=where(y2d gt 0.)
    if index(0) ne -1 then begin
       sfbin=-30.+2*findgen(40)
       contour,sf*1.e7,x,y,/overplot,levels=sfbin,/noeras,color=lc,$
               c_labels=0*sfbin,max_value=9999.,c_charsize=2
    endif
;
; mark vortices
;
    index=where(y2d gt 0. and mark eq 1.0)
    if index(0) ne -1 then begin
       oplot,x2d(index),y2d(index),psym=8,color=lc,symsize=2
;      contour,mark,x2d,y2d,levels=[0.1],color=mcolor*.05,c_labels=[0],$
;              thick=5,/overplot,max_value=9999.
    endif
    index=where(y2d gt 0. and mark lt 0.0)
    if index(0) ne -1 then begin
       oplot,x2d(index),y2d(index),psym=8,color=lc,symsize=2
;      contour,mark,x2d,y2d,levels=[-0.1],color=mcolor*.9,c_labels=[0],$
;              thick=5,/overplot,max_value=9999.
    endif

    o3max=10.
    o3min=3.
    index=where(yns gt 0. and th0ns eq thval and abs(o3ns) lt 1. and agens lt 2400.,ntraj)
    if index(0) ne -1 then begin
       xns=xns(index)
       yns=yns(index)
       o3ns=o3ns(index)*1e6
;print,'SAGE II ',min(o3ns),max(o3ns)
    endif
    index=where(yns3 gt 0. and th0ns3 eq thval and abs(o3ns3) lt 1. and agens3 lt 2400.,ntraj)
    if index(0) ne -1 then begin
       xns3=xns3(index)
       yns3=yns3(index)
       o3ns3=o3ns3(index)*1e6
;print,'SAGE III ',min(o3ns3),max(o3ns3)
    endif
    index=where(ynh gt 0. and th0nh eq thval and abs(o3nh) lt 1. and agenh lt 2400.,ntraj)
    if index(0) ne -1 then begin
       xnh=xnh(index)
       ynh=ynh(index)
       o3nh=o3nh(index)*1e6
;print,'HALOE ',min(o3nh),max(o3nh)
    endif
    index=where(ynp gt 0. and th0np eq thval and abs(o3np) lt 1. and agenp lt 2400.,ntraj)
    if index(0) ne -1 then begin
       xnp=xnp(index)
       ynp=ynp(index)
       o3np=o3np(index)*1e6
;print,'POAM ',min(o3np),max(o3np)
    endif

    index=where(o3ns lt 3.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 lt 3.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh lt 3.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np lt 3.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 3. and o3ns lt 4.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 3. and o3ns3 lt 4.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 3. and o3nh lt 4.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 3. and o3np lt 4.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 4. and o3ns lt 5.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 4. and o3ns3 lt 5.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 4. and o3nh lt 5.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 4. and o3np lt 5.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 5. and o3ns lt 6.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 5. and o3ns3 lt 6.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 5. and o3nh lt 6.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 5. and o3np lt 6.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 6. and o3ns lt 7.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 6. and o3ns3 lt 7.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 6. and o3nh lt 7.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 6. and o3np lt 7.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 7. and o3ns lt 8.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 7. and o3ns3 lt 8.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 7. and o3nh lt 8.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 7. and o3np lt 8.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 8. and o3ns lt 9.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 8. and o3ns3 lt 9.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 8. and o3nh lt 9.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 8. and o3np lt 9.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 9. and o3ns lt 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 9. and o3ns3 lt 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 9. and o3nh lt 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 9. and o3np lt 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    index=where(o3ns ge 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns(index(itraj)),xns(index(itraj))],$
              [yns(index(itraj)),yns(index(itraj))],psym=8,$
              color=((o3ns(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3ns3 ge 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xns3(index(itraj)),xns3(index(itraj))],$
              [yns3(index(itraj)),yns3(index(itraj))],psym=8,$
              color=((o3ns3(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3nh ge 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnh(index(itraj)),xnh(index(itraj))],$
              [ynh(index(itraj)),ynh(index(itraj))],psym=8,$
              color=((o3nh(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1
    index=where(o3np ge 10.,ntraj)
    if index(0) ne -1 then $
    for itraj=0L,ntraj-1 do $
        oplot,[xnp(index(itraj)),xnp(index(itraj))],$
              [ynp(index(itraj)),ynp(index(itraj))],psym=8,$
              color=((o3np(index(itraj))-o3min)/(o3max-o3min))*mcolor,symsize=1

    loadct,0
    contour,mark,x2d,y2d,levels=[-0.1],color=icolmax*.8,c_labels=[0],$
            thick=10,/overplot,max_value=9999.,c_linestyle=0
    contour,mark,x2d,y2d,levels=[0.1],color=icolmax*.5,c_labels=[0],$
            thick=10,/overplot,max_value=9999.,c_linestyle=0

; color bar for Ozone data
loadct,38
    imin=o3min
    imax=o3max
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
          xtitle='!6(ppmv)',charsize=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0L,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
    if setplot ne 'ps' then wait,1
    if setplot eq 'ps' then device, /close
jumpday:
endfor		; loop over days
jumprun:
endfor		; loop over airmass runs
end
