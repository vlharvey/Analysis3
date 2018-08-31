;
; store monthly ozone values in highs and out
;
@read_poam3_header
@read_poam3_airmass
@read_sage2_header
@read_sage2_airmass
@read_haloe_header
@read_haloe_airmass
@rd_ukmo_nc3

thlab=['2000K','1800K','1600K','1400K','1200K','1000K','900K',$
       '800K','700K','600K','550K','525K','500K','475K','450K',$
       '425K','400K','390K','380K','370K','360K','350K','340K','330K']
thlev=reverse([330.,340.,350.,360.,370.,380.,390.,400.,425.,450.,475.,$
               500.,525.,550.,600.,700.,800.,900.,1000.,1200.,1400.,$
               1600.,1800.,2000.])
ntheta=n_elements(thlev)
dirp='/aura2/harvey/Airmass/POAM/'
dirs='/aura2/harvey/Airmass/SAGEII/'
dirh='/aura2/harvey/Airmass/HALOE/'
ifile='                        '
nmon=10000L
nvals=1.e6
pcount=0L & scount=0L & hcount=0L
close,1,2,3
open,1,'poam3_airmass.fil'
pfile=strarr(nmon)
while not eof(1) do begin
      readf,1,ifile
      pfile(pcount)=ifile
      pcount=pcount+1L
endwhile
pfile=pfile(0:pcount-1L)
open,2,'sage2_airmass.fil'
sfile=strarr(nmon)
while not eof(2) do begin
      readf,2,ifile
      sfile(scount)=ifile
      scount=scount+1L
endwhile
sfile=sfile(0:scount-1L)
open,3,'haloe_airmass.fil'
hfile=strarr(nmon)
while not eof(3) do begin
      readf,3,ifile
      hfile(hcount)=ifile
      hcount=hcount+1L
endwhile
hfile=hfile(0:hcount-1L)
close,1,2,3
if pcount ne scount or pcount ne hcount stop
kfile=pcount
for kk=0,kfile-1 do begin
iflag=0L
close,10,11,12
openr,10,dirs+sfile(kk),/f77
read_sage2_header,10,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
openr,11,dirh+hfile(kk),/f77
read_haloe_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
openr,12,dirp+pfile(kk),/f77
read_poam3_header,12,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
;
; retain all values in these arrays
;
ozone=fltarr(nvals)
latitude=fltarr(nvals)
longitude=fltarr(nvals)
theta=fltarr(nvals)
latitude0=fltarr(nvals)
longitude0=fltarr(nvals)
theta0=fltarr(nvals)
marker=fltarr(nvals)
inst=fltarr(nvals)
date=lonarr(nvals)

istime=0L & ictime=0L & time=0. & ntraj=0L
for n=0,nfiles-1 do begin
    if n eq nfiles-1 then goto,jumprun

    READU,10,istime,ictime,time,ntraj
    print,'SAGE ',istime,ictime,time,ntraj
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

        xday=[-9999.]
        yday=[-9999.]
        thday=[-9999.]
        x0day=[-9999.]
        y0day=[-9999.]
        th0day=[-9999.]
        o3day=[-9999.]
        markday=[-9999.]
        instday=[-9999.]
        dateday=[-9999L]
        index=where(abs(o3ns) lt 100.,ns)
        if index(0) ne -1 then begin
           xday=[xday,xns(index)]
           yday=[yday,yns(index)]
           thday=[thday,thns(index)]
           x0day=[x0day,x0ns(index)]
           y0day=[y0day,y0ns(index)]
           th0day=[th0day,th0ns(index)]
           o3day=[o3day,o3ns(index)*1e6]
           markday=[markday,markns(index)]
           instday=[instday,1.+0.*fltarr(ns)]
           dateday=[dateday,ictime+0L*lonarr(ns)]
        endif
        index=where(abs(o3nh) lt 100.,nh)
        if index(0) ne -1 then begin
           xday=[xday,xnh(index)]
           yday=[yday,ynh(index)]
           thday=[thday,thnh(index)]
           x0day=[x0day,x0nh(index)]
           y0day=[y0day,y0nh(index)]
           th0day=[th0day,th0nh(index)]
           o3day=[o3day,o3nh(index)*1e6]
           markday=[markday,marknh(index)]
           instday=[instday,1.+0.*fltarr(nh)]
           dateday=[dateday,ictime+0L*lonarr(nh)]
        endif
        index=where(abs(o3np) lt 100.,np)
        if index(0) ne -1 then begin
           xday=[xday,xnp(index)]
           yday=[yday,ynp(index)]
           thday=[thday,thnp(index)]
           x0day=[x0day,x0np(index)]
           y0day=[y0day,y0np(index)]
           th0day=[th0day,th0np(index)]
           o3day=[o3day,o3np(index)*1e6]
           markday=[markday,marknp(index)]
           instday=[instday,1.+0.*fltarr(np)]
           dateday=[dateday,ictime+0L*lonarr(np)]
        endif

;ozone=fltarr(nvals)
;latitude=fltarr(nvals)
;longitude=fltarr(nvals)
;theta=fltarr(nvals)
;latitude0=fltarr(nvals)
;longitude0=fltarr(nvals)
;theta0=fltarr(nvals)
;marker=fltarr(nvals)
;inst=fltarr(nvals)
;date=lonarr(nvals)
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !psym=0
    erase
    plot,-2+findgen(5),findgen(12),xtitle='!6Marker',ytitle='!6Ozone',$
         yrange=[2.,12.],/nodata
    o3max=10.
    o3min=2.
    endif
    index=where(yns lt 0. and th0ns eq thval and abs(o3ns) lt 1.,ntraj)
    if index(0) ne -1 then begin
       xns=xns(index)
       yns=yns(index)
       o3ns=o3ns(index)*1e6
       markns=markns(index)
       oplot,markns,o3ns,psym=8
    endif
    index=where(ynh lt 0. and th0nh eq thval and abs(o3nh) lt 1.,ntraj)
    if index(0) ne -1 then begin
       xnh=xnh(index)
       ynh=ynh(index)
       o3nh=o3nh(index)*1e6
       marknh=marknh(index)
       oplot,marknh,o3nh,psym=8
    endif
    index=where(ynp lt 0. and th0np eq thval and abs(o3np) lt 1.,ntraj)
    if index(0) ne -1 then begin
       xnp=xnp(index)
       ynp=ynp(index)
       o3np=o3np(index)*1e6
       marknp=marknp(index)
       oplot,marknp,o3np,psym=8
    endif
stop

endfor		; loop over output times
jumprun:
endfor		; loop over airmass runs
end
