;
; polarator projection with ozone and vortices superimposed
;
@read_poam3_header
@read_poam3_airmass
@rd_ukmo_nc3

SETPLOT='ps'
read,'setplot=',setplot
print,' '
ihem=1
a=findgen(8)*(2*!pi/8.)
usersym,0.8*cos(a),0.8*sin(a),/fill
loadct,38
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
nxdim=750
nydim=750
xorig=[0.15,0.15,0.15]
yorig=[0.70,0.40,0.10]
xlen=0.65
ylen=0.225
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
thval=700.
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
dirh='/aura2/harvey/Airmass/POAM3/'
hfile=[$
'ukmo_poam3_SOLVE2_back.airm']
kfile=n_elements(hfile)
for kl=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kl),/f77
read_poam3_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
ntraj=0L
noccul=30L*nfiles
for n=0,nfiles-1 do begin
;   if n eq nfiles-1 then goto,jumprun
;
; POAM 3 airmass
;
    READU,11,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
    if ntraj gt 0L then $
       read_poam3_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,extn,$
          sadn,h2on,xno2n,o3n,extn_err,sadn_err,h2on_err,xno2n_err,o3n_err,pvn,$
          pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn
    if n eq 0L then begin
       xsave=-9999.+fltarr(noccul,nfiles)
       ysave=-9999.+fltarr(noccul,nfiles)
       zsave=-9999.+fltarr(noccul,nfiles)
       x0save=-9999.+fltarr(noccul,nfiles)
       y0save=-9999.+fltarr(noccul,nfiles)
       z0save=-9999.+fltarr(noccul,nfiles)
       o3save=-9999.+fltarr(noccul,nfiles)
       agesave=-9999.+fltarr(noccul,nfiles)
       tmpsave=-9999.+fltarr(noccul,nfiles)
       mintsave=-9999.+fltarr(noccul,nfiles)
       marksave=-9999.+fltarr(noccul,nfiles)
       frdaysave=-9999.+fltarr(noccul,nfiles)
       stimesave=-9999L+lonarr(noccul,nfiles)
       ctimesave=-9999L+lonarr(noccul,nfiles)
       ctimes=-9999L+lonarr(nfiles)
       stimes=strarr(nfiles)
    endif
    ctimes(n)=ictime

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
 
;  read same day of UKMO isentropic data
 
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
    stimes(n)=strcompress(string(ictime),/remove_all)

    smin=' ' & smax=' '
    if ntraj gt 0L then begin
        index=where(th0n eq thval and y0n gt 0. and abs(o3n) lt 1.,ntraj)
        if index(0) ne -1 then begin
           xn=xn(index)
           yn=yn(index)
           thn=thn(index)
           x0n=x0n(index)
           y0n=y0n(index)
           th0n=th0n(index)
           t0n=t0n(index)
           z0n=z0n(index)
           p0n=p0n(index)
           agen=agen(index)
           o3n=o3n(index)*1e6
           extn=extn(index)
           sadn=sadn(index)
           h2on=h2on(index)*1.e6
           xno2n=xno2n(index)*1.e9
           pvn=pvn(index)
           pn=pn(index)
           msfn=msfn(index)
           zn=zn(index)
           tmpn=tmpn(index)
           qn=qn(index)
           qdfn=qdfn(index)
           markn=markn(index)
           frday=frday(index)
           frnght=frnght(index)
           xmint=xmint(index)
           minttime=minttime(index)
           vpn=vpn(index)
           sfn=sfn(index)

           xsave(0:ntraj-1L,n)=xn
           ysave(0:ntraj-1L,n)=yn
           zsave(0:ntraj-1L,n)=zn
           x0save(0:ntraj-1L,n)=x0n
           y0save(0:ntraj-1L,n)=y0n
           z0save(0:ntraj-1L,n)=z0n
           agesave(0:ntraj-1L,n)=agen
           o3save(0:ntraj-1L,n)=o3n
           tmpsave(0:ntraj-1L,n)=tmpn
           mintsave(0:ntraj-1L,n)=xmint
           marksave(0:ntraj-1L,n)=markn
           frdaysave(0:ntraj-1L,n)=frday*24.	; total sunlit hours
           stimesave(0:ntraj-1L,n)=istime
           ctimesave(0:ntraj-1L,n)=ictime
        endif
        smin=string(format='(f6.2)',min(agen)/24.)
        smax=string(format='(f6.2)',max(agen)/24.)
    endif
    jumpday:
endfor          ; loop over days

if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='poam3_o3_'+stheta+'_sunlit_hours.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
endif

!type=2^2+2^3
erase
xyouts,.33,.96,'2002-2003   '+stheta,charsize=2,/normal
;xyouts,.33,.96,'1999-2000   '+stheta,charsize=2,/normal
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
stimes=reverse(stimes)
xindex=where(strmid(stimes,6,2) eq '01' or strmid(stimes,6,2) eq '15',nxtick)
plot,1.+findgen(nfiles),2.+.1*findgen(31),/noeras,/nodata,$
     title='Vortex Average Ozone',xticks=nxtick-1,xtickv=xindex,xtickname=' '+strarr(nxtick)
xyouts,xmn+0.02,ymn+0.01,'T<200 K',/normal,color=mcolor*.3
xlabels=strmid(stimes(xindex),4,4)
for i=0,nxtick-1 do xyouts,xindex(i),1.5,xlabels(i),/data,orientation=90.
dum=fltarr(nfiles)
for ii=0L,nfiles-1L do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii))
    if index(0) ne -1 then begin
       ozone=total(o3save(index,ii))/n_elements(index)
       dum(kk)=ozone
       oplot,[kk,kk],[ozone,ozone],psym=8
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue
    endif
endfor
dum=fltarr(nfiles)
for ii=0L,nfiles-1L do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                tmpsave(*,ii) le 200. and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii))
    if index(0) ne -1 then begin
       ozone=total(o3save(index,ii))/n_elements(index)
       dum(kk)=ozone
       oplot,[kk,kk],[ozone,ozone],psym=8,color=mcolor*.3
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue,color=mcolor*.3
    endif
endfor

!type=2^2+2^3
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1.+findgen(nfiles),24.*findgen(10),/noeras,/nodata,xticks=nxtick-1,$
     xtickv=xindex,xtickname=' '+strarr(nxtick),title='Maximum Total Sunlit Hours'
xyouts,xmn+0.02,ymn+0.01,'T<200 K',/normal,color=mcolor*.3
for i=0,nxtick-1 do xyouts,xindex(i),-40.,xlabels(i),/data,orientation=90.
dum=fltarr(nfiles)
for ii=0L,nfiles-11 do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii),ntraj)
    if index(0) ne -1 then begin
       totsun=fltarr(ntraj)
       for jj=0L,ntraj-1L do $
           totsun(jj)=total(frdaysave(index(jj),ii:ii+10))
;      print,max(totsun)
;      totsun=totsun/ntraj
       dum(kk)=max(totsun)
;      oplot,[kk,kk],[totsun,totsun],psym=8
       oplot,[kk,kk],[max(totsun),max(totsun)],psym=8
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue
    endif
endfor
dum=fltarr(nfiles)
for ii=0L,nfiles-11 do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                tmpsave(*,ii) le 200. and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii),ntraj)
    if index(0) ne -1 then begin
       totsun=fltarr(ntraj)
       for jj=0L,ntraj-1L do $
           totsun(jj)=total(frdaysave(index(jj),ii:ii+10))
;      print,max(totsun)
;      totsun=totsun/ntraj
       dum(kk)=max(totsun)
;      oplot,[kk,kk],[totsun,totsun],psym=8
       oplot,[kk,kk],[max(totsun),max(totsun)],psym=8,color=mcolor*.3
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue,color=mcolor*.3
    endif
endfor


!type=2^2+2^3
xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,1.+findgen(nfiles),180.+findgen(46),/noeras,/nodata,xticks=nxtick-1,$
     xtickv=xindex,xtickname=' '+strarr(nxtick),title='Minimum Vortex Temperature'
xyouts,xmn+0.02,ymn+0.01,'T<200 K',/normal,color=mcolor*.3
for i=0,nxtick-1 do xyouts,xindex(i),172.,xlabels(i),/data,orientation=90.
dum=fltarr(nfiles)
for ii=0L,nfiles-1L do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii))
    if index(0) ne -1 then begin
       temp=min(tmpsave(index,ii))
       dum(kk)=temp
;      print,ctimes(ii),temp
       oplot,[kk,kk],[temp,temp],psym=8
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue
    endif
endfor
dum=fltarr(nfiles)
for ii=0L,nfiles-1L do begin
    kk=nfiles-ii-1
    index=where(ysave(*,ii) gt 0.0 and $
                marksave(*,ii) eq 1.0 and $
                tmpsave(*,ii) le 200. and $
                abs(agesave(*,ii)) le 24. and $
                ctimesave(*,ii) eq ctimes(ii))
    if index(0) ne -1 then begin
       temp=min(tmpsave(index,ii))
       dum(kk)=temp
;      print,ctimes(ii),temp
       oplot,[kk,kk],[temp,temp],psym=8,color=mcolor*.3
    endif
endfor
for ii=0,nfiles-2 do begin
    if dum(ii) ne 0. and dum(ii+1) ne 0. then begin
       plots,ii,dum(ii)
       plots,ii+1,dum(ii+1),/continue,color=mcolor*.3
    endif
endfor

if setplot eq 'ps' then device, /close
if ntraj gt 0 and setplot eq 'x' then stop
jumprun:
endfor		; loop over airmass runs
end
