;
; Altitude-time section showing the average sunlight hours for 
; airmasses in the vortex
;
@read_ace_header
@read_ace_airmass

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
cbaryoff=0.15
cbarydel=0.02
!NOERAS=-1
thlev=reverse([400.,450.,500.,550.,600.,700.,800.,900.,$
               1000.,1200.,1400.,1600.,1800.,2000.])
nth=n_elements(thlev)
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dirw='/aura3/data/UKMO_data/Datfiles/ukmo_'
dirh='/aura2/harvey/Airmass/ACE/'
hfile=[$
'ukmo_ace_2004-2005_3d_back.airm']
kfile=n_elements(hfile)
for kk=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kk),/f77
read_ace_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
ntraj=0L
ntrajmax=25000L
for n=0,nfiles-1 do begin
;
; ACE airmass
;
    READU,11,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
    if ntraj gt 0L then $
       read_ace_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,extn,$
          sadn,h2on,xno2n,o3n,extn_err,sadn_err,h2on_err,xno2n_err,o3n_err,pvn,$
          pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn
;
; retain all trajectory information for all days
;
    if n eq 0L then begin
       xsave=-9999.+fltarr(ntrajmax,nfiles)
       ysave=-9999.+fltarr(ntrajmax,nfiles)
       zsave=-9999.+fltarr(ntrajmax,nfiles)
       x0save=-9999.+fltarr(ntrajmax,nfiles)
       y0save=-9999.+fltarr(ntrajmax,nfiles)
       z0save=-9999.+fltarr(ntrajmax,nfiles)
       o3save=-9999.+fltarr(ntrajmax,nfiles)
       agesave=-9999.+fltarr(ntrajmax,nfiles)
       pvsave=-9999.+fltarr(ntrajmax,nfiles)
       tmpsave=-9999.+fltarr(ntrajmax,nfiles)
       mintsave=-9999.+fltarr(ntrajmax,nfiles)
       marksave=-9999.+fltarr(ntrajmax,nfiles)
       frdaysave=-9999.+fltarr(ntrajmax,nfiles)
       stimesave=-9999L+lonarr(ntrajmax,nfiles)
       ctimesave=-9999L+lonarr(ntrajmax,nfiles)
       ctimes=-9999L+lonarr(nfiles)
    endif
    ctimes(n)=ictime
    if ntraj gt 0L then begin
       xsave(0:ntraj-1L,n)=xn
       ysave(0:ntraj-1L,n)=yn
       zsave(0:ntraj-1L,n)=thn
       x0save(0:ntraj-1L,n)=x0n
       y0save(0:ntraj-1L,n)=y0n
       z0save(0:ntraj-1L,n)=th0n
       agesave(0:ntraj-1L,n)=agen/24.		; days
       pvsave(0:ntraj-1L,n)=pvn
       o3save(0:ntraj-1L,n)=o3n
       tmpsave(0:ntraj-1L,n)=tmpn
       mintsave(0:ntraj-1L,n)=xmint
       marksave(0:ntraj-1L,n)=markn
       frdaysave(0:ntraj-1L,n)=frday*dtout    ; total sunlit hours
       stimesave(0:ntraj-1L,n)=istime
       ctimesave(0:ntraj-1L,n)=ictime
    endif
jumpday:
endfor          ; loop over days
;
; truncate arrays to fit day with most trajectories
;
xsave=xsave(0:ntraj-1L,*)
ysave=ysave(0:ntraj-1L,*)
zsave=zsave(0:ntraj-1L,*)
x0save=x0save(0:ntraj-1L,*)
y0save=y0save(0:ntraj-1L,*)
z0save=z0save(0:ntraj-1L,*)
agesave=agesave(0:ntraj-1L,*)
pvsave=pvsave(0:ntraj-1L,*)
o3save=o3save(0:ntraj-1L,*)
tmpsave=tmpsave(0:ntraj-1L,*)
mintsave=mintsave(0:ntraj-1L,*)
marksave=marksave(0:ntraj-1L,*)
frdaysave=frdaysave(0:ntraj-1L,*)
stimesave=stimesave(0:ntraj-1L,*)
ctimesave=ctimesave(0:ntraj-1L,*)
;
; loop over output times and extract average sunlit hours in the vortex 
; over the past 10 days
;
tsun=fltarr(nfiles,nth)
ntsun=lonarr(nfiles,nth)
for itraj=0L,ntraj-1L do begin
    xday=reform(xsave(itraj,*))
    yday=reform(ysave(itraj,*))
    zday=reform(zsave(itraj,*))
    z0day=reform(z0save(itraj,*))
    ageday=reform(agesave(itraj,*))
    tmpday=reform(tmpsave(itraj,*))
    mintday=reform(mintsave(itraj,*))
    markday=reform(marksave(itraj,*))
    frdayday=reform(frdaysave(itraj,*))
    stimeday=reform(stimesave(itraj,*))
    ctimeday=reform(ctimesave(itraj,*))
;
; extract only "good" part of this trajectory
;
    index=where(z0day ne -9999.)
    xday=xday(index)
    yday=yday(index)
    zday=zday(index)
    z0day=z0day(index)
    ageday=ageday(index)
    tmpday=tmpsave(index)
    mintday=mintday(index)
    markday=markday(index)
    frdayday=frdayday(index)
    stimeday=stimeday(index)
    ctimeday=ctimeday(index)
;
; check
;
;if itraj eq 2000L then begin
;   map_set,0,0,0,/contin,/grid,/noeras
;   oplot,xday,yday,psym=-2
;   stop
;endif
; 
; DETERMINE TSUN ARRAY INDICES (itime,klev)
; what is the initial altitude of this trajectory?
; what timestep was this trajectory initialized? (use max(ctime) for back trajectory)
;
klev=where(thlev eq max(z0day))
itime=where(ctimes eq max(ctimeday))
;print,itraj,min(ctimeday),max(ctimeday),itime(0)

;
; sum up all daylight ours spent inside the vortex within the past 10 days
;
index=where(markday gt 0. and abs(ageday) le 10.,nt)
if index(0) ne -1L then begin
   tsun(itime(0),klev(0))=tsun(itime(0),klev(0))+total(frdayday(index))
   ntsun(itime(0),klev(0))=ntsun(itime(0),klev(0))+1L
endif

endfor
;
; take average at each timestep
;
index=where(ntsun ge 1L)
if index(0) ne -1L then tsun(index)=tsun(index)/float(ntsun(index))
;
; plot
;
stime0=strcompress(min(ctimes),/remove_all)
stime1=strcompress(max(ctimes),/remove_all)
if setplot eq 'ps' then begin
   lc=0
   set_plot,'ps'
   xsize=nxdim/100.
   ysize=nydim/100.
   !psym=0
   !p.font=0
   device,font_size=9
   device,/landscape,bits=8,filename='ace_airmass_zt_sunhours_'+stime0+'-'+stime1+'.ps'
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
level=12.*findgen(11)
nlvls=n_elements(level)
col1=5+indgen(nlvls)*mcolor/nlvls
tsun=smooth(tsun,3,/edge_truncate)
;
; reverse date and tsun arrays to plot forward in time
;
ctimes=reverse(ctimes)
rtsun=0.*tsun
for ith=0L,nth-1L do rtsun(*,ith)=reverse(tsun(*,ith))

sctimes=strcompress(ctimes,/remove_all)
schours=strmid(sctimes,8,2)
xindex=where(strmid(sctimes,6,2) eq '15',nxticks)
xlabels=strmid(sctimes(xindex),0,8)
contour,rtsun,findgen(nfiles),thlev,levels=level,/noeras,/cell_fill,c_color=col1,$
    min_value=0.,c_charsize=2,color=lc,yrange=[min(thlev),700.],$
    title='ACE Avg Vortex Sunlit Hours in Previous 10 days',charsize=2,$
    xtickv=xindex,xticks=nxticks-1L,xtickname=' '+strarr(nxticks)
contour,rtsun,findgen(nfiles),thlev,/overplot,levels=level,/noeras,color=0
for ii=0L,nxticks-1L do begin
    plots,xindex(ii),400.
    plots,xindex(ii),380.,/continue,/data
    xyouts,xindex(ii),320.,xlabels(ii),orientation=90.,/data,charsize=1.5
endfor
imin=min(level)
imax=max(level)
ymnb=yorig(0) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
      xtitle='(Hours)',charsize=2
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
   spawn,'convert -trim ace_airmass_zt_sunhours_'+stime0+'-'+stime1+'.ps -rotate -90 '+$
         ' ace_airmass_zt_sunhours_'+stime0+'-'+stime1+'.jpg'
endif
if setplot eq 'x' then stop
jumprun:
endfor		; loop over airmass runs
end
