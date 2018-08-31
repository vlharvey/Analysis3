;
; SDWACCM version
;
; adapted from rdf_haloe_sage_poam_store_mapped_v3.pro
; RDF and FDF map SOCRATES o3, and PV to a daily grid.
; superimpose coincident occultation locations colored by satellite #.
; store mapped arrays
; note: need time as Julian hour
; VLH 8/16/11
;
; read soundings and not isentropic data!!!!
;
@rd_socrates_o3_soundings_julian
loadct,38
mcolor=!p.color
icmm1=mcolor-1B
icmm2=mcolor-2B
;
; user enters coincidence criteria
;
dxc=250. & dtc=24.	; use 24 hours since I don't have SOCRATES time of day (all at one time)
;print,'Enter coincidence criteria in'
;read,'Distance (km) ',dxc
;read,'Time (hours) ',dtc
sxc=strcompress(string(fix(dxc)),/remove_all)+'KMx'
stc=strcompress(string(fix(dtc)),/remove_all)+'h'
!noeras=1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
setplot='ps'
;read,'enter setplot',setplot
nxdim=750
nydim=750
if setplot ne 'ps' then $
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
!noeras=1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
re=40000./2./!pi
rad=double(180./!pi)
dtr=double(!pi/180.)
;dirw='/aura2/harvey/Traject/'
dirw='/Users/harvey/Traject/'
;dirh='/aura7/aura6/data/NOGAPS_Alpha/Sound_data/socrates_'
dirh='/Volumes/cloud/data/WACCM_data/Sound_data/socrates_'
;udir='/aura7/harvey/NOGAPS_Alpha/Datfiles/NOGAPSA_'
udir='/Volumes/cloud/data/WACCM_data/Datfiles_SD/sdwaccm2012-2014_1_2_2.cam.h1.'
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
ifileb=[$
;'NOGAPS_20080707_30day_rdf_350.traj'$
'SDWACCM_20130716_15day_rdf_360.traj'$
;'SDWACCM_20130711_15day_rdf_360.traj'$
;'SDWACCM_20130713_15day_rdf_360.traj'$
;'SDWACCM_20130715_15day_rdf_360.traj'$
]
ifilef=[$
;'NOGAPS_20080707_30day_fdf_350.traj'$
'SDWACCM_20130716_15day_fdf_360.traj'$
;'SDWACCM_20130711_15day_fdf_360.traj'$
;'SDWACCM_20130713_15day_fdf_360.traj'$
;'SDWACCM_20130715_15day_fdf_360.traj'$
]
nrdf=n_elements(ifilef)
dates=[$
;'20080707'$
'20130716'$
;'20130711'$
;'20130713'$
;'20130715'$
]
datelab=dates
;
; read soundings
;
ifile1=[$ 
;'jun_28_2008',$
;'jun_29_2008',$
;'jun_30_2008',$
;'jul_01_2008',$
;'jul_02_2008',$
;'jul_03_2008',$
;'jul_04_2008',$
;'jul_05_2008',$
;'jul_06_2008',$
;'jul_07_2008',$
;'jul_08_2008',$
;'jul_09_2008',$
;'jul_10_2008',$
;'jul_11_2008',$
;'jul_12_2008',$
;'jul_13_2008',$
;'jul_14_2008',$
;'jul_15_2008',$
;'jul_16_2008',$
;'jul_17_2008',$
;'jul_18_2008',$
;'jul_01_2013',$
;'jul_02_2013',$
;'jul_03_2013',$
;'jul_04_2013',$
;'jul_05_2013',$
;'jul_06_2013',$
;'jul_07_2013',$
;'jul_08_2013',$
'jul_09_2013',$
'jul_10_2013',$
'jul_11_2013',$
'jul_12_2013',$
'jul_13_2013',$
'jul_14_2013',$
'jul_15_2013',$
'jul_16_2013',$
'jul_17_2013',$
'jul_18_2013',$
'jul_19_2013',$
'jul_20_2013',$
'jul_21_2013',$
'jul_22_2013',$
'jul_23_2013'$
;'jul_24_2013',$
;'jul_25_2013',$
;'jul_26_2013',$
;'jul_27_2013',$
;'jul_28_2013',$
;'jul_29_2013',$
;'jul_30_2013',$
;'jul_31_2013'$
]
nday=n_elements(ifile1)
;
; read SDWACCM at SOCRATES
;
norbit=400L
nl=300L
numh=nday*norbit
xhal=9999+fltarr(nday*norbit,nl)
yhal=9999+fltarr(nday*norbit,nl)
thal=9999+fltarr(nday*norbit,nl)
thhal=9999+fltarr(nday*norbit,nl)
o3hal=9999+fltarr(nday*norbit,nl)
h2ohal=9999+fltarr(nday*norbit,nl)
modehal=9999+lonarr(nday*norbit,nl)
rd_socrates_o3_soundings_julian,nday,norbit,$
   ifile1+'_o3.sound',dirh,thal,xhal,yhal,thhal,o3hal,modehal
rd_socrates_o3_soundings_julian,nday,norbit,$
   ifile1+'_h2o.sound',dirh,thal,xhal,yhal,thhal,h2ohal,modehal
;
; eliminate soundings with no data and just take 360 K level (element 0)
;
hindex=where(thhal eq 360.,numh)
tall=thal(hindex)
xall=xhal(hindex)
index=where(xall lt 0.)
if index(0) ne -1 then xall(index)=xall(index)+360.
yall=yhal(hindex)
thall=thhal(hindex)
o3all=o3hal(hindex)
h2oall=h2ohal(hindex)
inst_flag=modehal(hindex)
thal=0 & xhal=0 & yhal=0 & thhal=0 & o3hal=0 & h2ohal=0 & modehal=0
;
; truncate sounding data
;
;for i=0L,nl-1L do begin
;    if max(o3all(*,i)) eq 9999. then begin
;       nl=i
;       goto,jumpout
;    endif
;endfor
;jumpout:
;
;tall=tall(index,0:nl-1)
;xall=xall(index,0:nl-1)
;yall=yall(index,0:nl-1)
;thall=thall(index,0:nl-1)
;o3all=o3all(index,0:nl-1)
;h2oall=h2oall(index,0:nl-1)
;inst_flag=inst_flag(index)
print,'soundings ready'
;
; loop over trajectory runs
;
for irdf=0,nrdf-1 do begin
;
; read trajectories
;
close,12,13,14
openr,12,dirw+ifilef(irdf),/f77
print,'opened '+ifilef(irdf)
openr,13,dirw+ifileb(irdf),/f77
print,'opened '+ifileb(irdf)
nmax=300000l
nrmax=181
ncmax=361
x=fltarr(ncmax)
y=fltarr(nrmax)
charexp='                                   '
nthp=0L
theta=0.
ukmo=' '
nmc=' '
ecmwf=' '
restart=' '
rfile='                                     '
nrday=0L
dir='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'
nfile=0L
wfile='sdwaccm2012-2014_1_2_2.cam.h1.20130715_utls.nc3'
istime=0l
ictime=0l
dtflow=0.
dt=0.
igw=' '
stream=' '
nstrm=0L
ds1=0.
strm0=0.
dstrm=0.
pviso=' '
npviso=0L
ds2=0.
pv0=0.
dpv=0.
tiso=' '
ntiso=0L
ds3=0.
temp0=0.
dtemp=0.
space=' '
dxs=0.
dys=0.
range_fill=' '
stlat=0.
stlon=0.
er2_range=' '
nrings=0L
npts=0L
dial=' '
dird='/aura3/data/DIAL_data/Datfiles/'
ndial=0l
dfile='dial_o3_20000315.diag'
er2=' '
nleg=0L
xleg0=0.
yleg0=0.
xleg1=0.
yleg1=0.
dc8=' '
dirdc8='/aura3/data/DC8_data/Datfiles/'
ndc8=0L
dc8file='dial_o3_20000309.diag.dc8'
dtout=0.
ofile='SDWACCM_20130715_11day_fdf_360-420.traj'
nr=0L
nc=0L
time=0.
ntraj=0l
char1='xtraj'
char2='ytraj'
char3='ztraj'
char4='pvtrj'
char5='ptraj'
char6='fdtraj'
char7='fntraj'
char8='qtraj'
char9='qdftraj'
char10='xmsftraj'
char11='mark_traj'
char12='t0_traj'
char13='age_traj'
char14='mint_traj'
char15='no_traj'
char16='co_traj'
char17='e_traj'
;
;  read input parameters for this experiment
;
readu,12,charexp
print,charexp
readu,12,nthp
print,nthp
thlevp=fltarr(nthp)
readu,12,thlevp
print,thlevp
readu,12,ukmo
readu,12,nmc
readu,12,ecmwf
readu,12,restart
readu,12,rfile
readu,12,nrday
readu,12,dir
readu,12,nfile
wfiles=strarr(nfile)
for n=0,nfile-1 do begin
    readu,12,wfile
    wfiles(n)=strtrim(wfile)
    print,wfile
endfor
readu,12,istime
readu,12,dtflow
readu,12,dt
readu,12,igw
readu,12,stream
readu,12,nstrm
readu,12,ds1
readu,12,strm0
readu,12,dstrm
readu,12,pviso
readu,12,npviso
readu,12,ds2
readu,12,pv0
readu,12,dpv
readu,12,tiso
readu,12,ntiso
readu,12,ds3
readu,12,temp0
readu,12,dtemp
readu,12,space
readu,12,dxs
readu,12,dys
readu,12,range_fill
readu,12,stlat
readu,12,stlon
readu,12,er2_range
readu,12,nrings
readu,12,npts
readu,12,dial
readu,12,dird
readu,12,ndial
for n=1,ndial do begin
    readu,12,dfile
    print,n,dfile
endfor
readu,12,er2
readu,12,nleg
for n=1,nleg do begin
    readu,12,xleg0
    readu,12,yleg0
    readu,12,xleg1
    readu,12,yleg1
endfor
readu,12,dc8
readu,12,dirdc8
readu,12,ndc8
for n=1,ndc8 do begin
    readu,12,dc8file
    print,n,dc8file
endfor
readu,12,dtout
readu,12,ofile
print,ofile
print,'    Forward header read ok'
nout=long(dtflow/dtout)
nfiles=nout*(nfile-2)
readu,13,charexp
readu,13,nthp
thlevp=fltarr(nthp)
readu,13,thlevp
readu,13,ukmo
readu,13,nmc
readu,13,ecmwf
readu,13,restart
readu,13,rfile
readu,13,nrday
readu,13,dir
readu,13,nfile
wfiles=strarr(nfile)
for n=0,nfile-1 do begin
    readu,13,wfile
    wfiles(n)=strtrim(wfile)
endfor
readu,13,istime
readu,13,dtflow
readu,13,dt
readu,13,igw
readu,13,stream
readu,13,nstrm
readu,13,ds1
readu,13,strm0
readu,13,dstrm
readu,13,pviso
readu,13,npviso
readu,13,ds2
readu,13,pv0
readu,13,dpv
readu,13,tiso
readu,13,ntiso
readu,13,ds3
readu,13,temp0
readu,13,dtemp
readu,13,space
readu,13,dxs
readu,13,dys
readu,13,range_fill
readu,13,stlat
readu,13,stlon
readu,13,er2_range
readu,13,nrings
readu,13,npts
readu,13,dial
readu,13,dird
readu,13,ndial
for n=1,ndial do begin
    readu,13,dfile
;   print , n, dfile
endfor
readu,13,er2
readu,13,nleg
for n=1,nleg do begin
    readu,13,xleg0
    readu,13,yleg0
    readu,13,xleg1
    readu,13,yleg1
endfor
readu,13,dc8
readu,13,dirdc8
readu,13,ndc8
for n=1,ndc8 do begin
    readu,13,dc8file
    print , n,dc8file
endfor
readu,13,dtout
readu,13,ofile
;print,ofile
print,'    Backward header read ok'
for n=0,nfiles-1 do begin
    timef=0.
    READU,12,istime,ictime,timef,ntraj
    print,istime,ictime,timef,ntraj
    ihr=fix(strmid(string(ictime),10,2))
    idy=fix(strmid(string(ictime),8,2))
    imn=fix(strmid(string(ictime),6,2))
    iyr=fix(strmid(string(ictime),2,4))
;
; convert trajectory time and sounding time to Julian hours
;
    leapyr=iyr mod 4
    if leapyr eq 0 then leapdy=1
    if leapyr ne 0 then leapdy=0
    if imn le 2 then leapdy=0
    mdays=0
    for i=0,imn-2 do mdays=mdays+mday(i)
    jday=mdays+idy+leapdy
    jhour=24.*jday+ihr
    timef=jhour
    print,ictime,timef,min(tall),max(tall)
    xnf=fltarr(ntraj)
    ynf=fltarr(ntraj)
    znf=fltarr(ntraj)
    pvnf=fltarr(ntraj)
    pnf=fltarr(ntraj)
    frdayf=fltarr(ntraj)
    frnghtf=fltarr(ntraj)
    qnf=fltarr(ntraj)
    qdfnf=fltarr(ntraj)
    xmsfnf=fltarr(ntraj)
    t0nf=fltarr(ntraj)
    agenf=fltarr(ntraj)
    xmintnf=fltarr(ntraj)
    xmrknf=fltarr(ntraj)
    o3nf=fltarr(ntraj)
    h2onf=fltarr(ntraj)
    xenf=fltarr(ntraj)
    readu,12,char1
    READU,12,xnf
    readu,12,char2
    READU,12,ynf
    readu,12,char3
    READU,12,znf
    readu,12,char4
    READU,12,pvnf
    readu,12,char5
    READU,12,pnf
    readu,12,char6
    READU,12,frdayf
    readu,12,char7
    READU,12,frnghtf
    readu,12,char8
    READU,12,qnf
    readu,12,char9
    READU,12,qdfnf
    readu,12,char10
    READU,12,xmsfnf	; SF
    readu,12,char11
    READU,12,xmrknf
    readu,12,char12
    READU,12,t0nf
    readu,12,char13
    READU,12,agenf
print,'age ',min(agenf),max(agenf)
    readu,12,char14
    READU,12,xmintnf
    readu,12,char15
    readu,12,h2onf	; old NO variable is water
    readu,12,char16
    readu,12,o3nf	; old CO variable is ozone
    readu,12,char17
    readu,12,xenf	; old electron variable not saved
;
; assign water to water array
;
    h2onf=xmrknf

    index=where(xnf lt 0.)
    if index(0) ne -1 then xnf(index)=xnf(index)+360.

    if ictime eq istime then begin
       x0f=xnf
       y0f=ynf
       z0f=znf
       pv0f=pvnf
       xmrk0f=xmrknf
       xmapf=9999.+0.*pvnf
       ymapf=9999.+0.*pvnf
       thmapf=9999.+0.*pvnf
       o3mapf=9999.+0.*pvnf
       xo3mapf=9999.+0.*pvnf
       yo3mapf=9999.+0.*pvnf
       h2omapf=9999.+0.*pvnf
       xh2omapf=9999.+0.*pvnf
       yh2omapf=9999.+0.*pvnf
       instmapf=9999.+0.*pvnf
       xinstmapf=9999.+0.*pvnf
       yinstmapf=9999.+0.*pvnf
       agemapf=9999.+0.*pvnf
       qdfmapf=9999.+0.*pvnf
       fdmapf=9999.+0.*pvnf
       xmintmapf=9999.+0.*pvnf
       pvmapf=9999.+0.*pvnf
       xmrkmapf=9999.+0.*pvnf
       fdavef=frdayf
       qdfavef=qdfnf
    endif

    if n gt 0 then begin
       index=where(frdayf lt 9999.)
       fdavef(index)=((n-1)*fdavef(index)+frdayf(index))/n
       index=where(qdfnf lt 9999.)
       qdfavef(index)=((n-1)*qdfavef(index)+qdfnf(index))/n
    endif
;
; map sounding data from forward trajectory location
;
    sindex=where(abs(tall-timef) le dtc,np)
    if sindex(0) ne -1 then begin
       xdayf=xall(sindex)
       ydayf=yall(sindex)
       thdayf=thall(sindex)	; all data is at 360 K
       o3dayf=o3all(sindex)
       h2odayf=h2oall(sindex)
       instdayf=inst_flag(sindex)
       for m=0,np-1 do begin
;
; determine if each extracted sounding is coincident in space
; with any uninitialized trajectories.
;
           dx=re*abs(xnf-xdayf(m))*dtr*cos(ydayf(m)*dtr)
           dy=re*abs(ynf-ydayf(m))*dtr
           dist=sqrt(dx*dx+dy*dy)
           index=where(dist le dxc and abs(agenf) ge 6. and $
                       h2omapf eq 9999.,nf)
           if index(0) ne -1 then begin
              xmapf(index)=xnf(index)
              ymapf(index)=ynf(index)
              thmapf(index)=znf(index)
;             instmapf(index)=instdayf(m)
              agemapf(index)=abs(agenf(index))
              qdfmapf(index)=qdfavef(index)
              pvmapf(index)=pvnf(index)
              fdmapf(index)=fdavef(index)
              xmintmapf(index)=xmintnf(index)
              xmrkmapf(index)=xmrknf(index)
;
; map o3dayf and h2odayf soundings
;
             if o3dayf(m) lt 100. and o3dayf(m) gt 0. then begin
                o3mapf(index)=o3dayf(m)
                xo3mapf(index)=xdayf(m)
                yo3mapf(index)=ydayf(m)
             endif
             if h2odayf(m) lt 9999. and h2odayf(m) gt 0. then begin
                h2omapf(index)=h2odayf(m)
                xh2omapf(index)=xdayf(m)
                yh2omapf(index)=ydayf(m)
             endif

             instmapf(index)=instdayf(m)
             xinstmapf(index)=xdayf(m)
             yinstmapf(index)=xdayf(m)
           endif        ; if there are soundings coincident in time
       endfor           ; loop over soundings coincident in time
   endif
   index=where(h2omapf ne 9999.,nfilled)
   if index(0) ne -1 then print,float(nfilled)*100./ntraj,' % forward filled'
   
   timeb=0.
   READU,13,istime,ictime,timeb,ntraj
   print,'backward ',istime,ictime,timeb,ntraj
   ihr=fix(strmid(string(ictime),10,2))
   idy=fix(strmid(string(ictime),8,2))
   imn=fix(strmid(string(ictime),6,2))
   iyr=fix(strmid(string(ictime),2,4))
;
; convert trajectory time and sounding time to Julian hours
;
   leapyr=iyr mod 4
   if leapyr eq 0 then leapdy=1
   if leapyr ne 0 then leapdy=0
   if imn le 2 then leapdy=0
   mdays=0
   for i=0,imn-2 do mdays=mdays+mday(i)
   jday=mdays+idy+leapdy
   jhour=24.*jday+ihr
   timeb=jhour
   print,ictime,timeb,min(tall),max(tall)
   xnb=fltarr(ntraj)
   ynb=fltarr(ntraj)
   znb=fltarr(ntraj)
   pvnb=fltarr(ntraj)
   pnb=fltarr(ntraj)
   frdayb=fltarr(ntraj)
   frnghtb=fltarr(ntraj)
   qnb=fltarr(ntraj)
   qdfnb=fltarr(ntraj)
   xmsfnb=fltarr(ntraj)
   xmrknb=fltarr(ntraj)
   t0nb=fltarr(ntraj)
   agenb=fltarr(ntraj)
   xmintnb=fltarr(ntraj)
   o3nb=fltarr(ntraj)
   h2onb=fltarr(ntraj)
   xenb=fltarr(ntraj)
   readu,13,char1
   READU,13,xnb
   readu,13,char2
   READU,13,ynb
   readu,13,char3
   READU,13,znb
   readu,13,char4
   READU,13,pvnb
   readu,13,char5
   READU,13,pnb
   readu,13,char6
   READU,13,frdayb
   readu,13,char7
   READU,13,frnghtb
   readu,13,char8
   READU,13,qnb
   readu,13,char9
   READU,13,qdfnb
   readu,13,char10
   READU,13,xmsfnb
   readu,13,char11
   READU,13,xmrknb
   readu,13,char12
   READU,13,t0nb
   readu,13,char13
   READU,13,agenb
   readu,13,char14
   READU,13,xmintnb
   readu,13,char15
   readu,13,h2onb
   readu,13,char16
   readu,13,o3nb
   readu,13,char17
   readu,13,xenb
;
; assign water to water array
;
    h2onb=xmrknb

   if ictime eq istime then begin
      x0b=xnb
      y0b=ynb
      z0b=znb
      pv0b=pvnb
      xmrk0b=xmrknb
      xmapb=9999.+0.*pvnb
      ymapb=9999.+0.*pvnb
      thmapb=9999.+0.*pvnb
      o3mapb=9999.+0.*pvnb
      xo3mapb=9999.+0.*pvnb
      yo3mapb=9999.+0.*pvnb
      h2omapb=9999.+0.*pvnb
      xh2omapb=9999.+0.*pvnb
      yh2omapb=9999.+0.*pvnb
      instmapb=9999.+0.*pvnb
      xinstmapb=9999.+0.*pvnb
      yinstmapb=9999.+0.*pvnb
      agemapb=9999.+0.*pvnb
      qdfmapb=9999.+0.*pvnb
      fdmapb=9999.+0.*pvnb
      xmintmapb=9999.+0.*pvnb
      pvmapb=9999.+0.*pvnb
      xmrkmapb=9999.+0.*pvnb
      fdaveb=frdayb
      qdfaveb=qdfnb
   endif
   if n gt 0 then begin
      index=where(frdayb lt 9999.)
      fdaveb(index)=((n-1)*fdaveb(index)+frdayb(index))/n
      index=where(qdfnb lt 9999.)
      qdfaveb(index)=((n-1)*qdfaveb(index)+qdfnb(index))/n
   endif
;
; extract soundings within 6 hours of trajectory time
;
   sindex=where(abs(tall-timeb) le dtc,np)
   if sindex(0) ne -1 then begin
      xdayb=xall(sindex)
      ydayb=yall(sindex)
      thdayb=thall(sindex)
      o3dayb=o3all(sindex)
      h2odayb=h2oall(sindex)
      instdayb=inst_flag(sindex)
      for m=0,np-1 do begin
;
; determine if each extracted sounding is coincident in space
; with any uninitialized trajectories.
;
          dx=re*abs(xnb-xdayb(m))*dtr*cos(ydayb(m)*dtr)
          dy=re*abs(ynb-ydayb(m))*dtr
          dist=sqrt(dx*dx+dy*dy)
          index=where(dist le dxc and abs(agenb) ge 6. and $
                      h2omapb eq 9999.,nf)
          if index(0) ne -1 then begin
             xmapb(index)=xnb(index)
             ymapb(index)=ynb(index)
             thmapb(index)=znb(index)
;            instmapb(index)=instdayb(m)
             agemapb(index)=abs(agenb(index))
             qdfmapb(index)=qdfaveb(index)
             pvmapb(index)=pvnb(index)
             fdmapb(index)=fdaveb(index)
             xmintmapb(index)=xmintnb(index)
             xmrkmapb(index)=xmrknb(index)
;
; map o3dayb and h2odayb soundings
;
             if o3dayb(m) lt 100. and o3dayb(m) gt 0. then begin
                o3mapb(index)=o3dayb(m)
                xo3mapb(index)=xdayb(m)
                yo3mapb(index)=ydayb(m)
             endif
             if h2odayb(m) lt 9999. and h2odayb(m) gt 0. then begin
                h2omapb(index)=h2odayb(m)
                xh2omapb(index)=xdayb(m)
                yh2omapb(index)=ydayb(m)
             endif

             instmapb(index)=instdayb(m)
             xinstmapb(index)=xdayb(m)
             yinstmapb(index)=xdayb(m)

          endif        ; if there are soundings coincident in time
      endfor           ; loop over soundings coincident in time
   endif
   index=where(h2omapb ne 9999.,nfilled)
   if index(0) ne -1 then print,float(nfilled)*100./ntraj,' % backward filled'
endfor	; loop over output times
;
; build mapped arrays:
; blend forward and backward mapped ozone such that
; if both forward and backward maps contain information
; ozone of the youngest trajectory is assigned
;
imax=long(360./dxs)
jmax=long(180./dys+1)
xnd=360.*findgen(imax)/(imax)
ynd=-90.+180.*findgen(jmax)/(jmax-1)
x0map=9999.+fltarr(imax,jmax,nthp)
y0map=9999.+fltarr(imax,jmax,nthp)
th0map=9999.+fltarr(imax,jmax,nthp)
xmrk0map=9999.+fltarr(imax,jmax,nthp)
pv0map=9999.+fltarr(imax,jmax,nthp)
xmap=9999.+fltarr(imax,jmax,nthp)
ymap=9999.+fltarr(imax,jmax,nthp)
thmap=9999.+fltarr(imax,jmax,nthp)
o3map=9999.+fltarr(imax,jmax,nthp)
xo3map=9999.+fltarr(imax,jmax,nthp)
yo3map=9999.+fltarr(imax,jmax,nthp)
h2omap=9999.+fltarr(imax,jmax,nthp)
xh2omap=9999.+fltarr(imax,jmax,nthp)
yh2omap=9999.+fltarr(imax,jmax,nthp)
agemap=9999.+fltarr(imax,jmax,nthp)
instmap=9999.+fltarr(imax,jmax,nthp)
xinstmap=9999.+fltarr(imax,jmax,nthp)
yinstmap=9999.+fltarr(imax,jmax,nthp)
qdfmap=9999.+fltarr(imax,jmax,nthp)
fdmap=9999.+fltarr(imax,jmax,nthp)
xmintmap=9999.+fltarr(imax,jmax,nthp)
xmrkmap=9999.+fltarr(imax,jmax,nthp)
pvmap=9999.+fltarr(imax,jmax,nthp)
ncount=0L
for i=0,imax-1 do begin
for j=0,jmax-1 do begin
for k=0,nthp-1 do begin
    if agemapf(ncount) lt agemapb(ncount) and $
       h2omapf(ncount) ne 9999. then begin

       x0map(i,j,k)=x0f(ncount)
       y0map(i,j,k)=y0f(ncount)
       th0map(i,j,k)=z0f(ncount)
       xmrk0map(i,j,k)=xmrk0f(ncount)
       pv0map(i,j,k)=pv0f(ncount)
       xmap(i,j,k)=xmapf(ncount)
       ymap(i,j,k)=ymapf(ncount)
       thmap(i,j,k)=thmapf(ncount)
       agemap(i,j,k)=agemapf(ncount)
       pvmap(i,j,k)=pvmapf(ncount)
       instmap(i,j,k)=instmapf(ncount)
       xinstmap(i,j,k)=xinstmapf(ncount)
       yinstmap(i,j,k)=yinstmapf(ncount)
       qdfmap(i,j,k)=qdfmapf(ncount)
       fdmap(i,j,k)=fdmapf(ncount)
       xmintmap(i,j,k)=xmintmapf(ncount)
       xmrkmap(i,j,k)=xmrkmapf(ncount)
       o3map(i,j,k)=o3mapf(ncount)
       xo3map(i,j,k)=xo3mapf(ncount)
       yo3map(i,j,k)=yo3mapf(ncount)
       h2omap(i,j,k)=h2omapf(ncount)
       xh2omap(i,j,k)=xh2omapf(ncount)
       yh2omap(i,j,k)=yh2omapf(ncount)
    endif
    if agemapb(ncount) lt agemapf(ncount) and $
       h2omapb(ncount) ne 9999. then begin

       x0map(i,j,k)=x0b(ncount)
       y0map(i,j,k)=y0b(ncount)
       th0map(i,j,k)=z0b(ncount)
       xmrk0map(i,j,k)=xmrk0b(ncount)
       pv0map(i,j,k)=pv0b(ncount)
       xmap(i,j,k)=xmapb(ncount)
       ymap(i,j,k)=ymapb(ncount)
       thmap(i,j,k)=thmapb(ncount)
       agemap(i,j,k)=-1.*agemapb(ncount)
       pvmap(i,j,k)=pvmapb(ncount)
       instmap(i,j,k)=instmapb(ncount)
       xinstmap(i,j,k)=xinstmapb(ncount)
       yinstmap(i,j,k)=yinstmapb(ncount)
       qdfmap(i,j,k)=qdfmapb(ncount)
       fdmap(i,j,k)=fdmapb(ncount)
       xmintmap(i,j,k)=xmintmapb(ncount)
       xmrkmap(i,j,k)=xmrkmapb(ncount)
       o3map(i,j,k)=o3mapb(ncount)
       xo3map(i,j,k)=xo3mapb(ncount)
       yo3map(i,j,k)=yo3mapb(ncount)
       h2omap(i,j,k)=h2omapb(ncount)
       xh2omap(i,j,k)=xh2omapb(ncount)
       yh2omap(i,j,k)=yh2omapb(ncount)
    endif
    ncount=ncount+1L
endfor
endfor
endfor
index=where(h2omap ne 9999.,nfilled)
if index(0) ne -1 then print,'H2O ',max(h2omap(index)),$
   min(h2omap(index)),float(nfilled)*100./ntraj,' % final filled'
;
; write out mapped fields
;
save,filename='rdf_socrates_'+dates(0)+'_'+sxc+stc+'_2weeks.sav',imax,jmax,nthp,$
     xnd,ynd,thlevp,x0map,y0map,th0map,xmrk0map,pv0map,xmap,ymap,thmap,agemap,$
     pvmap,instmap,xinstmap,yinstmap,qdfmap,fdmap,xmintmap,xmrkmap,o3map,$
     xo3map,yo3map,xh2omap,yh2omap,h2omap

endfor		; loop over trajectory runs
end
