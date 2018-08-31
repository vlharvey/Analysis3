;
; make scatterplot
;
@rd_ukmo_nc3

SETPLOT='x'
read,'setplot=',setplot

loadct,38
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
nxdim=750
nydim=750
xorig=[0.25]
yorig=[0.35]
xlen=0.5
ylen=0.5
cbaryoff=0.13
cbarydel=0.02
!NOERAS=-1

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
;print,thlev
;thval=0
;read,'enter theta level ',thval
thval=400.
k=where(thval eq thlev)
k=k(0)

if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
nlvls=10
col1=5+indgen(nlvls)*mcolor/nlvls
ins='ATMOS'
dirw='/usr72/users/ukmo/Datfiles/ukmo_'
dirh='~harvey/Airmass/ATMOS/'
ifile=[$
'ukmo_atmos_94110412-94103012_3d.airm',$
'ukmo_atmos_94110512-94103112_3d.airm',$
'ukmo_atmos_94110612-94110112_3d.airm',$
'ukmo_atmos_94110712-94110212_3d.airm',$
'ukmo_atmos_94110812-94110312_3d.airm',$
'ukmo_atmos_94110912-94110412_3d.airm',$
'ukmo_atmos_94111012-94110512_3d.airm',$
'ukmo_atmos_94111112-94110612_3d.airm',$
'ukmo_atmos_94111212-94110712_3d.airm']
kfile=n_elements(ifile)

ntrajold=0
pvs='n'
ps='y'
msfs='n'
us='n'
vs='n'
qs='n'
qdfs='n'
marks='n'
speed='n'
pno2='n'
paer='n'
po3='y'
!noeras=1
charexp='                                '
theta=0.
ukmo=' '
nmc=' '
ecmwf=' '
restart=' '
rfile=' '
istime=0L
ictime=0L
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
char1='     '
char2='     '
char3='     '
char4='     '
char5='     '
char6='     '
char7='     '
char8='     '
char9='     '
char10='     '
char11='     '
char12='     '
char13='     '
char14='     '
char15='     '
char16='     '
char17='     '
char18='     '
char19='     '
char20='     '
char21='     '
char22='     '
char23='     '
char24='     '
char25='     '

for kk=0,kfile-1 do begin
close,10
openr,10,dirh+ifile(kk),/f77
readu,10,charexp                         
readu,10,ukmo                            
readu,10,nmc                             
readu,10,ecmwf                           
readu,10,restart
readu,10,rfile
readu,10,istime
readu,10,dir                             
readu,10,nfiles
for n=1,nfiles do readu,10,wfile
readu,10,dir                             
readu,10,nfiles
for n=1,nfiles do readu,10,wfile
readu,10,ictime                           
readu,10,dtflow                          
readu,10,dt                              
readu,10,igw                             
readu,10,dtout
readu,10,ofile
for n=0,nfiles-1 do begin
    if n eq nfiles-1 then goto,jumprun
;
; read current time in airmass run and number of trajectories
;
    READU,10,istime,ictime,time,ntraj
    print,istime,ictime,time,ntraj
;
; calculate trajectory time in Julian hours
;
    leapyr=(fix(strmid(string(ictime),4,2)) mod 4)
    if leapyr eq 0 then leapdy=1
    if leapyr ne 0 then leapdy=0
    if fix(strmid(string(ictime),6,2)) le 2 then leapdy=0
    mdays=0
    for i=0,fix(strmid(string(ictime),6,2))-2 do begin
    mdays=mdays+mday(i)
    endfor
    jday=mdays+fix(strmid(string(ictime),8,2))+leapdy
    jtime=24.*jday+fix(strmid(string(ictime),10,2))

    xn=fltarr(ntraj)
    yn=fltarr(ntraj)
    thn=fltarr(ntraj)
    agen=fltarr(ntraj)
    x0n=fltarr(ntraj)
    y0n=fltarr(ntraj)
    th0n=fltarr(ntraj)
    t0n=fltarr(ntraj)
    z0n=fltarr(ntraj)
    p0n=fltarr(ntraj)

    ch4n=fltarr(ntraj)
    clono2n=fltarr(ntraj)
    h2on=fltarr(ntraj)
    hcln=fltarr(ntraj)
    hno3n=fltarr(ntraj)
    hno4n=fltarr(ntraj)
    n2on=fltarr(ntraj)
    n2o5n=fltarr(ntraj)
    non=fltarr(ntraj)
    no2n=fltarr(ntraj)
    o3n=fltarr(ntraj)
    sf6n=fltarr(ntraj)
    ch4n_err=fltarr(ntraj)
    clono2n_err=fltarr(ntraj)
    h2on_err=fltarr(ntraj)
    hcln_err=fltarr(ntraj)
    hno3n_err=fltarr(ntraj)
    hno4n_err=fltarr(ntraj)
    n2on_err=fltarr(ntraj)
    n2o5n_err=fltarr(ntraj)
    non_err=fltarr(ntraj)
    no2n_err=fltarr(ntraj)
    o3n_err=fltarr(ntraj)
    sf6n_err=fltarr(ntraj)

    pvn=fltarr(ntraj)
    pn=fltarr(ntraj)
    msfn=fltarr(ntraj)
    zn=fltarr(ntraj)
    tmpn=fltarr(ntraj)
    qn=fltarr(ntraj)
    qdfn=fltarr(ntraj)
    markn=fltarr(ntraj)

    readu,10,char1
    READU,10,xn
    readu,10,char2
    READU,10,yn
    readu,10,char3
    READU,10,thn
    readu,10,char4
    READU,10,agen
    readu,10,char5
    READU,10,x0n
    readu,10,char6
    READU,10,y0n
    readu,10,char7
    READU,10,th0n
    readu,10,char7
    READU,10,t0n
    readu,10,char8
    READU,10,z0n
    readu,10,char9
    READU,10,p0n
; chemicals
    readu,10,char10
    READU,10,ch4n
    READU,10,char11
    READU,10,clono2n
    readu,10,char12
    READU,10,h2on
    readu,10,char13
    READU,10,hcln
    readu,10,char14
    READU,10,hno3n
    readu,10,char15
    READU,10,hno4n
    readu,10,char16
    READU,10,n2on
    readu,10,char17
    READU,10,n2o5n
    readu,10,char18
    READU,10,non
    readu,10,char19
    READU,10,no2n
    readu,10,char20
    READU,10,o3n
    readu,10,char21
    READU,10,sf6n
; errors
    readu,10,char22
    READU,10,ch4n_err
    READU,10,char23
    READU,10,clono2n_err
    readu,10,char24
    READU,10,h2on_err
    readu,10,char25
    READU,10,hcln_err
    readu,10,char26
    READU,10,hno3n_err
    readu,10,char27
    READU,10,hno4n_err
    readu,10,char28
    READU,10,n2on_err
    readu,10,char29
    READU,10,n2o5n_err
    readu,10,char30
    READU,10,non_err
    readu,10,char31
    READU,10,no2n_err
    readu,10,char32
    READU,10,o3n_err
    readu,10,char33
    READU,10,sf6n_err
    readu,10,char34

    pvn=fltarr(ntraj)
    pn=fltarr(ntraj)
    msfn=fltarr(ntraj)
    zn=fltarr(ntraj)
    tmpn=fltarr(ntraj)
    qn=fltarr(ntraj)
    qdfn=fltarr(ntraj)
    markn=fltarr(ntraj)

    READU,10,pvn
    readu,10,char35
    READU,10,pn
    readu,10,char36
    READU,10,msfn
    readu,10,char37
    READU,10,zn
    readu,10,char38
    READU,10,tmpn
    readu,10,char39
    READU,10,qn
    readu,10,char40
    READU,10,qdfn
    readu,10,char41
    READU,10,markn


    if n lt nfiles-2 then goto,jumpday

; extract day, month, year information from trajectory file
    idy=strmid(string(ictime),8,2)
    imn=strmid(string(ictime),6,2)
    iyr=strmid(string(ictime),4,2)
    date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4)',$
         month(imn-1),' ',idy,', ',iyr+1900))
;
;  read same day of UKMO isentropic data
;
    iflag=0
    ufile=mon(imn-1)+(string(FORMAT='(I2.2,A1,I2,A4)',idy,'_',iyr,'.nc3'))
;   rd_ukmo_nc3,dirw+ufile,nc,nr,nl,x,y,th,pv2,p2,msf2,$
;            u2,v2,q2,qdf2,markpv2,mark2,vp2,sf2,iflag

; identify correct UKMO theta level from trajectory filename
;   if theta ne th(k) then stop

; isolate UKMO in 2d on proper theta surface (ipv in PVU)
;   pv=transpose(pv2(*,*,k))*1.e4
;   p=transpose(p2(*,*,k))
;   msf=transpose(msf2(*,*,k))
;   sf=transpose(sf2(*,*,k))
;   q=transpose(q2(*,*,k))
;   u=transpose(u2(*,*,k))
;   v=transpose(v2(*,*,k))
;   qdf=transpose(qdf2(*,*,k))
;   mark=transpose(mark2(*,*,k))
;   s=sqrt(u^2+v^2)

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
   device,/landscape,bits=8,filename='atmos_h2o_vs_o3_'+stime+'.ps'
   device,/color
   device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
          xsize=xsize,ysize=ysize
   !p.thick=2.0                   ;Plotted lines twice as thick
   !p.charsize=1.0
endif

    !type=2^2+2^3
if kk eq 0 then begin
    erase
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    !psym=0

; ozone water vapor scatterplot
   plot,findgen(11),findgen(11),/nodata,xrange=[0.,1.],yrange=[0.,20.],$
        xtitle='!6Ozone (ppmv)',$
        ytitle='!6Water Vapor (ppmv)',title='!6ATMOS data',charsize=2
endif
   o3n=o3n*1e6
   h2on=h2on*1e6
   thmax=410.
   thmin=310.
   index=where(o3n gt 0. and h2on gt 0.,ntraj)
   a=findgen(8)*(2*!pi/8.)
   usersym,cos(a),sin(a),/fill
   for itraj=0,ntraj-1 do $
          oplot,[o3n(index(itraj)),o3n(index(itraj))],$
                [h2on(index(itraj)),h2on(index(itraj))],$
                 color=(((thn(index(itraj))-thmin))/(thmax-thmin))*mcolor,$
                 psym=8,symsize=2
;  a=findgen(8)*(2*!pi/8.)
;  usersym,cos(a),sin(a)
;  for itraj=0,ntraj-1 do $
;      if o3n(itraj) gt 0. then $
;         oplot,[xn(itraj),xn(itraj)],[yn(itraj),yn(itraj)],$
;                psym=8,color=lc,symsize=2

   index=where(o3n gt 0.)
   if index(0) ne -1 then print,'max/min  o3=',max(o3n(index)),min(o3n(index))
   index=where(h2on gt 0.)
   if index(0) ne -1 then print,'max/min h2o=',max(h2on(index)),min(h2on(index))
   index=where(no2n gt 0.)
   if index(0) ne -1 then print,'max/min no2=',max(no2n(index)),min(no2n(index))

; color bar for ATMOS data
if kk eq kfile-1 then begin
    imin=thmin
    imax=thmax
    ymnb=yorig(0) -cbaryoff
    ymxb=ymnb  +cbarydel
    set_viewport,xmn,xmx,ymnb,ymxb
    !type=2^2+2^3+2^6
    plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
          xtitle='!6Theta (K)',charsize=2
    ybox=[0,10,10,0,0]
    x1=imin
    dx=(imax-imin)/float(nlvls)
    for j=0,nlvls-1 do begin
        xbox=[x1,x1,x1+dx,x1+dx,x1]
        polyfill,xbox,ybox,color=col1(j)
        x1=x1+dx
    endfor
    theta=fix(theta)
;    xyouts,.35,.83,'!6'+date,/normal,charsize=3
endif

if setplot eq 'x' then begin
   save=assoc(3,bytarr(nxdim,nydim))
   img=bytarr(nxdim,nydim)
   img(0,0)=TVRD(0,0,nxdim,nydim)
   write_gif,'ATMOS_GIFS/atmos_h2o_vs_o3_'+stime+'.gif',img
endif

jumpday:
ntrajold=ntraj
endfor		; loop over days
if setplot eq 'ps' then device, /close
jumprun:
endfor		; loop over airmass runs
end
