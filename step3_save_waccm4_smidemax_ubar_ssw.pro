;
; read WACCM4 CO2x1SmidEmax_yBWCN set1 (and then set2) data and save yearly files
; NOTE: save Year150 from set1 and save Year151 from set2
;
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set1
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set2
;
loadct,39
device,decompose=0
mcolor=255
icolmax=mcolor
nlvls=301
col1=(findgen(nlvls)/float(nlvls))*mcolor
!p.background=mcolor
setplot='ps'
read,'setplot=',setplot
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
yorig=[0.6,0.15,0.6,0.15]
xorig=[0.15,0.15,0.6,0.6]
xlen=0.3
ylen=0.3
cbaryoff=0.03
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

dir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/'

ivars=[$
'U']
;
; loop over years
;
smonth=['01','02','03','04','05','06','07','08','09','10','11','12']
nmonth=n_elements(smonth)
goto,skip
weastdays_nh=fltarr(nmonth)
weastdays_sh=fltarr(nmonth)
meastdays_nh=fltarr(nmonth)
meastdays_sh=fltarr(nmonth)

for iyr=1,300 do begin		; all years (and take set2 year 151)
    syr=string(format='(i3.3)',iyr)
    ifiles=dir+ivars+'_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'		; filenames for all species each year
    nfiles=n_elements(ifiles)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles(ii)
        restore,ifiles(ii)
        if ivars(ii) eq 'U' then U=U_YEARLY
    endfor
    pindex=where(abs(lev-10.) eq min(abs(lev-10.)))
    u10=reform(u(*,pindex(0),*))

    utemp=fltarr(365)
    for iday=0,364 do utemp(iday) = interpol(reform(u10(*,iday)), LAT, 60)	; Ubar at 60N
    if iyr eq 1L then u60n_all=utemp
    if iyr gt 1L then u60n_all=[u60n_all,utemp]

    for iday=0,364 do utemp(iday) = interpol(reform(u10(*,iday)), LAT, -60)      ; Ubar at 60S
    if iyr eq 1L then u60s_all=utemp
    if iyr gt 1L then u60s_all=[u60s_all,utemp]

    if iyr eq 1L then sdate_tot=sdate_all
    if iyr gt 1L then sdate_tot=[sdate_tot,sdate_all]

;if iyr eq 1 then begin
;   erase
;   !type=2^2+2^3
;   plot,u(80,36,*),ytitle='Ubar at 60N and 10hPa',yrange=[-40,80],color=0,charsize=2,title='FR-WACCM CO2x1SmidEmax_yBWCN_vE'
;endif
;oplot,u(80,36,*),thick=3,color=col1(iyr-1)
endfor		; loop over years
;
; count how many easterly days per month
;
for imon=0L,nmonth-1L do begin
    index=where(strmid(sdate_tot,0,2) eq smonth(imon),nhall)
    index=where(strmid(sdate_tot,0,2) eq smonth(imon) and u60n_all lt 0.,nh)
    if index(0) ne -1L then weastdays_nh(imon)=100.*(float(nh)/float(nhall))
    index=where(strmid(sdate_tot,0,2) eq smonth(imon),shall)
    index=where(strmid(sdate_tot,0,2) eq smonth(imon) and u60s_all lt 0.,sh)
    if index(0) ne -1L then weastdays_sh(imon)=100.*(float(sh)/float(shall))
print,imon,nh,nhall,sh,shall
endfor

!p.linestyle=0
plot,1+findgen(nmonth),weastdays_nh,thick=3,color=0,yrange=[0.,100.],ytitle='% Days with Easterlies'
oplot,1+findgen(nmonth),weastdays_sh,thick=3,color=mcolor*.9
;
; save ubar for entire record plus ssw stats for waccm and merra
;
;
; restore MERRA Ubar
;
restore,'/Users/harvey/Hackett/merra_ubar_60N+S_10hPa.sav
syyyymmdd=strcompress(yyyymmdd,/r)
for imon=0L,nmonth-1L do begin
    index=where(strmid(syyyymmdd,4,2) eq smonth(imon),nhall)
    index=where(strmid(syyyymmdd,4,2) eq smonth(imon) and ubarall_nh lt 0.,nh)
    if index(0) ne -1L then meastdays_nh(imon)=100.*(float(nh)/float(nhall))
    index=where(strmid(syyyymmdd,4,2) eq smonth(imon),shall)
    index=where(strmid(syyyymmdd,4,2) eq smonth(imon) and ubarall_sh lt 0.,sh)
    if index(0) ne -1L then meastdays_sh(imon)=100.*(float(sh)/float(shall))
print,imon,nh,nhall,sh,shall
endfor
;!p.linestyle=5
oplot,1+findgen(nmonth),meastdays_nh,thick=3,color=0
oplot,1+findgen(nmonth),meastdays_sh,thick=3,color=mcolor*.9
;
; save ubar for entire record plus ssw stats for waccm and merra
;
waccm_ubar_nh=u60n_all
waccm_ubar_sh=u60s_all
waccm_dates=sdate_tot
merra_ubar_nh=ubarall_nh
merra_ubar_sh=ubarall_sh
merra_dates=syyyymmdd
save,filename='UBAR_SSW_CO2x1SmidEmax_yBWCN_vE+MERRA.sav',weastdays_nh,weastdays_sh,meastdays_nh,meastdays_sh,smonth,waccm_ubar_nh,waccm_ubar_sh,waccm_dates,merra_ubar_nh,merra_ubar_sh,merra_dates,nmonth

skip:
restore,'UBAR_SSW_CO2x1SmidEmax_yBWCN_vE+MERRA.sav
!p.linestyle=0
erase
!p.charthick=2
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,[1,365],[0,0],yrange=[-40,80],xrange=[1,365],charsize=2,ytitle='Ubar 60N 10hPa',title='NH',color=0
m=long(strmid(waccm_dates,0,2))
d=long(strmid(waccm_dates,2,2))
y=1979+0*findgen(n_elements(WACCM_UBAR_SH))
wdoy=julday(m,d,y)-julday(1,1,y)+1.    ; January 1st is doy=1
m=long(strmid(merra_dates,4,2))
d=long(strmid(merra_dates,6,2))
y=long(strmid(merra_dates,0,4))
mdoy=julday(m,d,y)-julday(1,1,y)+1.    ; January 1st is doy=1
loadct,0
oplot,mdoy,merra_ubar_nh,psym=8,color=100,symsize=1.5
loadct,39
oplot,wdoy,waccm_ubar_nh,psym=1,color=mcolor*.9,symsize=0.5
loadct,0
oplot,mdoy,merra_ubar_nh,psym=3,color=100

xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,[1,365],[0,0],yrange=[-40,120],xrange=[1,365],charsize=2,ytitle='Ubar 60S 10hPa',title='SH',color=0
loadct,0
oplot,mdoy,merra_ubar_sh,psym=8,color=100,symsize=1.5
loadct,39
oplot,wdoy,waccm_ubar_sh,psym=1,color=mcolor*.9,symsize=0.5
loadct,0
oplot,mdoy,merra_ubar_sh,psym=3,color=100
loadct,39

xmn=xorig(2)
xmx=xorig(2)+xlen
ymn=yorig(2)
ymx=yorig(2)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,[1,nmonth],[0,0],yrange=[0,100],xrange=[1,12],charsize=2,ytitle='% Days with Easterlies',title='NH',color=0
xyouts,.45,.52,'SmidEmax',color=250,/normal,charthick=2,charsize=2

loadct,0
xyouts,.45,.49,'MERRA',color=100,/normal,charthick=2,charsize=2
for i=0L,n_elements(smonth)-1L do begin
    ybox=[0,meastdays_nh(i),meastdays_nh(i),0,0]
    x1=i+0.5
if i eq 0 then x1=1
    dx=1
if i eq 0 or i eq n_elements(smonth)-1L then dx=0.5
print,'MERRA ',x1,x1+dx
    xbox=[x1,x1,x1+dx,x1+dx,x1]
    polyfill,xbox,ybox,color=100
    x1=x1+dx
endfor
loadct,39
for i=0L,n_elements(smonth)-1L do begin
    ybox=[0,weastdays_nh(i),weastdays_nh(i),0,0]
    x1=i+0.6
if i eq 0 then x1=1
    dx=0.8
if i eq 0 or i eq n_elements(smonth)-1L then dx=0.4
print,'WACCM ',x1,x1+dx
    xbox=[x1,x1,x1+dx,x1+dx,x1]
    polyfill,xbox,ybox,color=250
    x1=x1+dx
endfor

xmn=xorig(3)
xmx=xorig(3)+xlen
ymn=yorig(3)
ymx=yorig(3)+ylen
set_viewport,xmn,xmx,ymn,ymx
plot,[1,nmonth],[0,0],yrange=[0,100],xrange=[1,12],xtitle='Month',charsize=2,ytitle='% Days with Easterlies',title='SH',color=0

loadct,0
for i=0L,n_elements(smonth)-1L do begin
    ybox=[0,meastdays_sh(i),meastdays_sh(i),0,0]
    x1=i+0.5
if i eq 0 then x1=1
    dx=1
if i eq 0 or i eq n_elements(smonth)-1L then dx=0.5
    xbox=[x1,x1,x1+dx,x1+dx,x1]
    polyfill,xbox,ybox,color=100
    x1=x1+dx
endfor
loadct,39
for i=0L,n_elements(smonth)-1L do begin
    ybox=[0,weastdays_sh(i),weastdays_sh(i),0,0]
    x1=i+0.6
if i eq 0 then x1=1
    dx=0.8
if i eq 0 or i eq n_elements(smonth)-1L then dx=0.4
    xbox=[x1,x1,x1+dx,x1+dx,x1]
    polyfill,xbox,ybox,color=250
    x1=x1+dx
endfor




end
