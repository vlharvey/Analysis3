;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; interpolate SD-WACCM output to SOCRATES measurement locations (apply N year offset depending on case study date and date of model run)
; current case study under consideration is in 2013. Current socrates measurement locations are in 2022, 2023, and 2024
; apply 9 year offset of model data to test satellites in tight configuration.
; apply 10 year offset of model data to test satellites in year 2 spread out configuration
; apply 11 year offset of model data to test satellites in year 3 most spread out configuration
;
; Profile OUTPUT:
;       => number of occultations
;       => time, latitude, longitude, tropopause diagnostics
;	=> number of theta levels
;       => vertical profiles of pressure, temperature, z, PV, O3, H2O on theta (o3 and h2o in ppmv)
;
; VLH 8/15/2011
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@kgmt
@ckday
@kdate
@rd_sdwaccm4_nc3
@interp_poam
@tropopause

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(!pi/8.)
usersym,cos(a),sin(a),/fill
!type=2^2+2^3

;diru='/Volumes/earth/harvey/NOGAPS_Alpha/Datfiles/NOGAPSA_'
diru='/Volumes/cloud/data/WACCM_data/Datfiles_SD/sdwaccm2012-2014_1_2_2.cam.h1.'
re=40000./2./!pi
rtd=double(180./!pi)
dtr=1./rtd
ks=1.931853d-3
ecc=0.081819
gamma45=9.80
;
; restore SOCRATES yearly catalog
;
socyear=[$
'2022',$
'2023',$
'2024']
nyear=n_elements(socyear)
for iyear=0L,nyear-1L do begin
icount=0
;
; copy SOCRATES cat files from /Users/harvey/Proposals/SOCRATES/Satellite_Locations/
;
restore,'/Volumes/cloud/data/WACCM_data/Datfiles_SOSST/cat_socrates.'+socyear(iyear)
cattime=time
if iyear eq 2 then date=date-110000L	; subtract 11 years from second year of SOCRATES measurements (2023) to mesh with WACCM case study in 2013
if iyear eq 1 then date=date-100000L	; subtract 10 years from second year of SOCRATES measurements (2023) to mesh with WACCM case study in 2013
if iyear eq 0 then date=date-90000L	; subtract 9 years from first year of measurements (2022) to mesh with WACCM case study in 2013
sdate=strcompress(string(date),/remove_all)	; want one year forward
time=time/24.0
;
; sort the file in time order
;
sdate=strcompress(date,/r)
year=strmid(sdate,0,4)
mon=strmid(sdate,4,2)
day=strmid(sdate,6,2)
jday=julday(mon,day,year)         
jday0=julday(1+0*mon,1+0*day,year)
doy=jday-jday0+1.
fdoy=doy+time
index=sort(fdoy)
SDATE=sdate(index)	;            LONG      = Array[67032]
DATE=date(index)	;            LONG      = Array[67032]
ID=id(index)		;              LONG      = Array[67032]
LATITUDE=latitude(index)	;        DOUBLE    = Array[67032]
LONGITUDE=longitude(index)	;       DOUBLE    = Array[67032]
TIME=time(index)	;            FLOAT     = Array[67032]
cattime=cattime(index)
;
; extract July minus first and last days (for interpolation)
;
index=where(mon eq '07' and day ne '01' and day ne '31')
SDATE=sdate(index)
DATE=date(index)
ID=id(index)
LATITUDE=latitude(index)
LONGITUDE=longitude(index)
TIME=time(index) 
CATTIME=cattime(index) 

nfiles=n_elements(time)

for n=0L,nfiles-1L do begin
;for n=0L,1000-1L do begin		; testing purposes
    iyr=long(strmid(sdate(n),0,4))
    syear=string(format='(i4)',iyr)
    imn=long(strmid(sdate(n),4,2))
    idy=long(strmid(sdate(n),6,2))
;   date(n)=long(syear+strmid(sdate(n),4,2)+strmid(sdate(n),6,2))
    z = kgmt(imn,idy,iyr,jday)		; calculate Julian day
    slon=longitude(n) & slat=latitude(n)
    if slat lt 0. then ihem=-1
    if slat gt 0. then ihem=1
    print,n,id(n),date(n),time(n),slon,slat
;
; determine 2 bounding dates in (month, day, year) format
; based on fractional year and day and analyses valid at 12Z
;
    if time(n) lt 0.5 then begin
        jday0=jday-1.0
        jday1=jday
        tscale=time(n)+0.5
    endif
    if time(n) ge 0.5 then begin
        jday0=jday
        jday1=jday+1.0
        tscale=time(n)-0.5
    endif
    iyr0=iyr
    iyr1=iyr
    kdate,float(jday0),iyr0,imn0,idy0
    ckday,jday0,iyr0
    kdate,float(jday1),iyr1,imn1,idy1
    ckday,jday1,iyr1
;
; read WACCM data on day 0
;
    newday=0L
    ifile=string(FORMAT='(i4,i2.2,i2.2,a9)',iyr0,imn0,idy0,'_utls.nc3')
    if n eq 0L then begin
       rd_sdwaccm4_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvold,pold,zold,uold,vold,qold,qdfold,markold,sfold,h2oold,n2oold,o3old,iflag
;      rd_nogaps_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvold,pold,msfold,$
;                  uold,vold,qold,qdfold,markold,vpold,sfold,o3old,h2oold,iflag
       if iflag eq 1L then goto,jumpprof
       newday=1L
       print,n,nfiles,ifile
       markold=smooth(markold,3,/edge_truncate)
    endif
    if n gt 0L then begin
    if ifile ne file1 then begin
       rd_sdwaccm4_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvold,pold,zold,uold,vold,qold,qdfold,markold,sfold,h2oold,n2oold,o3old,iflag
;      rd_nogaps_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvold,pold,msfold,$
;                  uold,vold,qold,qdfold,markold,vpold,sfold,o3old,h2oold,iflag
       if iflag eq 1L then goto,jumpprof
       print,n,nfiles,ifile
       newday=1L
       markold=smooth(markold,3,/edge_truncate)
    endif
    endif
    file1=ifile
;
; read WACCM data on day 1
;
    ifile=string(FORMAT='(i4,i2.2,i2.2,a9)',iyr1,imn1,idy1,'_utls.nc3')
    if n eq 0L then begin
       rd_sdwaccm4_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvnew,pnew,znew,unew,vnew,qnew,qdfnew,marknew,sfnew,h2onew,n2onew,o3new,iflag
;      rd_nogaps_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvnew,pnew,msfnew,$
;                  unew,vnew,qnew,qdfnew,marknew,vpnew,sfnew,o3new,h2onew,iflag
       if iflag eq 1L then goto,jumpprof
       print,n,nfiles,ifile
       newday=1L
       marknew=smooth(marknew,3,/edge_truncate)
    endif
    if n gt 0L then begin
    if ifile ne file2 then begin
       rd_sdwaccm4_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvnew,pnew,znew,unew,vnew,qnew,qdfnew,marknew,sfnew,h2onew,n2onew,o3new,iflag
;      rd_nogaps_nc3,diru+ifile,ncw,nrw,nthw,alon,alat,th,pvnew,pnew,msfnew,$
;                  unew,vnew,qnew,qdfnew,marknew,vpnew,sfnew,o3new,h2onew,iflag
       if iflag eq 1L then goto,jumpprof
       print,n,nfiles,ifile
       newday=1L
       marknew=smooth(marknew,3,/edge_truncate)
    endif
    endif
    file2=ifile
;
; check for a bad day of MetO data
;
    if min(pvold) eq 1.00000e+12 or min(pvnew) eq 1.00000e+12 then goto,jumpprof
    if newday eq 1L then begin
;
; perform time interpolation
;
    pgrd=pold+TSCALE*(pnew-pold)
    pvgrd=pvold+TSCALE*(pvnew-pvold)
    ggrd=(zold+TSCALE*(znew-zold))/1000.
    o3grd=o3old+TSCALE*(o3new-o3old)
    h2ogrd=h2oold+TSCALE*(h2onew-h2oold)
;
; o3 and h2o in ppmv
;
    o3grd=1.e6*o3grd
    h2ogrd=1.e6*h2ogrd
;
; where T = theta* (p/po)^R/cp and divide by 1000 for km
    tgrd=0.*pvgrd
    for k=0L,nthw-1L do tgrd(0:nrw-1,0:ncw-1,k)=th(k)*( (pgrd(0:nrw-1,0:ncw-1,k)/1000.)^(.286) )
;
; convert geopotential to geometric height
;
    zgrd=0.*pvgrd
    for k=0L,nthw-1L do begin
        for j=0L,nrw-1L do begin
            sin2=sin( (alat(j)*dtr)^2.0 )
            numerator=1.0+ks*sin2
            denominator=sqrt( 1.0 - (ecc^2.0)*sin2 )
            gammas=gamma45*(numerator/denominator)
            r=6378.137/(1.006803-(0.006706*sin2))
            zgrd(j,*,k)=(r*ggrd(j,*,k))/ ( (gammas/gamma45)*r - ggrd(j,*,k) )
        endfor
    endfor
    endif
;
; logic to handle profiles which are out of the latitude range
;
    if slat lt min(alat) then slat=min(alat)
    if slat gt max(alat) then slat=max(alat)
;
; interpolate WACCM to SOCRATES location
;
    if slon lt alon(0) then slon=slon+360.
    for i=0L,ncw-1L do begin
        ip1=i+1
        if i eq ncw-1L then ip1=0L
        xlon=alon(i)
        xlonp1=alon(ip1)
        if i eq ncw-1L then xlonp1=360.+alon(ip1)
        if slon ge xlon and slon le xlonp1 then begin
           xscale=(slon-xlon)/(xlonp1-xlon)
           goto,jumpx
        endif
    endfor
jumpx:
    for j=0L,nrw-2L do begin
        jp1=j+1
        xlat=alat(j)
        xlatp1=alat(jp1)
        if slat ge xlat and slat le xlatp1 then begin
            yscale=(slat-xlat)/(xlatp1-xlat)
            goto,jumpy
        endif
    endfor
jumpy:
;
; declare profile arrays
;
    if n eq 0L then begin
       p_prof=-99.+fltarr(nfiles,nthw)
       z_prof=-99.+fltarr(nfiles,nthw)
       tp_prof=-99.+fltarr(nfiles,nthw)
       pv_prof=-99.+fltarr(nfiles,nthw)
       o3_prof=-99.+fltarr(nfiles,nthw)
       h2o_prof=-99.+fltarr(nfiles,nthw)
       dyntrop=-99.+fltarr(nfiles)
       zthermtrop=-99.+fltarr(nfiles)
       pthermtrop=-99.+fltarr(nfiles)
       ththermtrop=-99.+fltarr(nfiles)
    endif

    pj1=pvgrd(j,i,*)+xscale*(pvgrd(j,ip1,*)-pvgrd(j,i,*))
    pjp1=pvgrd(jp1,i,*)+xscale*(pvgrd(jp1,ip1,*)-pvgrd(jp1,i,*))
    pv_prof(n,*)=pj1+yscale*(pjp1-pj1)

    pj1=pgrd(j,i,*)+xscale*(pgrd(j,ip1,*)-pgrd(j,i,*))
    pjp1=pgrd(jp1,i,*)+xscale*(pgrd(jp1,ip1,*)-pgrd(jp1,i,*))
    p_prof(n,*)=pj1+yscale*(pjp1-pj1)

    pj1=zgrd(j,i,*)+xscale*(zgrd(j,ip1,*)-zgrd(j,i,*))
    pjp1=zgrd(jp1,i,*)+xscale*(zgrd(jp1,ip1,*)-zgrd(jp1,i,*))
    z_prof(n,*)=pj1+yscale*(pjp1-pj1)

    pj1=tgrd(j,i,*)+xscale*(tgrd(j,ip1,*)-tgrd(j,i,*))
    pjp1=tgrd(jp1,i,*)+xscale*(tgrd(jp1,ip1,*)-tgrd(jp1,i,*))
    tp_prof(n,*)=pj1+yscale*(pjp1-pj1)

    pj1=o3grd(j,i,*)+xscale*(o3grd(j,ip1,*)-o3grd(j,i,*))
    pjp1=o3grd(jp1,i,*)+xscale*(o3grd(jp1,ip1,*)-o3grd(jp1,i,*))
    o3_prof(n,*)=pj1+yscale*(pjp1-pj1)

    pj1=h2ogrd(j,i,*)+xscale*(h2ogrd(j,ip1,*)-h2ogrd(j,i,*))
    pjp1=h2ogrd(jp1,i,*)+xscale*(h2ogrd(jp1,ip1,*)-h2ogrd(jp1,i,*))
    h2o_prof(n,*)=pj1+yscale*(pjp1-pj1)

;print,id(n),date(n),time(n),slon,slat,max(z_prof(n,*)),max(tp_prof(n,*)),max(p_prof(n,*)),max(o3_prof(n,*)),max(h2o_prof(n,*))
;print,reform(z_prof(n,*)),reform(tp_prof(n,*)),reform(p_prof(n,*)),reform(pv_prof(n,*)),reform(o3_prof(n,*)),reform(h2o_prof(n,*))
;
; TROPOPAUSE CALCULATION
; interpolate to the height (km) of the dynamical tropopause (PV=2.5 PVU)
; calculate the height (km), pressure (hPa), and potential temperature (K) of the thermal tropopause
;
    kindex=where(abs(reform(z_prof(n,*))) lt 40.,nzz)
    z0=reform(z_prof(n,kindex))
    p0=reform(p_prof(n,kindex))
    tp0=reform(tp_prof(n,kindex))
    th0=tp0+(1000./p0)^0.286
    pv0=reform(pv_prof(n,kindex))
    index=sort(p0)	; sort profiles top down
    z0=z0(index)
    p0=p0(index)
    tp0=tp0(index)
    th0=th0(index)
    pv0=pv0(index)
    pvval=2.5e-6
    interp_poam,1,nzz,reverse(z0),reverse(abs(pv0)),aer_theta,pvval
;   if aer_theta gt 0. then dyntrop(n)=aer_theta
;   if aer_theta lt 0. then dyntrop(n)=pvval
;   print,'dyntrop= ',dyntrop(n)
    tropopause,tp0,p0,z0,th0,nzz,p_trop,z_trop,th_trop
;   print,'thermtrop ',z_trop,p_trop,th_trop
    zthermtrop(n)=z_trop
    pthermtrop(n)=p_trop
    ththermtrop(n)=th_trop
jumpprof:
endfor

zindex=where(th eq 350.)
ilev=zindex(0)
plot,h2o_prof(*,ilev),o3_prof(*,ilev),psym=4,symsize=0.5,title='350 K',xtitle='Water Vapor',ytitle='Ozone',/noeras,xrange=[0.1,1000.],/xlog,yrange=[0.,2.]
time=cattime
save,file='/Volumes/cloud/data/WACCM_data/Datfiles_SOSST/dmps_socrates.sdwaccm.'+socyear(iyear),id,date,time,$
     longitude,latitude,th,p_prof,tp_prof,z_prof,pv_prof,o3_prof,h2o_prof,dyntrop,zthermtrop,pthermtrop,ththermtrop
endfor
end
