;
; map MLS data to a lat/lon grid
; 2x/day separated by UT_TIME
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MLS-Aura_L2GP-BrO_v03-43-c01_2014d001.he5               
;MLS-Aura_L2GP-HNO3_v03-43-c01_2014d001.he5
;MLS-Aura_L2GP-CO_v03-43-c01_2014d001.he5                
;MLS-Aura_L2GP-HO2_v03-43-c01_2014d001.he5
;MLS-Aura_L2GP-ClO_v03-43-c01_2014d001.he5               
;MLS-Aura_L2GP-N2O_v03-43-c01_2014d001.he5
;MLS-Aura_L2GP-GPH_v03-43-c01_2014d001.he5               
;MLS-Aura_L2GP-O3_v03-43-c01_2014d001.he5
;MLS-Aura_L2GP-H2O_v03-43-c01_2014d001.he5               
;MLS-Aura_L2GP-Temperature_v03-43-c01_2014d001.he5
;MLS-Aura_L2GP-HCl_v03-43-c01_2014d001.he5

@/Users/harvey/France/IDL_files/readl2gp_std
@/Users/harvey/France/IDL_files/date2doy
@/Users/harvey/France/IDL_files/aura2date
@/Users/harvey/France/IDL_files/kgmt
MLS_path='/atmos/aura6/data/MLS_data/Datfiles/'
output_path='/atmos/aura6/data/MLS_data/Datfiles_Grid/'

;LONGITUDE (lon)
;double[144]
lon=dindgen(144L)*2.5
nlon=n_elements(lon) & lonstep=(lon[1:*]-lon[0:nlon-2])[nlon/2]
nc=nlon

;LATITUDE (lat)
;double[96]
lat=dindgen(96L)* 1.89474 -90.0
nlat=n_elements(lat) & latstep=(lat[1:*]-lat[0:nlat-2])[nlat/2]
nr=nlat

version='v4.2'
start_date='20180806'
end_date='20180812'
ndays=julday(strmid(end_date,4,2),strmid(end_date,6,2),strmid(end_date,0,4)) - $
      julday(strmid(start_date,4,2),strmid(start_date,6,2),strmid(start_date,0,4)) +1
current_date = start_date
start_julday = julday(strmid(start_date,4,2),strmid(start_date,6,2),strmid(start_date,0,4))
; day loop
for iday=0,ndays-1 do begin
s0=systime(1)

;prepare date variables
current_julday = start_julday + iday
caldat, current_julday, month, day, year
current_doy = current_julday-julday(1,1,year)+1
str_year = strcompress(string(year),/r)

doystr = strmid(strtrim(strcompress(year),2),0,4)+strtrim(string(format='(I2.2)',month),2)+strtrim(string(format='(I2.2)',day),2)
str_date_code = str_year+'d'+strcompress(string(current_doy,format='(i3.3)'),/r)
ifile = strcompress(strtrim(string(year),2))+strcompress(string(month,format='(I02)'))+strcompress(string(day,format='(I02)'))

x=file_search(output_path+'MLS_grid5_ALL_U_V_2xdaily_'+version+'_'+ifile + '.sav',count=count) & if count eq 1 then continue		;SKIP DAY IF ALREADY PROCESSED

print, 'Starting '+str_date_code+' !!!'

; spawn, 'ls '+ output_path+'MLS_grid5_ALL_v3.3_'+strcompress(strtrim(string(year),2))+strcompress(string(month,format='(I02)'))$
;	+strcompress(string(day,format='(I02)')) + '.sav',ifile, count = nfile
;	if nfile eq 1L then continue
brofiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-BrO_v04-*_'+str_date_code+'.he5')
cofiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-CO_v04-*_'+str_date_code+'.he5')
clofiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-ClO_v04-*_'+str_date_code+'.he5')
gpfiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-GPH_v04-*_'+str_date_code+'.he5')
h2ofiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-H2O_v04-*_'+str_date_code+'.he5')
hclfiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-HCl_v04-*_'+str_date_code+'.he5')
hno3files=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-HNO3_v04-*_'+str_date_code+'.he5')
n2ofiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-N2O_v04-*_'+str_date_code+'.he5')
o3files=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-O3_v04-*_'+str_date_code+'.he5')
tpfiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-Temperature_v04-*_'+str_date_code+'.he5')
ho2files=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-HO2_v04-*_'+str_date_code+'.he5')
ohfiles=FILE_SEARCH(MLS_path+'MLS-Aura_L2GP-OH_Temperature_v04-*_'+str_date_code+'.he5')

 result1=size(brofiles)
 result2=size(cofiles)
 result3=size(clofiles)
 result4=size(gpfiles)
 result5=size(h2ofiles)
 result6=size(hclfiles)
 result7=size(hno3files)
 result8=size(n2ofiles)
 result9=size(o3files)
 result10=size(tpfiles)
 result11=size(ho2files)
 result12=size(ohfiles)
 ;
 ; this logic will jump day if any one of the above 10 products are missing
 ;
 result1_1=1
 result2_1=1
 result3_1=1
 result4_1=1
 result5_1=1
 result6_1=1
 result7_1=1
 result8_1=1
 result9_1=1
 result10_1=1
 result11_1=1
 result12_1=1
if result1[0] eq 0L then result1_1=0
 if result2[0] eq 0L then result2_1=0
 if result3[0] eq 0L then result3_1=0
 if result4[0] eq 0L then result4_1=0
 if result5[0] eq 0L then result5_1=0
 if result6[0] eq 0L then result6_1=0
 if result7[0] eq 0L then result7_1=0
 if result8[0] eq 0L then result8_1=0
 if result9[0] eq 0L then result9_1=0
 if result10[0] eq 0L then result10_1=0
 if result11[0] eq 0L then result11_1=0
 if result12[0] eq 0L then result12_1=0

 if result1_1 eq 0 or result2_1 eq 0 or result3_1 eq 0 or result4_1 eq 0 or result5_1 eq 0 or result6_1 eq 0 or result7_1 eq 0 or result8_1 eq 0 or result9_1 eq 0 or result10_1 eq 0 then continue

 print,'MLS data complete on '+str_date_code

  if result1_1 eq 1 then bro=readl2gp_std(brofiles[0],swathName='BrO',variableName=variableName,precisionName=precisionName)
  if result2_1 eq 1 then co=readl2gp_std(cofiles[0],swathName='CO',variableName=variableName,precisionName=precisionName)
  if result3_1 eq 1 then clo=readl2gp_std(clofiles[0],swathName='ClO',variableName=variableName,precisionName=precisionName)
  if result4_1 eq 1 then gp=readl2gp_std(gpfiles[0],swathName='GPH',variableName=variableName,precisionName=precisionName)
  if result5_1 eq 1 then h2o=readl2gp_std(h2ofiles[0],swathName='H2O',variableName=variableName,precisionName=precisionName)
  if result6_1 eq 1 then hcl=readl2gp_std(hclfiles[0],swathName='HCl',variableName=variableName,precisionName=precisionName)
  if result7_1 eq 1 then hno3=readl2gp_std(hno3files[0],swathName='HNO3',variableName=variableName,precisionName=precisionName)
  if result8_1 eq 1 then n2o=readl2gp_std(n2ofiles[0],swathName='N2O',variableName=variableName,precisionName=precisionName)
  if result9_1 eq 1 then o3=readl2gp_std(o3files[0],swathName='O3',variableName=variableName,precisionName=precisionName)
  if result10_1 eq 1 then tp=readl2gp_std(tpfiles[0],swathName='Temperature',variableName=variableName,precisionName=precisionName)
  if result11_1 eq 1 then ho2=readl2gp_std(ho2files[0],swathName='HO2',variableName=variableName,precisionName=precisionName)
  if result12_1 eq 1 then begin
  	oh=readl2gp_std(ohfiles[0],swathName='OH',variableName=variableName,precisionName=precisionName)
	pmls3=oh.pressure         ; 49 elements (OH)
  	mlev3=n_elements(pmls3)
  	zcommon=fltarr(mlev3)
  	for k=0L,mlev3-1L do begin
  		index=where(pmls3(k) eq pmls)
  		if index[0] ne -1L then zcommon(k)=1.
  	endfor
  endif

if result10_1 eq 0 or result1_1 eq 0 then continue
;
; extract some catalog information from MLS ozone structure... dimensions,x,y,t,p
;
 mprof=tp.ntimes
 pmls=bro.pressure         ; 37 elements (BrO,ClO,CO,HCl,HNO3,N2O)
 pmls2=tp.pressure         ; 55 elements (GPH,H2O,O3,Temp)
 mlev=n_elements(pmls)
 mlev2=n_elements(pmls2)
;
; common levels where zflag=1
;
 zflag=fltarr(mlev2)
 for k=0L,mlev2-1L do begin
 	index=where(pmls2(k) eq pmls)
 	if index[0] ne -1L then zflag(k)=1.
 endfor
 kindex=where(zflag eq 1.)

 longitude=tp.longitude
 index=where(longitude lt 0. and longitude ne -999.990)
 if index[0] ne -1L then longitude(index)=longitude(index)+360.
 latitude=tp.latitude
 mtime=float(tp.time)
;
; convert MLS time from elapsed seconds to date (yyyymmddhh)
;
 aura2date,yyyymmddhh,mtime
;
; convert yyyymmdd to UT time
;
 mno=[31,28,31,30,31,30,31,31,30,31,30,31]

 yyyymmdd=yyyymmddhh/100L
 uttime=mtime
 istime=1993010100L
 ehr=uttime/60./60.       ; convert time from seconds to hours
 hh2=0.*uttime
 for n=0L,mprof-1L do begin
 	yy1=istime/1000000L
 	if yy1 mod 4 eq 0 then mno(1)=29L
 	if yy1 mod 4 ne 0 then mno(1)=28L
 	mm1=istime/10000L-yy1*100L
 	dd1=istime/100L-yy1*10000L-mm1*100L
 	dd2=dd1+long(ehr(n))/24L
 	hh1=istime-yy1*1000000L-mm1*10000L-dd1*100L
 	yy2=yy1 & mm2=mm1
 	while dd2 gt mno(mm2-1) do begin
 		dd2=dd2-mno(mm2-1)
 		mm2=mm2+1L
 		if mm2 gt 12L then begin
 			mm2=mm2-12L
 			yy2=yy2+1L
 			if yy2 mod 4 eq 0 then mno(1)=29
 			if yy2 mod 4 ne 0 then mno(1)=28
 		endif
 	endwhile
 	hh2(n)=ehr(n) mod 24
 	if hh2(n) ge 24. then begin
 		hh2(n)=hh2(n)-24.
 		dd2=dd2+1L
 		if dd2 gt mno(mm2-1L) then begin
 			dd2=dd2-mno(mm2-1L)
 			mm2=mm2+1L
 			if mm2 gt 12L then begin
 				mm2=mm2-12L
 				yy2=yy2+1L
 			endif
 		endif
 	endif
 endfor
 uttime=hh2
;
; build fractional day from day of year and UT time
;
 fdoy=0.*uttime
 syyyymmdd=strcompress(yyyymmdd,/remove_all)
 for i=0L,mprof-1L do begin
 	kyr=long(strmid(syyyymmdd(i),0,4))
 	kmn=long(strmid(syyyymmdd(i),4,2))
 	kdy=long(strmid(syyyymmdd(i),6,2))
 	kgmt,kmn,kdy,kyr,kday
 	fdoy(i)=float(kday)+uttime(i)/24.
 endfor
;
; CALCULATE LOCAL TIME AND LOCAL DAY.  THE TIME DIFFERENCE FROM UTTIME, dH, IS THE EAST
; LONGITUDE MULTIPLIED BY 24 HOURS/360 DEGREES.  0 LONGITUDE IS UTC, 180 LONGITUDE IS
; INTERNATIONAL DATE LINE.  THEREFORE, THE LOCAL TIME CAN BE CALCULATED AS:
;     0 LE LON LE 180:  LTIME=UT+dH.  IF LT>24 THEN LT=LT-24 AND LOCAL DAY=DAY+1.
;   180 LT LON LT 360:  LTIME=UT-dH.  IF LT<0  THEN LT=LT+24 AND LOCAL DAY=DAY-1.
;
 LDAY=FIX(FDOY) & LTIME=UTTIME
 L=LONGITUDE
 X=WHERE(L GT 180.,NX)
 IF NX GT 0 THEN L(X)=360.-L(X)    ; L is the Delta Longitude from 0, ranging from 0-180.
 X=WHERE(LONGITUDE LE 180. and LONGITUDE ne -999.990,NX)
 IF NX GT 0 THEN LTIME(X)=UTTIME(X)+L(X)*24./360.
 X=WHERE(LONGITUDE GT 180.,NX)
 IF NX GT 0 THEN LTIME(X)=UTTIME(X)-L(X)*24./360.
 X=WHERE(LTIME GT 24.,NX)
 IF NX GT 0 THEN BEGIN
 	LTIME(X)=LTIME(X)-24.
 	LDAY(X)=fix(FDOY(X))+1
 ENDIF
 X=WHERE(LTIME LT 0.,NX)
 IF NX GT 0 THEN BEGIN
 	LTIME(X)=LTIME(X)+24.
 	LDAY(X)=fix(FDOY(X))-1
 ENDIF
;
; extract individual species information from MLS structures
; use quality, status, and precision flags to "mask" suspect data
; status=0 is good, all odd values are bad
; impose quality limits for v4.2 (see http://mls.jpl.nasa.gov/data/v4-2_data_quality_document.pdf)
; impose convergence limits for v4.2
; screen profiles outside recommended altitude range
;
broprs2d=transpose(bro.L2GPVALUE)*0.			; 2d 37 level pressure array
 for i=0L,mprof-1L do broprs2d[i,*]=pmls
 tpprs2d=transpose(tp.L2GPVALUE)*0.			; 2d 55 level pressure array
 for i=0L,mprof-1L do tpprs2d[i,*]=pmls2

 ;
if result1_1 eq 1 then begin
     	bromls=transpose(bro.L2GPVALUE)
      	broprecision=transpose(bro.L2GPPRECISION)
      	brostatus=bro.STATUS
      	broquality=bro.QUALITY
      	broconvergence=bro.CONVERGENCE
      	bromask=0.*bromls
      	brobad=where(broprecision lt 0.)
      	if brobad[0] ne -1L then bromask[brobad]=-99.
      	brobad=where(broprecision lt 0.)
      	if brobad(0) ne -1L then bromask(brobad)=-99.
      	brobad=where(brostatus mod 2 ne 0L)		; use if status is even
      	if brobad[0] ne -1L then bromask[brobad,*]=-99.
      	brobad=where(broquality lt 1.3)			; do not use if quality < 1.3
      	if brobad[0] ne -1L then bromask[brobad,*]=-99.
      	brobad=where(broconvergence gt 1.05)                   ; do not use if convergence > 1.05
      	if brobad[0] ne -1L then bromask[brobad,*]=-99.
      	brobad=where(broprs2d gt 10. or broprs2d lt 3.2)                   ; do not use outside Pressure range 10-3.2 hPa
      	if brobad[0] ne -1L then bromask[brobad]=-99.
     endif

     if result2_1 eq 1 then begin
      	comls=transpose(co.L2GPVALUE)
      	coprecision=transpose(co.L2GPPRECISION)
      	costatus=co.STATUS
      	coquality=co.QUALITY
      	coconvergence=co.CONVERGENCE
      	comask=0.*comls
      	cobad=where(coprecision lt 0.)
      	if cobad[0] ne -1L then comask[cobad]=-99.
      	cobad=where(costatus mod 2 ne 0L)			; use if status is even
      	if cobad[0] ne -1L then comask[cobad,*]=-99.
      	cobad=where(coquality lt 1.5)			; do not use if quality < 1.5
      	if cobad[0] ne -1L then comask[cobad,*]=-99.
      	cobad=where(coconvergence gt 1.03)                     ; do not use if convergence > 1.03
      	if cobad[0] ne -1L then comask[cobad,*]=-99.
      	cobad=where(broprs2d gt 215. or broprs2d lt 0.0046)                   ; do not use outside Pressure range 215-0.0046 hPa
      	if cobad[0] ne -1L then comask[cobad]=-99.
     endif

     if result3_1 eq 1 then begin
      	clomls=transpose(clo.L2GPVALUE)
      	cloprecision=transpose(clo.L2GPPRECISION)
      	clostatus=clo.STATUS
      	cloquality=clo.QUALITY
      	cloconvergence=clo.CONVERGENCE
      	clomask=0.*clomls
      	clobad=where(cloprecision lt 0.)
      	if clobad[0] ne -1L then clomask[clobad]=-99.
      	clobad=where(clostatus ne 0L)			; only use status=0. this is a conservative approach
      	if clobad[0] ne -1L then clomask[clobad,*]=-99.
      	clobad=where(cloquality lt 1.3)			; do not use if quality < 1.3
      	if clobad[0] ne -1L then clomask[clobad,*]=-99.
      	clobad=where(cloconvergence gt 1.05)                   ; do not use if convergence > 1.05
      	if clobad[0] ne -1L then clomask[clobad,*]=-99.
      	clobad=where(broprs2d gt 147. or broprs2d lt 1.0)                   ; do not use outside Pressure range 147-1 hPa
      	if clobad[0] ne -1L then clomask[clobad]=-99.
     endif
     
     if result4_1 eq 1 then begin
      	gpmls=transpose(gp.L2GPVALUE)
      	gpprecision=transpose(gp.L2GPPRECISION)
      	gpstatus=gp.STATUS
      	gpquality=gp.QUALITY
      	gpconvergence=gp.CONVERGENCE
      	gpmask=0.*gpmls
      	gpbad=where(gpprecision lt 0.)
      	if gpbad[0] ne -1L then gpmask[gpbad]=-99.
      	gpbad=where(gpstatus mod 2 ne 0L)
      	if gpbad[0] ne -1L then gpmask[gpbad,*]=-99.
     endif
     
     if result5_1 eq 1 then begin
      	h2omls=transpose(h2o.L2GPVALUE)
      	h2oprecision=transpose(h2o.L2GPPRECISION)
      	h2ostatus=h2o.STATUS
      	h2oquality=h2o.QUALITY
      	h2oconvergence=h2o.CONVERGENCE
      	h2omask=0.*h2omls
      	h2obad=where(h2oprecision lt 0.)
      	if h2obad[0] ne -1L then h2omask[h2obad]=-99.
      	h2obad=where(h2ostatus mod 2 ne 0L)
      	if h2obad[0] ne -1L then h2omask[h2obad,*]=-99.
      	h2obad=where(h2oquality lt 1.45)			; do not use if h2oquality < 1.45
      	if h2obad[0] ne -1L then h2omask[h2obad,*]=-99.
      	h2obad=where(h2oconvergence gt 2.)                   ; do not use if convergence > 2.0
      	if h2obad[0] ne -1L then h2omask[h2obad,*]=-99.
      	h2obad=where(tpprs2d gt 316. or tpprs2d lt 0.002)                   ; do not use outside Pressure range 316-0.002 hPa
      	if h2obad[0] ne -1L then h2omask[h2obad]=-99.
     endif
     
     if result6_1 eq 1 then begin
      	hclmls=transpose(hcl.L2GPVALUE)
      	hclprecision=transpose(hcl.L2GPPRECISION)
      	hclstatus=hcl.STATUS
      	hclquality=hcl.QUALITY
      	hclconvergence=hcl.CONVERGENCE
	hclmask=0.*hclmls
	hclbad=where(hclprecision lt 0.)
	if hclbad[0] ne -1L then hclmask[hclbad]=-99.
	hclbad=where(hclstatus mod 2 ne 0L)
	if hclbad[0] ne -1L then hclmask[hclbad,*]=-99.
	hclbad=where(hclquality lt 1.2)			; do not use if quality < 1.2
	if hclbad[0] ne -1L then hclmask[hclbad,*]=-99.
	hclbad=where(hclconvergence gt 1.05)                    ; do not use if convergence > 1.05
	if hclbad[0] ne -1L then hclmask[hclbad,*]=-99.
	hclbad=where(broprs2d gt 100. or broprs2d lt 0.32)                   ; do not use outside Pressure range 100-0.32 hPa
	if hclbad[0] ne -1L then hclmask[hclbad]=-99.
     endif
     
     if result7_1 eq 1 then begin
      	hno3mls=transpose(hno3.L2GPVALUE)
      	hno3precision=transpose(hno3.L2GPPRECISION)
      	hno3status=hno3.STATUS
      	hno3quality=hno3.QUALITY
      	hno3convergence=hno3.CONVERGENCE
      	hno3mask=0.*hno3mls
      	hno3bad=where(hno3precision lt 0.)
      	if hno3bad[0] ne -1L then hno3mask[hno3bad]=-99.
      	hno3bad=where(hno3status ne 0L)			; discard all non-zero HNO3 status values due to potential cloud contamination below 68 hPa
      	if hno3bad[0] ne -1L then hno3mask[hno3bad,*]=-99.
      	hno3bad=where(hno3quality lt 0.8)			; do not use if quality < 0.8
      	if hno3bad[0] ne -1L then hno3mask[hno3bad,*]=-99.
      	hno3bad=where(hno3convergence gt 1.03)                 ; do not use if convergence > 1.03
      	if hno3bad[0] ne -1L then hno3mask[hno3bad,*]=-99.
      	hno3bad=where(broprs2d gt 215. or broprs2d lt 1.5)                 ; do not use outside Pressure range: 215 - 1.5 hPa
      	if hno3bad[0] ne -1L then hno3mask[hno3bad]=-99.
     endif
     
     if result8_1 eq 1 then begin
      	n2omls=transpose(n2o.L2GPVALUE)
      	n2oprecision=transpose(n2o.L2GPPRECISION)
      	n2ostatus=n2o.STATUS
      	n2oquality=n2o.QUALITY
      	n2oconvergence=n2o.CONVERGENCE
      	n2omask=0.*n2omls
      	n2obad=where(n2oprecision lt 0.)
      	if n2obad[0] ne -1L then n2omask[n2obad]=-99.
      	n2obad=where(n2ostatus mod 2 ne 0L)
      	if n2obad[0] ne -1L then n2omask[n2obad,*]=-99.
      	n2obad=where(n2oquality lt 1.3)			; do not use if quality < 1.3 (N2O-190 is standard product)
      	if n2obad[0] ne -1L then n2omask[n2obad,*]=-99.
      	n2obad=where(n2oconvergence gt 2.0)                   ; do not use if convergence > 2.0 (N2O-190 is standard product)
      	if n2obad[0] ne -1L then n2omask[n2obad,*]=-99.
      	n2obad=where(broprs2d gt 68. or broprs2d lt 0.46)                   ; do not use outside Pressure range 68-0.46 hPa (N2O-190 is standard product)
      	if n2obad[0] ne -1L then n2omask[n2obad]=-99.
     endif
     
     if result9_1 eq 1 then begin
      	o3mls=transpose(o3.L2GPVALUE)
      	o3precision=transpose(o3.L2GPPRECISION)
      	o3status=o3.STATUS
      	o3quality=o3.QUALITY
      	o3convergence=o3.CONVERGENCE
	o3mask=0.*o3mls
	o3bad=where(o3precision lt 0.)
	if o3bad[0] ne -1L then o3mask[o3bad]=-99.
	o3bad=where(o3status mod 2 ne 0L)
	if o3bad[0] ne -1L then o3mask[o3bad,*]=-99.
	o3bad=where(o3quality lt 1.0)			; do not use if quality < 1.0
	if o3bad[0] ne -1L then o3mask[o3bad,*]=-99.
	o3bad=where(o3convergence gt 1.03)                     ; do not use if convergence > 1.03
	if o3bad[0] ne -1L then o3mask[o3bad,*]=-99.
	o3bad=where(tpprs2d gt 261. or tpprs2d lt 0.02)			; do not use outside Pressure range 261-0.02 hPa
	if o3bad[0] ne -1L then o3mask[o3bad]=-99.
     endif
     
     if result10_1 eq 1 then begin
      	tpmls=transpose(tp.L2GPVALUE)
      	tpprecision=transpose(tp.L2GPPRECISION)
      	tpstatus=tp.STATUS
      	tpquality=tp.QUALITY
      	tpconvergence=tp.CONVERGENCE
      	tpmask=0.*tpmls
      	tpbad=where(tpprecision lt 0.)
      	if tpbad[0] ne -1L then tpmask[tpbad]=-99.
      	tpbad=where(tpstatus mod 2 ne 0L)
      	if tpbad[0] ne -1L then tpmask[tpbad,*]=-99.
      	tpbad=where(tpquality lt 0.2)			; apply "stratospheric" threshhold: do not use if tpquality < 0.9 (261-100) or if < 0.2 (83-0.001)
      	if tpbad[0] ne -1L then tpmask[tpbad,*]=-99.
      	if tpbad[0] ne -1L then gpmask[tpbad,*]=-99.
      	tpbad=where(tpconvergence gt 1.03)                     ; do not use if convergence > 1.03
      	if tpbad[0] ne -1L then tpmask[tpbad,*]=-99.
      	tpbad=where(tpprs2d gt 261. or tpprs2d lt 0.001)                 ; do not use outside Pressure range: 261-0.001 hPa
      	if tpbad[0] ne -1L then tpmask[tpbad]=-99.
      	if tpbad[0] ne -1L then gpmask[tpbad]=-99.
      	gpbad=where(tpconvergence gt 1.03)                     ; do not use if convergence > 1.03
      	if gpbad[0] ne -1L then gpmask[gpbad,*]=-99.
     endif
     if result11_1 eq 1 then begin
        ho2mls=transpose(ho2.L2GPVALUE)
        ho2precision=transpose(ho2.L2GPPRECISION)
	ho2status=ho2.STATUS
	ho2quality=ho2.QUALITY
	ho2convergence=ho2.CONVERGENCE
	ho2mask=0.*ho2mls
	ho2bad=where(ho2precision lt 0.)
	if ho2bad[0] ne -1L then ho2mask[ho2bad]=-99.
	ho2bad=where(ho2status mod 2 ne 0L)
	if ho2bad[0] ne -1L then ho2mask[ho2bad,*]=-99.
;     ho2bad=where(ho2quality lt 1.e100)                 ; HO2 has no quality threshhold
;     if ho2bad[0] ne -1L then ho2mask[ho2bad,*]=-99.
	ho2bad=where(ho2convergence gt 1.1)                 ; do not use if convergence > 1.1
	if ho2bad[0] ne -1L then ho2mask[ho2bad,*]=-99.
	ho2bad=where(broprs2d gt 22. or broprs2d lt 0.046)                 ; do not use outside Pressure range: 22-0.046 hPa
	if ho2bad[0] ne -1L then ho2mask[ho2bad]=-99.
     endif
     if result12_1 eq 1 then begin
	ohmls=transpose(oh.L2GPVALUE)
	ohprecision=transpose(oh.L2GPPRECISION)
	ohstatus=oh.STATUS
	ohquality=oh.QUALITY
	ohconvergence=oh.CONVERGENCE
	ohmask=0.*ohmls
	ohbad=where(ohprecision lt 0.)
	if ohbad[0] ne -1L then ohmask[ohbad]=-99.
	ohbad=where(ohstatus mod 2 ne 0L)
	if ohbad[0] ne -1L then ohmask[ohbad,*]=-99.
;     ohbad=where(ohquality lt 1.e100)                 ; OH has no quality threshhold
;     if ohbad[0] ne -1L then ohmask[ohbad,*]=-99.
	ohbad=where(ohconvergence gt 1.1)                 ; do not use if convergence > 1.1
	if ohbad[0] ne -1L then ohmask[ohbad,*]=-99.
	ohbad=where(broprs2d gt 32. or broprs2d lt 0.0032)                 ; do not use outside Pressure range: 32-0.0032 hPa
	if ohbad[0] ne -1L then ohmask[ohbad]=-99.
     endif
;
; calculate geometric altitude from geopotential height
; use geometric altitude to interpolate data from pressure levels to SOSST altitude grid
;
 ks=1.931853d-3
 ecc=0.081819
 gamma45=9.80
 rtd=double(180./!pi)
 dtr=1./rtd
 zmls=0.*gpmls
 gpkm=gpmls/1000.
 for j=0L,mprof-1L do begin
 	sin2=sin( (latitude[j]*dtr)^2.0 )
 	numerator=1.0+ks*sin2
 	denominator=sqrt( 1.0 - (ecc^2.0)*sin2 )
 	gammas=gamma45*(numerator/denominator)
 	r=6378.137/(1.006803-(0.006706*sin2))
 	zmls[j,*]= (r*gpkm[j,*])/ ( (gammas/gamma45)*r - gpkm[j,*] )
 endfor
 index=where(gpmls eq -999.990)
 if index[0] ne -1L then zmls(index)=-999.990

date2doy,doystr,doy

ntimes = n_elements(gpmls[*,0])
fdoy = current_doy 
    pi=3.14159265
    dtor=pi/180.
    earinc=23.5
    zangle=fltarr(n_elements(latitude))
    for ii=0L,n_elements(latitude)-1 do begin
        rlat=latitude(ii)
        rlon=longitude(ii)
        gmt=ltime(ii)
        sinlat=sin(rlat*dtor)
        coslat=sqrt(1.-sinlat^2.)
        sinlon=sin(rlon*dtor)
        coslon=cos(rlon*dtor)
        soya=(doy-81.25)*pi/182.5           ; day angle
        soha=2.*pi*(gmt-12.)/24.            ; hour angle
        soha=-soha
        sininc=sin(earinc*dtor)
        sindec=sininc*sin(soya)
        cosdec= sqrt(1.-sindec^2.)
        coszen=cos(soha)*coslon+sin(soha)*sinlon
        coszen=coszen*cosdec*coslat
        coszen=sindec*sinlat+coszen
        coszen=min([max([coszen,-1.]),1.])
        chi = acos(coszen)
        zangle(ii) = chi/dtor
    endfor
;
;produce ascending/descending flag
; 
lat_gradient=convol(latitude,[-1,0,1],/normalize,/center,/nan,/edge_truncate)
 
morning=where(uttime le 12.,comp=evening)

UT_6Z_18Z = uttime*0L
UT_6Z_18Z[morning]=-1
UT_6Z_18Z[evening]=1

node=1*(lat_gradient gt 0)-1*(lat_gradient lt 0) ;1 = ascending, -1 = descending, 0 = neither
;  
;	device,decompose=0
    x=where(latitude lt -100 or ltime lt 0)
    ltime[x]=!values.f_nan
    latitude[x]=!values.f_nan
    x=where(node eq 1)
;   loadct,0
;   plot, ltime[x],latitude[x],xtitle='local time',ytitle='Latitude',yrange = [-90,90],xrange = [0,24],charsize=2,ystyle=1,yticks=6,title='MLS '+ifile,psym=2,color=250,/nodata
;   loadct,39
;   oplot, ltime[x],latitude[x],psym=2,color=100
;    x=where(node eq -1)
;    oplot, ltime[x],latitude[x],color = 250,psym=4

x = where(gpmls lt 0.,nx)
if nx gt 0L then gpmls[x] = !values.f_nan
index=where(longitude lt 0 or longitude gt 360,nindex)
if nindex ne 0 then longitude[index]=!values.f_nan
index=where(latitude lt -90 or latitude gt 90,nindex)
if nindex ne 0 then latitude[index]=!values.f_nan
index=where(finite(longitude) ne 1 or finite(latitude) ne 1,nindex)
; produce sunlight flag
earth_radius=6367500. ;in m
sza_sunset=180.-180./!pi*asin(earth_radius/(earth_radius))
;  sunlight=transpose(rebin(sza,n_elements(sza),mlev2)) lt sza_sunset  ;1 = sunlight, 0 = darkness
;  
;  GRID DATA

if result1_1 eq 1 then begin
	if n_elements(BrOmls[0,*]) eq 55L then lev = pmls2
	if n_elements(BrOmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	BrO_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	BrO_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	x = where(BrOmls lt 0. or BrOmask eq -99.,nx)
	if nx gt 0L then BrOmls[x] = !values.f_nan
	value = BrOmls
	ntimes = n_elements(BrOmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
              ;gridding  
			for ilev=0,nlev-1 do begin    
				for inode= -1,1,2 do begin      
					gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(BrOmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)      
					if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere      
					grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(BrOmls[gindex,ilev]), $                  
               		prep_cartesian, prep_values, $                  
               		/sphere,/degrees,duplicates='Avg',epsilon=.5                       
               		prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)      
               		prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)                  
               		prep_lat = reform(prep_spherical[1,*])      ;      
;               		prep_lon=MLS_lon[gindex];      
;               		prep_lat=MLS_lat[gindex];      
;               		prep_values=reform(t_mix[ilev,gindex])           
 
               		qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s            
               		case inode of	
               		 	-1: BrO_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))        
						1: BrO_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))        
					 	else: stop      
					 endcase
				endfor ;end of inode

			endfor ;end of ilev  print, '   MLS data gridded.'
			bro_grid=mean(bro_grid_node,dim=4,/nan)
endif			




if result2_1 eq 1 then begin
	if n_elements(COmls[0,*]) eq 55L then lev = pmls2
	if n_elements(COmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	CO_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	CO_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan

	x = where(COmls lt 0. or COmask eq -99.,nx)
	if nx gt 0L then COmls[x] = !values.f_nan
	value = COmls
	ntimes = n_elements(COmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(COmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(COmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: CO_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: CO_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	CO_grid=mean(CO_grid_node,dim=4,/nan)
endif



if result3_1 eq 1 then begin
	if n_elements(ClOmls[0,*]) eq 55L then lev = pmls2
	if n_elements(ClOmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	ClO_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	ClO_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan

	x = where(ClOmls lt 0. or ClOmask eq -99.,nx)
	if nx gt 0L then ClOmls[x] = !values.f_nan
	value = ClOmls
	ntimes = n_elements(ClOmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(ClOmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(ClOmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: ClO_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: ClO_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	ClO_grid=mean(ClO_grid_node,dim=4,/nan)
endif


if result4_1 eq 1 then begin
	if n_elements(gpmls[0,*]) eq 55L then lev = pmls2
	if n_elements(gpmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	gp_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	gp_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(gpmls lt 0. or gpmask eq -99.,nx)
	if nx gt 0L then gpmls[x] = !values.f_nan
	value = gpmls
	ntimes = n_elements(gpmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(gpmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(gpmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: gp_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: gp_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	gp_grid=mean(gp_grid_node,dim=4,/nan)
endif


if result5_1 eq 1 then begin
	if n_elements(H2Omls[0,*]) eq 55L then lev = pmls2
	if n_elements(H2Omls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	H2O_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	H2O_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(H2Omls lt 0. or H2Omask eq -99.,nx)
	if nx gt 0L then H2Omls[x] = !values.f_nan
	value = H2Omls
	ntimes = n_elements(H2Omls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(H2Omls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(H2Omls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: H2O_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: H2O_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	H2O_grid=mean(H2O_grid_node,dim=4,/nan)
endif


if result6_1 eq 1 then begin
	if n_elements(HClmls[0,*]) eq 55L then lev = pmls2
	if n_elements(HClmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	HCl_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	HCl_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(HClmls lt 0. or HClmask eq -99.,nx)
	if nx gt 0L then HClmls[x] = !values.f_nan
	value = HClmls
	ntimes = n_elements(HClmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(HClmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(HClmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: HCl_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: HCl_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	HCl_H2Od=mean(HCl_grid_node,dim=4,/nan)
endif


if result7_1 eq 1 then begin
	if n_elements(HNO3mls[0,*]) eq 55L then lev = pmls2
	if n_elements(HNO3mls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	HNO3_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	HNO3_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(HNO3mls lt 0. or HNO3mask eq -99.,nx)
	if nx gt 0L then HNO3mls[x] = !values.f_nan
	value = HNO3mls
	ntimes = n_elements(HNO3mls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(HNO3mls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(HNO3mls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: HNO3_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: HNO3_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	HNO3_grid=mean(HNO3_grid_node,dim=4,/nan)
endif


if result8_1 eq 1 then begin
	if n_elements(N2Omls[0,*]) eq 55L then lev = pmls2
	if n_elements(N2Omls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	N2O_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	N2O_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(N2Omls lt 0. or N2Omask eq -99.,nx)
	if nx gt 0L then N2Omls[x] = !values.f_nan
	value = N2Omls
	ntimes = n_elements(N2Omls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(N2Omls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(N2Omls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: N2O_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: N2O_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	N2O_grid=mean(N2O_grid_node,dim=4,/nan)
endif


if result9_1 eq 1 then begin
	if n_elements(O3mls[0,*]) eq 55L then lev = pmls2
	if n_elements(O3mls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	O3_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	O3_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(O3mls lt 0. or O3mask eq -99.,nx)
	if nx gt 0L then O3mls[x] = !values.f_nan
	value = O3mls
	ntimes = n_elements(O3mls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(O3mls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(O3mls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: O3_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: O3_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	O3_grid=mean(O3_grid_node,dim=4,/nan)
endif


if result10_1 eq 1 then begin
	if n_elements(tpmls[0,*]) eq 55L then lev = pmls2
	if n_elements(tpmls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	tp_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	tp_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(tpmls lt 0. or tpmask eq -99.,nx)
	if nx gt 0L then tpmls[x] = !values.f_nan
	value = tpmls
	ntimes = n_elements(tpmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(tpmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(tpmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: tp_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: tp_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	tp_grid=mean(tp_grid_node,dim=4,/nan)
endif


if result11_1 eq 1 then begin
	if n_elements(ho2mls[0,*]) eq 55L then lev = pmls2
	if n_elements(ho2mls[0,*]) eq 37L then lev = pmls
	nlev = n_elements(lev)
	ho2_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	ho2_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(ho2mls lt 0. or ho2mask eq -99.,nx)
	if nx gt 0L then ho2mls[x] = !values.f_nan
	value = ho2mls
	ntimes = n_elements(ho2mls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(ho2mls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(ho2mls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: ho2_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: ho2_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	HO2_grid=mean(HO2_grid_node,dim=4,/nan)
endif


if result12_1 eq 1 then begin
     lev = pmls3
	nlev = n_elements(lev)
	OH_grid = fltarr(nlon,nlat,nlev)*!values.f_nan
	OH_grid_node=fltarr(nlon,nlat,nlev,2)*!values.f_nan
	
	x = where(OHmls lt 0. or OHmask eq -99.,nx)
	if nx gt 0L then OHmls[x] = !values.f_nan
	value = OHmls
	ntimes = n_elements(OHmls[*,0])
	MLS_lon = longitude
	MLS_lat = latitude ;check for coordinates out of range  index=where(MLS_lon lt 0 or MLS_lon gt 360,nindex)
	if nindex ne 0 then MLS_lon[index]=!values.f_nan
	index=where(MLS_lat lt -90 or MLS_lat gt 90,nindex)
	if nindex ne 0 then MLS_lat[index]=!values.f_nan
	index=where(finite(MLS_lon) ne 1 or finite(MLS_lat) ne 1,nindex) ;
	;produce sunlight flag;
	;gridding
	for ilev=0,nlev-1 do begin
		for inode= -1,1,2 do begin
			gindex=where(finite(reform(MLS_lon[*])) and finite(reform(MLS_lat[* ])) and finite(reform(OHmls[*,ilev])) and UT_6Z_18Z eq inode,ngindex)
			if ngindex lt 24 then continue ;only grid when there are at least 24 points for the entire sphere
			grid_input, MLS_lon[gindex ], MLS_lat[gindex ], reform(OHmls[gindex,ilev]), $
				prep_cartesian, prep_values, $
				/sphere,/degrees,duplicates='Avg',epsilon=.5
			prep_spherical = cv_coord(/degrees, /double, from_rect=prep_cartesian, /to_sphere)
			prep_lon = reform(prep_spherical[0,*]*(prep_spherical[0,*] gt 0)+(360+prep_spherical[0,*])*(prep_spherical[0,*] lt 0)) ;transform from -180/+180 to 0/360)
			prep_lat = reform(prep_spherical[1,*])      ;
			;               		prep_lon=MLS_lon[gindex];
			;               		prep_lat=MLS_lat[gindex];
			;               		prep_values=reform(t_mix[ilev,gindex])
			
			qhull, prep_lon, prep_lat, tr, /delaunay, sphere=s
			case inode of
				-1: OH_grid_node[*,*,ilev,0]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				1: OH_grid_node[*,*,ilev,1]=reform(griddata(prep_lon, prep_lat, prep_values, /degrees, /grid, xout=lon, yout=lat, triangles=tr, /natural_neighbor, /sphere))
				else: stop
			endcase
		endfor ;end of inode
	endfor ;end of ilev  print, '   MLS data gridded.'
	OH_grid=mean(OH_grid_node,dim=4,/nan)
endif

 if result1_1 eq 0 then bro = 	bro_grid_node*!values.f_nan
 if result2_1 eq 0 then co =	co_grid_node*!values.f_nan
 if result3_1 eq 0 then clo = 	clo_grid_node*!values.f_nan
 if result4_1 eq 0 then gph = 	gp_grid_node*!values.f_nan
 if result5_1 eq 0 then h2o = 	h2o_grid_node*!values.f_nan
 if result6_1 eq 0 then hcl = 	hcl_grid_node*!values.f_nan
 if result7_1 eq 0 then hno3 =	hno3_grid_node*!values.f_nan
 if result8_1 eq 0 then n2o = 	n2o_grid_node*!values.f_nan
 if result9_1 eq 0 then o3 = 	o3_grid_node*!values.f_nan
 if result10_1 eq 0 then t = 	tp_grid_node*!values.f_nan
 if result11_1 eq 0 then ho2 = 	fltarr(n_elements(lon),n_elements(lat),49,2)*!values.f_nan
 if result12_1 eq 0 then oh = 	fltarr(n_elements(lon),n_elements(lat),49,2)*!values.f_nan
 if result12_1 eq 0 then pmls3 =fltarr(49)*!values.f_nan

 if result1_1 eq 1 then bro = 	bro_grid_node
 if result2_1 eq 1 then co = 	co_grid_node
 if result3_1 eq 1 then clo = 	clo_grid_node
 if result4_1 eq 1 then gph = 	gp_grid_node
 if result5_1 eq 1 then h2o = 	h2o_grid_node
 if result6_1 eq 1 then hcl = 	hcl_grid_node
 if result7_1 eq 1 then hno3 =	hno3_grid_node
 if result8_1 eq 1 then n2o = 	n2o_grid_node
 if result9_1 eq 1 then o3 = 	o3_grid_node
 if result10_1 eq 1 then t = 	tp_grid_node
 if result11_1 eq 1 then ho2 = 	ho2_grid_node
 if result12_1 eq 1 then oh = 	oh_grid_node
 
 ;Derive geostophic winds

 U =gp_grid_node*0.
 V = gp_grid_node*0.
 dZx =gp_grid_node[*,*,*,0]*0.
 dZy = gp_grid_node[*,*,*,0]*0.
 dZys = fltarr(n_elements(lon) + 5L, n_elements(lat), n_elements(pmls2))
 
 R = 6378100. ;radius of the earth (m)
 g = 9.81 ; gravit (m/s^2)
 omega = 7.292*(10.^(-5.))
 f = 2.*omega*sin(!DtoR*lat)
 
 dlat = 2.*!Pi*R/360. * (lat[1]-lat[0])
 dlon = 2.*!Pi*R/360.*cos(!DtoR*lat) * (lon[1]-lon[0])
 
 
 ;create wraparound for derivatives
 gp_grid_nodes = fltarr(n_elements(lon) + 5L, n_elements(lat), n_elements(pmls2),2)
 gp_grid_nodes[0:n_elements(lon)-1L,*,*,*] = gp_grid_node
 gp_grid_nodes[n_elements(lon):n_elements(lon)+4L,*,*,*] = gp_grid_node[0L:4L,*,*,*]
 lons = fltarr(n_elements(lon) + 5L)
 lons[0:n_elements(lon)-1L] = lon
 lons[n_elements(lon):n_elements(lon)+4L] = lon[0L:4L]
 
 
 
 for kk = 0L, n_elements(pmls2) - 1L do begin
 	for ii = 0L, n_elements(lons) - 1L do dZys[ii,*,kk] = deriv(smooth(gp_grid_nodes[ii,*,kk,0],5,/nan,/edge_truncate))
 	for ii = 0L, n_elements(lat) - 1L do dZx[*,ii,kk] = deriv(smooth(gp_grid_node[*,ii,kk,0],5,/nan,/edge_truncate))
 	
 	;Fix wraparound at Greenwhich Meridian
 	dZy[*,*,kk] = dzys[0:n_elements(lon)-1,*,kk]
 	dzy[0,*,kk] = dzys[n_elements(lon),*,kk]
 	dzy[1,*,kk] = dzys[n_elements(lon)+1,*,kk]
 	
 	
 	; Calculate geostrophic winds at each lat/lon
 	for ii = 0L, n_elements(lon) - 1L do begin
 		U[ii,*,kk,0] = -(g/f)*(dZy[ii,*,kk]/dlat)
 		V[ii,*,kk,0] = (g/f)*(dZx[ii,*,kk]/dlon[*])
 	endfor
 	for ii = 0L, n_elements(lons) - 1L do dZys[ii,*,kk] = deriv(smooth(gp_grid_nodes[ii,*,kk,1],5,/nan,/edge_truncate))
 	for ii = 0L, n_elements(lat) - 1L do dZx[*,ii,kk] = deriv(smooth(gp_grid_node[*,ii,kk,1],5,/nan,/edge_truncate))
 	
 	;Fix wraparound at Greenwhich Meridian
 	dZy[*,*,kk] = dzys[0:n_elements(lon)-1,*,kk]
 	dzy[0,*,kk] = dzys[n_elements(lon),*,kk]
 	dzy[1,*,kk] = dzys[n_elements(lon)+1,*,kk]
 	
 	; Calculate geostrophic winds at each lat/lon
 	for ii = 0L, n_elements(lon) - 1L do begin
 		U[ii,*,kk,1] = -(g/f)*(dZy[ii,*,kk]/dlat)
 		V[ii,*,kk,1] = (g/f)*(dZx[ii,*,kk]/dlon[*])
 	endfor
 endfor
 
UT_mean=[6,18]
 
if mprof lt 3150L then save, filename = output_path+'MLS_grid5_ALL_U_V_2xdaily_'+version+'_'+ifile + '.sav.bad', lon, lat,bro,co,clo,gph,h2o,hcl,$
        hno3,n2o,o3,t,ho2,oh,pmls,pmls2,pmls3,u,v,UT_mean
if mprof ge 3150L then begin
   save, filename = output_path+'MLS_grid5_ALL_U_V_2xdaily_'+version+'_'+ifile + '.sav', lon, lat,bro,co,clo,gph,h2o,hcl,$
	hno3,n2o,o3,t,ho2,oh,pmls,pmls2,pmls3,u,v,UT_mean

;erase
;loadct,39
;mcolor=byte(!p.color)
;device,decompose=0
;mcolor=byte(!p.color)
;set_viewport,0.1,0.9,0.1,0.9
;!type=2^2+2^3
;nlvls=16L
;col1=1+(indgen(nlvls)/float(nlvls))*mcolor
;ilev=45
;map_set,90,0,-90,/stereo,/contin,/grid,/noerase,color=mcolor,charsize=1.5,title=doystr+' '+strcompress(pmls2(ilev),/r)+'hPa'
;udum=reform(u(*,*,ilev,1))
;vdum=reform(v(*,*,ilev,1))
;udum1=fltarr(nc+1,nr)
;udum1(0:nc-1,0:nr-1)=udum
;udum1(nc,*)=udum1(0,*)
;vdum1=fltarr(nc+1,nr)
;vdum1(0:nc-1,0:nr-1)=vdum
;vdum1(nc,*)=vdum1(0,*)
;;dum=reform(gph(*,*,ilev,1))/1000.
;dum=sqrt(udum*udum+vdum*vdum)
;;dum=reform(dzx(*,*,ilev))
;nc=n_elements(lon)
;nr=n_elements(lat)
;lon1=[lon,lon(0)+360.]
;dum1=fltarr(nc+1,nr)
;dum1(0:nc-1,0:nr-1)=dum
;dum1(nc,*)=dum1(0,*)
;index=where(dum1 ne 0.)
;imin=min(dum1(index))
;imax=max(dum1(index))
;level=imin+((imax-imin)/float(nlvls-1L))*findgen(nlvls)
;level=10.*findgen(nlvls)
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,c_color=col1,/cell_fill,/overplot
;contour,dum1,lon1,lat,levels=level,/noeras,charsize=2,color=0,/foll,/overplot,thick=2
;drawvectors,nc+1,nr,lon1,lat,udum1,vdum1,10,1
;;velovect,udum1,vdum1,lon1,lat1,length=20,/overplot
;;oplot,longitude,latitude,psym=1
;map_set,90,0,-90,/stereo,/contin,/grid,/noerase,color=mcolor,charsize=1.5
;stop

endif

print,'   Gridded MLS data saved.'
print, 'Finished '+str_date_code+' in '+strcompress(string(systime(1)-s0,format='(i4)'),/r)+'s !!!'
endfor ;end of iday loop
print,'...done.'
end
