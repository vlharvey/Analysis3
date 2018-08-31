;
; read WACCM4 netcdf data from Ethan Peck
; check for missing days
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2
;
@stddat
@kgmt
@ckday
@kdate

print,'SmidEmin set2'
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/CLONO2_CO2x1SmidEmin_yBWCN_vE.sav
print,'clono2 ',min(dates),max(dates)
clono2=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/CO_CO2x1SmidEmin_yBWCN_vE.sav
print,'co ',min(dates),max(dates)
co=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/DTCORE_CO2x1SmidEmin_yBWCN_vE.sav
print,'dtcore ',min(dates),max(dates)
dtcore=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/DTV_CO2x1SmidEmin_yBWCN_vE.sav
print,'dtv ',min(dates),max(dates)
dtv=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/EKGWSPEC_CO2x1SmidEmin_yBWCN_vE.sav
print,'ekgwspec ',min(dates),max(dates)
EKGWSPEC=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/FNT_CO2x1SmidEmin_yBWCN_vE.sav
print,'fnt ',min(dates),max(dates)
fnt=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/ICEFRAC_CO2x1SmidEmin_yBWCN_vE.sav
print,'icefrac ',min(dates),max(dates)
ICEFRAC=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/NO2_CO2x1SmidEmin_yBWCN_vE.sav
print,'no2 ',min(dates),max(dates)
no2=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/NO_CO2x1SmidEmin_yBWCN_vE.sav
print,'no ',min(dates),max(dates)
no=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/NOx_CO2x1SmidEmin_yBWCN_vE.sav
print,'nox ',min(dates),max(dates)
nox=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/NOy_CO2x1SmidEmin_yBWCN_vE.sav
print,'noy ',min(dates),max(dates)
noy=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/O3_CO2x1SmidEmin_yBWCN_vE.sav
print,'o3 ',min(dates),max(dates)
o3=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/OMEGA_CO2x1SmidEmin_yBWCN_vE.sav
print,'omega ',min(dates),max(dates)
omega=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/PRECT_CO2x1SmidEmin_yBWCN_vE.sav
print,'prect ',min(dates),max(dates)
PRECT=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/PSL_CO2x1SmidEmin_yBWCN_vE.sav
print,'psl ',min(dates),max(dates)
psl=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/PTTEND_CO2x1SmidEmin_yBWCN_vE.sav
print,'pttend ',min(dates),max(dates)
pttend=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/P_CO2x1SmidEmin_yBWCN_vE.sav
print,'p ',min(dates),max(dates)
p=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QCP_CO2x1SmidEmin_yBWCN_vE.sav
print,'qcp ',min(dates),max(dates)
qcp=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QJOULE_CO2x1SmidEmin_yBWCN_vE.sav
print,'qjoule ',min(dates),max(dates)
qjoule=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QNO_CO2x1SmidEmin_yBWCN_vE.sav
print,'qno ',min(dates),max(dates)
qno=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRLNLTE_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrlnlte ',min(dates),max(dates)
QRLNLTE=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRL_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrl ',min(dates),max(dates)
qrl=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_AUR_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_aur ',min(dates),max(dates)
qrs_aur=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_CO2NIR_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_co2nir ',min(dates),max(dates)
qrs_co2nir=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs ',min(dates),max(dates)
QRS=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_EUV_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_euv ',min(dates),max(dates)
QRS_EUV=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_LO3A_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_lo3a ',min(dates),max(dates)
QRS_LO3A=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_LO3B_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_lo3b ',min(dates),max(dates)
QRS_LO3B=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_LO3_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_lo3 ',min(dates),max(dates)
QRS_LO3=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_SO2A_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_so2a ',min(dates),max(dates)
QRS_SO2A=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_SO2B_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_so2b ',min(dates),max(dates)
QRS_SO2B=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_SO3A_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_so3a ',min(dates),max(dates)
QRS_SO3A=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QRS_SO3B_CO2x1SmidEmin_yBWCN_vE.sav
print,'qrs_so3b ',min(dates),max(dates)
QRS_SO3B=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QSUM_CO2x1SmidEmin_yBWCN_vE.sav
print,'qsum ',min(dates),max(dates)
qsum=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/QTHERMAL_CO2x1SmidEmin_yBWCN_vE.sav
print,'qthermal ',min(dates),max(dates)
qthermal=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/T200_CO2x1SmidEmin_yBWCN_vE.sav
print,'t200 ',min(dates),max(dates)
t200=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/T300_CO2x1SmidEmin_yBWCN_vE.sav
print,'t300 ',min(dates),max(dates)
t300=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/T500_CO2x1SmidEmin_yBWCN_vE.sav
print,'t500 ',min(dates),max(dates)
t500=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/T850_CO2x1SmidEmin_yBWCN_vE.sav
print,'t850 ',min(dates),max(dates)
t850=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/TAUX_CO2x1SmidEmin_yBWCN_vE.sav
print,'taux ',min(dates),max(dates)
taux=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/TAUY_CO2x1SmidEmin_yBWCN_vE.sav
print,'tauy ',min(dates),max(dates)
tauy=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/TS_CO2x1SmidEmin_yBWCN_vE.sav
print,'ts ',min(dates),max(dates)
ts=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/TTEND_CO2x1SmidEmin_yBWCN_vE.sav
print,'ttend ',min(dates),max(dates)
ttend=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/TTGW_CO2x1SmidEmin_yBWCN_vE.sav
print,'ttgw ',min(dates),max(dates)
ttgw=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/T_CO2x1SmidEmin_yBWCN_vE.sav
print,'t ',min(dates),max(dates)
t=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/U200_CO2x1SmidEmin_yBWCN_vE.sav
print,'u200 ',min(dates),max(dates)
u200=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/U850_CO2x1SmidEmin_yBWCN_vE.sav
print,'u850 ',min(dates),max(dates)
u850=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/UBOT_CO2x1SmidEmin_yBWCN_vE.sav
print,'ubot ',min(dates),max(dates)
ubot=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/UTGWSPEC_CO2x1SmidEmin_yBWCN_vE.sav
print,'utgwspec ',min(dates),max(dates)
utgwspec=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/U_CO2x1SmidEmin_yBWCN_vE.sav
print,'u ',min(dates),max(dates)
u=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/V200_CO2x1SmidEmin_yBWCN_vE.sav
print,'v200 ',min(dates),max(dates)
v200=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/V850_CO2x1SmidEmin_yBWCN_vE.sav
print,'v850 ',min(dates),max(dates)
v850=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/VBOT_CO2x1SmidEmin_yBWCN_vE.sav
print,'vbot ',min(dates),max(dates)
vbot=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/V_CO2x1SmidEmin_yBWCN_vE.sav
print,'v ',min(dates),max(dates)
v=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z050_CO2x1SmidEmin_yBWCN_vE.sav
print,'z050 ',min(dates),max(dates)
z050=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z100_CO2x1SmidEmin_yBWCN_vE.sav
print,'z100 ',min(dates),max(dates)
z100=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z200_CO2x1SmidEmin_yBWCN_vE.sav
print,'z200 ',min(dates),max(dates)
z200=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z300_CO2x1SmidEmin_yBWCN_vE.sav
print,'z300 ',min(dates),max(dates)
z300=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z3_CO2x1SmidEmin_yBWCN_vE.sav
print,'z3 ',min(dates),max(dates)
z3=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z500_CO2x1SmidEmin_yBWCN_vE.sav
print,'z500 ',min(dates),max(dates)
z500=0
restore,'/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/Z700_CO2x1SmidEmin_yBWCN_vE.sav
print,'z700 ',min(dates),max(dates)
z700=0
stop

;for ifile=0L,nfile-1L do begin
;    restore,ifiles1(ifile)
;    print,ifiles1(ifile),min(dates),max(dates)
;    datesave=dates
;    restore,ifiles2(ifile)
;    print,ifiles2(ifile),min(dates),max(dates)
;endfor
;restore,dir+'P_CO2x1SmidEmin_yBWCN_vE.sav'
;sdates='2'+string(FORMAT='(i7.7)',long(DATES))	; for P, set2 dates range from 20010101 to 21510110, set2 dates range from 21501222 to 23001231
;lstmn=1 & lstdy=1 & lstyr=2001 & lstday=0
;ledmn=1 & leddy=10 & ledyr=2151 & ledday=0
;
;lstmn=12 & lstdy=22 & lstyr=2150 & lstday=0
;ledmn=12 & leddy=31 & ledyr=2300 & ledday=0     
;
;z = stddat(lstmn,lstdy,lstyr,lstday)
;z = stddat(ledmn,leddy,ledyr,ledday)
;if ledday lt lstday then stop,' Wrong dates! '
;iyr = lstyr
;idy = lstdy
;imn = lstmn
;z = kgmt(imn,idy,iyr,iday)
;iday = iday - 1
;;
;; --- Loop here --------
;;
;jump: iday = iday + 1
;      kdate,float(iday),iyr,imn,idy
;      ckday,iday,iyr
;;
;; test for end condition and close windows.
;;
;      z = stddat(imn,idy,iyr,ndays)
;      if ndays lt lstday then stop,'starting day outside range'
;      if ndays gt ledday then stop,'normal termination condition'
;      syr=string(FORMAT='(i4)',iyr)
;      sdy=string(FORMAT='(i2.2)',idy)
;      smn=string(FORMAT='(i2.2)',imn)
;      sdate=syr+smn+sdy
;      if smn+sdy eq '0229' then goto,jump
;;
;; find day
;;
;;     print,sdate
;      today=where(sdate eq sdates)
;      if today(0) eq -1L then print,'missing '+sdate
;goto,jump
end
