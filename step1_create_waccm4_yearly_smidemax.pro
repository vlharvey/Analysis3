;
; read WACCM4 CO2x1SmidEmax_yBWCN set1 (and then set2) data and save yearly files
; NOTE: save Year150 from set1 and save Year151 from set2
;
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set1
; /atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set2
;
idir='/atmos/pecked/WACCM/CO2x1SmidEmax_yBWCN/set2/'
dir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/'

ivars=[$
'Vstar',$
'Wstar']
;'CLONO2',$
;'CO',$
;'DTCORE',$
;'DTV',$
;'EKGWSPEC',$
;'NO2',$
;'NO',$
;'NOx',$
;'NOy',$
;'O3',$
;'OMEGA',$
;'PSL',$
;'PTTEND',$
;'P',$
;'QCP',$
;'QJOULE',$
;'QNO',$
;'QRLNLTE',$
;'QRL',$
;'QRS_AUR',$
;'QRS_CO2NIR',$
;'QRS',$
;'QRS_EUV',$
;'QRS_LO3A',$
;'QRS_LO3B',$
;'QRS_LO3',$
;'QRS_SO2A',$
;'QRS_SO2B',$
;'QRS_SO3A',$
;'QRS_SO3B',$
;'QSUM',$
;'QTHERMAL',$
;'TAUX',$
;'TAUY',$
;'TS',$
;'TTEND',$
;'TTGW',$
;'T',$
;'UTGWSPEC',$
;'U',$
;'V',$
;'Z3']

ifiles=idir+ivars+'_CO2x1SmidEmax_yBWCN_vE.sav'
nfiles=n_elements(ifiles)
for ii=0L,nfiles-1L do begin
    restore,ifiles(ii)
    lat=float(lat)
;   lev=float(lev)
    sdates=string(format='(i7.7)',dates)
    y=strmid(sdates,0,3)
    m=strmid(sdates,3,2)
    d=strmid(sdates,5,2)
;
; check size
;
if ivars(ii) eq 'CLONO2' then help,CLONO2
if ivars(ii) eq 'CO' then help,CO
if ivars(ii) eq 'DTCORE' then help,DTCORE
if ivars(ii) eq 'DTV' then help,DTV
if ivars(ii) eq 'EKGWSPEC' then help,EKGWSPEC
if ivars(ii) eq 'NO2' then help,NO2
if ivars(ii) eq 'NO' then help,NO
if ivars(ii) eq 'NOx' then help,NOx
if ivars(ii) eq 'NOy' then help,NOy
if ivars(ii) eq 'O3' then help,O3
if ivars(ii) eq 'OMEGA' then help,OMEGA
if ivars(ii) eq 'PSL' then help,PSL
if ivars(ii) eq 'PTTEND' then help,PTTEND
if ivars(ii) eq 'P' then help,P
if ivars(ii) eq 'QCP' then help,QCP
if ivars(ii) eq 'QJOULE' then help,QJOULE
if ivars(ii) eq 'QNO' then help,QNO
if ivars(ii) eq 'QRLNLTE' then help,QRLNLTE
if ivars(ii) eq 'QRL' then help,QRL
if ivars(ii) eq 'QRS_AUR' then help,QRS_AUR
if ivars(ii) eq 'QRS_CO2NIR' then help,QRS_CO2NIR
if ivars(ii) eq 'QRS' then help,QRS
if ivars(ii) eq 'QRS_EUV' then help,QRS_EUV
if ivars(ii) eq 'QRS_LO3A' then help,QRS_LO3A
if ivars(ii) eq 'QRS_LO3B' then help,QRS_LO3B
if ivars(ii) eq 'QRS_LO3' then help,QRS_LO3
if ivars(ii) eq 'QRS_SO2A' then help,QRS_SO2A
if ivars(ii) eq 'QRS_SO2B' then help,QRS_SO2B
if ivars(ii) eq 'QRS_SO3A' then help,QRS_SO3A
if ivars(ii) eq 'QRS_SO3B' then help,QRS_SO3B
if ivars(ii) eq 'QSUM' then help,QSUM
if ivars(ii) eq 'QTHERMAL' then help,QTHERMAL
if ivars(ii) eq 'TAUX' then help,TAUX
if ivars(ii) eq 'TAUY' then help,TAUY
if ivars(ii) eq 'TS' then help,TS
if ivars(ii) eq 'TTEND' then help,TTEND
if ivars(ii) eq 'TTGW' then help,TTGW
if ivars(ii) eq 'T' then help,T
if ivars(ii) eq 'UTGWSPEC' then help,UTGWSPEC
if ivars(ii) eq 'U' then help,U
if ivars(ii) eq 'V' then help,V
if ivars(ii) eq 'Z3' then help,Z3
if ivars(ii) eq 'Vstar' then help,vstar
if ivars(ii) eq 'Wstar' then help,wstar
;
; loop over years
;
    for iyr=min(long(y)),max(long(y)) do begin
        index=where(long(y) eq iyr)

sdate_all=m(index)+d(index)
if ivars(ii) eq 'CLONO2' then CLONO2_YEARLY=reform(CLONO2(*,*,index))
if ivars(ii) eq 'CLONO2' then save,CLONO2_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'CO' then CO_YEARLY=reform(CO(*,*,index))
if ivars(ii) eq 'CO' then save,CO_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'DTCORE' then DTCORE_YEARLY=reform(DTCORE(*,*,index))
if ivars(ii) eq 'DTCORE' then save,DTCORE_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'DTV' then DTV_YEARLY=reform(DTV(*,*,index))
if ivars(ii) eq 'DTV' then save,DTV_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'EKGWSPEC' then begin
   EKGWSPEC_YEARLY=reform(EKGWSPEC(*,*,index))
   ilev=float(ilev)
   save,EKGWSPEC_YEARLY,LAT,ILEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'NO2' then NO2_YEARLY=reform(NO2(*,*,index))
if ivars(ii) eq 'NO2' then save,NO2_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'NO' then NO_YEARLY=reform(NO(*,*,index))
if ivars(ii) eq 'NO' then save,NO_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'NOx' then NOx_YEARLY=reform(NOx(*,*,index))
if ivars(ii) eq 'NOx' then save,NOx_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'NOy' then NOy_YEARLY=reform(NOy(*,*,index))
if ivars(ii) eq 'NOy' then save,NOy_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'O3' then O3_YEARLY=reform(O3(*,*,index))
if ivars(ii) eq 'O3' then save,O3_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'OMEGA' then OMEGA_YEARLY=reform(OMEGA(*,*,index))
if ivars(ii) eq 'OMEGA' then save,OMEGA_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'PSL' then begin
   lon=float(lon)
   PSL_YEARLY=reform(PSL(*,*,index))
   save,PSL_YEARLY,LAT,LON,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'PTTEND' then PTTEND_YEARLY=reform(PTTEND(*,*,index))
if ivars(ii) eq 'PTTEND' then save,PTTEND_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'P' then P_YEARLY=reform(P(*,*,index))
if ivars(ii) eq 'P' then save,P_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QCP' then QCP_YEARLY=reform(QCP(*,*,index))
if ivars(ii) eq 'QCP' then save,QCP_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QJOULE' then QJOULE_YEARLY=reform(QJOULE(*,*,index))
if ivars(ii) eq 'QJOULE' then save,QJOULE_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QNO' then QNO_YEARLY=reform(QNO(*,*,index))
if ivars(ii) eq 'QNO' then save,QNO_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRLNLTE' then QRLNLTE_YEARLY=reform(QRLNLTE(*,*,index))
if ivars(ii) eq 'QRLNLTE' then save,QRLNLTE_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRL' then QRL_YEARLY=reform(QRL(*,*,index))
if ivars(ii) eq 'QRL' then save,QRL_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_AUR' then QRS_AUR_YEARLY=reform(QRS_AUR(*,*,index))
if ivars(ii) eq 'QRS_AUR' then save,QRS_AUR_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR_YEARLY=reform(QRS_CO2NIR(*,*,index))
if ivars(ii) eq 'QRS_CO2NIR' then save,QRS_CO2NIR_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS' then QRS_YEARLY=reform(QRS(*,*,index))
if ivars(ii) eq 'QRS' then save,QRS_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_EUV' then QRS_EUV_YEARLY=reform(QRS_EUV(*,*,index))
if ivars(ii) eq 'QRS_EUV' then save,QRS_EUV_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A_YEARLY=reform(QRS_LO3A(*,*,index))
if ivars(ii) eq 'QRS_LO3A' then save,QRS_LO3A_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B_YEARLY=reform(QRS_LO3B(*,*,index))
if ivars(ii) eq 'QRS_LO3B' then save,QRS_LO3B_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_LO3' then QRS_LO3_YEARLY=reform(QRS_LO3(*,*,index))
if ivars(ii) eq 'QRS_LO3' then save,QRS_LO3_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A_YEARLY=reform(QRS_SO2A(*,*,index))
if ivars(ii) eq 'QRS_SO2A' then save,QRS_SO2A_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B_YEARLY=reform(QRS_SO2B(*,*,index))
if ivars(ii) eq 'QRS_SO2B' then save,QRS_SO2B_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A_YEARLY=reform(QRS_SO3A(*,*,index))
if ivars(ii) eq 'QRS_SO3A' then save,QRS_SO3A_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B_YEARLY=reform(QRS_SO3B(*,*,index))
if ivars(ii) eq 'QRS_SO3B' then save,QRS_SO3B_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QSUM' then QSUM_YEARLY=reform(QSUM(*,*,index))
if ivars(ii) eq 'QSUM' then save,QSUM_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'QTHERMAL' then QTHERMAL_YEARLY=reform(QTHERMAL(*,*,index))
if ivars(ii) eq 'QTHERMAL' then save,QTHERMAL_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'TAUX' then begin
   lon=float(lon)
   TAUX_YEARLY=reform(TAUX(*,*,index))
   save,TAUX_YEARLY,LAT,LON,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'TAUY' then begin
   lon=float(lon)
   TAUY_YEARLY=reform(TAUY(*,*,index))
   save,TAUY_YEARLY,LAT,LON,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'TS' then begin
   lon=float(lon)
   TS_YEARLY=reform(TS(*,*,index))
   save,TS_YEARLY,LAT,LON,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'TTEND' then TTEND_YEARLY=reform(TTEND(*,*,index))
if ivars(ii) eq 'TTEND' then save,TTEND_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'TTGW' then TTGW_YEARLY=reform(TTGW(*,*,index))
if ivars(ii) eq 'TTGW' then save,TTGW_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'T' then T_YEARLY=reform(T(*,*,index))
if ivars(ii) eq 'T' then save,T_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC_YEARLY=reform(UTGWSPEC(*,*,index))
if ivars(ii) eq 'UTGWSPEC' then save,UTGWSPEC_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'U' then U_YEARLY=reform(U(*,*,index))
if ivars(ii) eq 'U' then save,U_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'V' then V_YEARLY=reform(V(*,*,index))
if ivars(ii) eq 'V' then save,V_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
if ivars(ii) eq 'Z3' then Z3_YEARLY=reform(Z3(*,*,index))
if ivars(ii) eq 'Z3' then save,Z3_YEARLY,LAT,LEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'

if ivars(ii) eq 'Vstar' then begin
   VSTAR_YEARLY=reform(VSTAR(*,*,index))
   ilev=float(ilev)
   save,VSTAR_YEARLY,LAT,ILEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
if ivars(ii) eq 'Wstar' then begin
   WSTAR_YEARLY=reform(WSTAR(*,*,index))
   ilev=float(ilev)
   save,WSTAR_YEARLY,LAT,ILEV,SDATE_ALL,filename=dir+ivars(ii)+'_CO2x1SmidEmax_yBWCN_vE_Year'+y(index(0))+'.sav'
endif
    endfor	; loop over years
;
; clear memory
;
if ivars(ii) eq 'CLONO2' then CLONO2=0
if ivars(ii) eq 'CO' then CO=0
if ivars(ii) eq 'DTCORE' then DTCORE=0
if ivars(ii) eq 'DTV' then DTV=0
if ivars(ii) eq 'EKGWSPEC' then EKGWSPEC=0
if ivars(ii) eq 'NO2' then NO2=0
if ivars(ii) eq 'NO' then NO=0
if ivars(ii) eq 'NOx' then NOx=0
if ivars(ii) eq 'NOy' then NOy=0
if ivars(ii) eq 'O3' then O3=0
if ivars(ii) eq 'OMEGA' then OMEGA=0
if ivars(ii) eq 'PSL' then PSL=0
if ivars(ii) eq 'PTTEND' then PTTEND=0
if ivars(ii) eq 'P' then P=0
if ivars(ii) eq 'QCP' then QCP=0
if ivars(ii) eq 'QJOULE' then QJOULE=0
if ivars(ii) eq 'QNO' then QNO=0
if ivars(ii) eq 'QRLNLTE' then QRLNLTE=0
if ivars(ii) eq 'QRL' then QRL=0
if ivars(ii) eq 'QRS_AUR' then QRS_AUR=0
if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR=0
if ivars(ii) eq 'QRS' then QRS=0
if ivars(ii) eq 'QRS_EUV' then QRS_EUV=0
if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A=0
if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B=0
if ivars(ii) eq 'QRS_LO3' then QRS_LO3=0
if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A=0
if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B=0
if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A=0
if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B=0
if ivars(ii) eq 'QSUM' then QSUM=0
if ivars(ii) eq 'QTHERMAL' then QTHERMAL=0
if ivars(ii) eq 'TAUX' then TAUX=0
if ivars(ii) eq 'TAUY' then TAUY=0
if ivars(ii) eq 'TS' then TS=0
if ivars(ii) eq 'TTEND' then TTEND=0
if ivars(ii) eq 'TTGW' then TTGW=0
if ivars(ii) eq 'T' then T=0
if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC=0
if ivars(ii) eq 'U' then U=0
if ivars(ii) eq 'V' then V=0
if ivars(ii) eq 'Z3' then Z3=0
if ivars(ii) eq 'Vstar' then VSTAR=0
if ivars(ii) eq 'Wstar' then WSTAR=0
endfor		; loop over variables
end
