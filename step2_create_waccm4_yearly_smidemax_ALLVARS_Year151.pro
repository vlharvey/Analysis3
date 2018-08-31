;
; read WACCM4 CO2x1SmidEmax_yBWCN set1 (and then set2) data and save yearly files
; NOTE: Year151 is special overlap year. get first 10 days from set1 and the remainder from set2
;
loadct,39
device,decompose=0
mcolor=255
nlvls=150
col1=(findgen(nlvls)/float(nlvls))*mcolor
!p.background=mcolor
dir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/'

ivars=[$
'CLONO2',$
'CO',$
'DTCORE',$
'DTV',$
'EKGWSPEC',$
'NO2',$
'NO',$
'NOx',$
'NOy',$
'O3',$
'OMEGA',$
'PSL',$
'PTTEND',$
'P',$
'QCP',$
'QJOULE',$
'QNO',$
'QRLNLTE',$
'QRL',$
'QRS_AUR',$
'QRS_CO2NIR',$
'QRS',$
'QRS_EUV',$
'QRS_LO3A',$
'QRS_LO3B',$
'QRS_LO3',$
'QRS_SO2A',$
'QRS_SO2B',$
'QRS_SO3A',$
'QRS_SO3B',$
'QSUM',$
'QTHERMAL',$
'TAUX',$
'TAUY',$
'TS',$
'TTEND',$
'TTGW',$
'T',$
'UTGWSPEC',$
'U',$
'V',$
'Z3',$
'Vstar',$
'Wstar']
;
; loop over years
;
;for iyr=151,151 do begin		; all years from set1
iyr=151L
    syr=string(format='(i3.3)',iyr)
    ifiles1=dir+ivars+'_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-set1.sav'		; first 10 days of Year 151
    nfiles=n_elements(ifiles1)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles1(ii)
        restore,ifiles1(ii)
        if ivars(ii) eq 'CLONO2' then CLONO2_set1=CLONO2_YEARLY
        if ivars(ii) eq 'CO' then CO_set1=CO_YEARLY
        if ivars(ii) eq 'DTCORE' then DTCORE_set1=DTCORE_YEARLY
        if ivars(ii) eq 'DTV' then DTV_set1=DTV_YEARLY
        if ivars(ii) eq 'EKGWSPEC' then EKGWSPEC_set1=EKGWSPEC_YEARLY
        if ivars(ii) eq 'NO2' then NO2_set1=NO2_YEARLY
        if ivars(ii) eq 'NO' then NO_set1=NO_YEARLY
        if ivars(ii) eq 'NOx' then NOx_set1=NOx_YEARLY
        if ivars(ii) eq 'NOy' then NOy_set1=NOy_YEARLY
        if ivars(ii) eq 'O3' then O3_set1=O3_YEARLY
        if ivars(ii) eq 'OMEGA' then OMEGA_set1=OMEGA_YEARLY
        if ivars(ii) eq 'PSL' then PSL_set1=PSL_YEARLY
        if ivars(ii) eq 'PSL' then sdate_set1=sdate_all
        if ivars(ii) eq 'PTTEND' then PTTEND_set1=PTTEND_YEARLY
        if ivars(ii) eq 'P' then P_set1=P_YEARLY
        if ivars(ii) eq 'QCP' then QCP_set1=QCP_YEARLY
        if ivars(ii) eq 'QJOULE' then QJOULE_set1=QJOULE_YEARLY
        if ivars(ii) eq 'QNO' then QNO_set1=QNO_YEARLY
        if ivars(ii) eq 'QRLNLTE' then QRLNLTE_set1=QRLNLTE_YEARLY
        if ivars(ii) eq 'QRL' then QRL_set1=QRL_YEARLY
        if ivars(ii) eq 'QRS_AUR' then QRS_AUR_set1=QRS_AUR_YEARLY
        if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR_set1=QRS_CO2NIR_YEARLY
        if ivars(ii) eq 'QRS' then QRS_set1=QRS_YEARLY
        if ivars(ii) eq 'QRS_EUV' then QRS_EUV_set1=QRS_EUV_YEARLY
        if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A_set1=QRS_LO3A_YEARLY
        if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B_set1=QRS_LO3B_YEARLY
        if ivars(ii) eq 'QRS_LO3' then QRS_LO3_set1=QRS_LO3_YEARLY
        if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A_set1=QRS_SO2A_YEARLY
        if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B_set1=QRS_SO2B_YEARLY
        if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A_set1=QRS_SO3A_YEARLY
        if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B_set1=QRS_SO3B_YEARLY
        if ivars(ii) eq 'QSUM' then QSUM_set1=QSUM_YEARLY
        if ivars(ii) eq 'QTHERMAL' then QTHERMAL_set1=QTHERMAL_YEARLY
        if ivars(ii) eq 'TAUX' then TAUX_set1=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY_set1=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS_set1=TS_YEARLY
        if ivars(ii) eq 'TTEND' then TTEND_set1=TTEND_YEARLY
        if ivars(ii) eq 'TTGW' then TTGW_set1=TTGW_YEARLY
        if ivars(ii) eq 'T' then T_set1=T_YEARLY
        if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC_set1=UTGWSPEC_YEARLY
        if ivars(ii) eq 'U' then U_set1=U_YEARLY
        if ivars(ii) eq 'V' then V_set1=V_YEARLY
        if ivars(ii) eq 'Z3' then Z3_set1=Z3_YEARLY
        if ivars(ii) eq 'Vstar' then vstar_set1=vstar_YEARLY
        if ivars(ii) eq 'Wstar' then wstar_set1=wstar_YEARLY
help,'set1 ',ivars(ii),sdate_all
    endfor

    ifiles=dir+ivars+'_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'          ; most variables have full Year 151 but some start on day 11
    nfiles=n_elements(ifiles)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles(ii)
        restore,ifiles(ii)
        if ivars(ii) eq 'CLONO2' then CLONO2_set2=CLONO2_YEARLY
        if ivars(ii) eq 'CO' then CO_set2=CO_YEARLY
        if ivars(ii) eq 'DTCORE' then DTCORE_set2=DTCORE_YEARLY
        if ivars(ii) eq 'DTV' then DTV_set2=DTV_YEARLY
        if ivars(ii) eq 'EKGWSPEC' then EKGWSPEC_set2=EKGWSPEC_YEARLY
        if ivars(ii) eq 'NO2' then NO2_set2=NO2_YEARLY
        if ivars(ii) eq 'NO' then NO_set2=NO_YEARLY
        if ivars(ii) eq 'NOx' then NOx_set2=NOx_YEARLY
        if ivars(ii) eq 'NOy' then NOy_set2=NOy_YEARLY
        if ivars(ii) eq 'O3' then O3_set2=O3_YEARLY
        if ivars(ii) eq 'OMEGA' then OMEGA_set2=OMEGA_YEARLY
        if ivars(ii) eq 'PSL' then PSL_set2=PSL_YEARLY
        if ivars(ii) eq 'PSL' then sdate_set2=sdate_all
        if ivars(ii) eq 'PTTEND' then PTTEND_set2=PTTEND_YEARLY
        if ivars(ii) eq 'P' then P_set2=P_YEARLY
        if ivars(ii) eq 'QCP' then QCP_set2=QCP_YEARLY
        if ivars(ii) eq 'QJOULE' then QJOULE_set2=QJOULE_YEARLY
        if ivars(ii) eq 'QNO' then QNO_set2=QNO_YEARLY
        if ivars(ii) eq 'QRLNLTE' then QRLNLTE_set2=QRLNLTE_YEARLY
        if ivars(ii) eq 'QRL' then QRL_set2=QRL_YEARLY
        if ivars(ii) eq 'QRS_AUR' then QRS_AUR_set2=QRS_AUR_YEARLY
        if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR_set2=QRS_CO2NIR_YEARLY
        if ivars(ii) eq 'QRS' then QRS_set2=QRS_YEARLY
        if ivars(ii) eq 'QRS_EUV' then QRS_EUV_set2=QRS_EUV_YEARLY
        if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A_set2=QRS_LO3A_YEARLY
        if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B_set2=QRS_LO3B_YEARLY
        if ivars(ii) eq 'QRS_LO3' then QRS_LO3_set2=QRS_LO3_YEARLY
        if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A_set2=QRS_SO2A_YEARLY
        if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B_set2=QRS_SO2B_YEARLY
        if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A_set2=QRS_SO3A_YEARLY
        if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B_set2=QRS_SO3B_YEARLY
        if ivars(ii) eq 'QSUM' then QSUM_set2=QSUM_YEARLY
        if ivars(ii) eq 'QTHERMAL' then QTHERMAL_set2=QTHERMAL_YEARLY
        if ivars(ii) eq 'TAUX' then TAUX_set2=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY_set2=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS_set2=TS_YEARLY
        if ivars(ii) eq 'TTEND' then TTEND_set2=TTEND_YEARLY
        if ivars(ii) eq 'TTGW' then TTGW_set2=TTGW_YEARLY
        if ivars(ii) eq 'T' then T_set2=T_YEARLY
        if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC_set2=UTGWSPEC_YEARLY
        if ivars(ii) eq 'U' then U_set2=U_YEARLY
        if ivars(ii) eq 'V' then V_set2=V_YEARLY
        if ivars(ii) eq 'Z3' then Z3_set2=Z3_YEARLY
        if ivars(ii) eq 'Vstar' then vstar_set2=vstar_YEARLY
        if ivars(ii) eq 'Wstar' then wstar_set2=wstar_YEARLY
help,'set2 ',ivars(ii),sdate_all
    endfor

CLONO2=CLONO2_set2
CO=CO_set2
DTCORE=DTCORE_set2
DTV=DTV_set2
EKGWSPEC=EKGWSPEC_set2
NO2=NO2_set2
NO=NO_set2
NOx=NOx_set2
NOy=NOy_set2
O3=O3_set2
OMEGA=OMEGA_set2
;
; PSL set2 starts on January 11th
;
startset2=where(sdate_set2 eq '0111')
psl=[[[psl_set1[*,*,0:-1]]],[[psl_set2[*,*,startSet2:-1]]]]
sdate_all=[sdate_set1,sdate_set2]

PTTEND=PTTEND_set2
P=P_set2
QCP=QCP_set2
QJOULE=QJOULE_set2
QNO=QNO_set2
QRLNLTE=QRLNLTE_set2
QRL=QRL_set2
QRS_AUR=QRS_AUR_set2
QRS_CO2NIR=QRS_CO2NIR_set2
QRS=QRS_set2
QRS_EUV=QRS_EUV_set2
QRS_LO3A=QRS_LO3A_set2
QRS_LO3B=QRS_LO3B_set2
QRS_LO3=QRS_LO3_set2
QRS_SO2A=QRS_SO2A_set2
QRS_SO2B=QRS_SO2B_set2
QRS_SO3A=QRS_SO3A_set2
QRS_SO3B=QRS_SO3B_set2
QSUM=QSUM_set2
QTHERMAL=QTHERMAL_set2
;
; TAUX, TAUY, TS set2 starts on January 11th
;
TAUX=[[[TAUX_set1[*,*,0:-1]]],[[TAUX_set2[*,*,startSet2:-1]]]]
TAUY=[[[TAUY_set1[*,*,0:-1]]],[[TAUY_set2[*,*,startSet2:-1]]]]
TS=[[[TS_set1[*,*,0:-1]]],[[TS_set2[*,*,startSet2:-1]]]]

TTEND=TTEND_set2
TTGW=TTGW_set2
T=T_set2
UTGWSPEC=UTGWSPEC_set2
U=U_set2
V=V_set2
Z3=Z3_set2
;
; vstar and wstar set2 starts on January 11th
;
VSTAR=[[[VSTAR_set1[*,*,0:-1]]],[[VSTAR_set2[*,*,startSet2:-1]]]]
WSTAR=[[[WSTAR_set1[*,*,0:-1]]],[[WSTAR_set2[*,*,startSet2:-1]]]]

save,filename=dir+'ALLVARS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav',LAT,LEV,ILEV,LON,SDATE_ALL,$
     CLONO2,CO,DTCORE,DTV,EKGWSPEC,NO2,NO,NOx,NOy,O3,OMEGA,PSL,PTTEND,P,QCP,QJOULE,QNO,QRLNLTE,QRL,$
     QRS_AUR,QRS_CO2NIR,QRS,QRS_EUV,QRS_LO3A,QRS_LO3B,QRS_LO3,QRS_SO2A,QRS_SO2B,QRS_SO3A,$
     QRS_SO3B,QSUM,QTHERMAL,TAUX,TAUY,TS,TTEND,TTGW,T,UTGWSPEC,U,V,Z3,VSTAR,WSTAR

erase
!type=2^2+2^3
plot,u(80,36,*),ytitle='Ubar at 60N and 10hPa',yrange=[-50,100],color=0,charsize=2,title='FR-WACCM CO2x1SmidEmax_yBWCN_vE Year 151',thick=3

;endfor		; loop over years
end
