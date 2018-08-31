;
; read WACCM4 CO2x1SmidEmin_yBWCN set1 (and then set2) data and save yearly files
; NOTE: save Year150 from set1 and save Year151 from set2
;
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set1
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2
;
loadct,39
device,decompose=0
mcolor=255
nlvls=150
col1=(findgen(nlvls)/float(nlvls))*mcolor
!p.background=mcolor
dir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmin_yBWCN/'

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
for iyr=1,150 do begin		; all years from set1
    syr=string(format='(i3.3)',iyr)
    ifiles=dir+ivars+'_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav'		; filenames for all species each year
    nfiles=n_elements(ifiles)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles(ii)
        restore,ifiles(ii)
        if ivars(ii) eq 'CLONO2' then CLONO2=CLONO2_YEARLY
        if ivars(ii) eq 'CO' then CO=CO_YEARLY
        if ivars(ii) eq 'DTCORE' then DTCORE=DTCORE_YEARLY
        if ivars(ii) eq 'DTV' then DTV=DTV_YEARLY
        if ivars(ii) eq 'EKGWSPEC' then EKGWSPEC=EKGWSPEC_YEARLY
        if ivars(ii) eq 'NO2' then NO2=NO2_YEARLY
        if ivars(ii) eq 'NO' then NO=NO_YEARLY
        if ivars(ii) eq 'NOx' then NOx=NOx_YEARLY
        if ivars(ii) eq 'NOy' then NOy=NOy_YEARLY
        if ivars(ii) eq 'O3' then O3=O3_YEARLY
        if ivars(ii) eq 'OMEGA' then OMEGA=OMEGA_YEARLY
        if ivars(ii) eq 'PSL' then PSL=PSL_YEARLY
        if ivars(ii) eq 'PTTEND' then PTTEND=PTTEND_YEARLY
        if ivars(ii) eq 'P' then P=P_YEARLY
        if ivars(ii) eq 'QCP' then QCP=QCP_YEARLY
        if ivars(ii) eq 'QJOULE' then QJOULE=QJOULE_YEARLY
        if ivars(ii) eq 'QNO' then QNO=QNO_YEARLY
        if ivars(ii) eq 'QRLNLTE' then QRLNLTE=QRLNLTE_YEARLY
        if ivars(ii) eq 'QRL' then QRL=QRL_YEARLY
        if ivars(ii) eq 'QRS_AUR' then QRS_AUR=QRS_AUR_YEARLY
        if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR=QRS_CO2NIR_YEARLY
        if ivars(ii) eq 'QRS' then QRS=QRS_YEARLY
        if ivars(ii) eq 'QRS_EUV' then QRS_EUV=QRS_EUV_YEARLY
        if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A=QRS_LO3A_YEARLY
        if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B=QRS_LO3B_YEARLY
        if ivars(ii) eq 'QRS_LO3' then QRS_LO3=QRS_LO3_YEARLY
        if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A=QRS_SO2A_YEARLY
        if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B=QRS_SO2B_YEARLY
        if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A=QRS_SO3A_YEARLY
        if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B=QRS_SO3B_YEARLY
        if ivars(ii) eq 'QSUM' then QSUM=QSUM_YEARLY
        if ivars(ii) eq 'QTHERMAL' then QTHERMAL=QTHERMAL_YEARLY
        if ivars(ii) eq 'TAUX' then TAUX=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS=TS_YEARLY
        if ivars(ii) eq 'TTEND' then TTEND=TTEND_YEARLY
        if ivars(ii) eq 'TTGW' then TTGW=TTGW_YEARLY
        if ivars(ii) eq 'T' then T=T_YEARLY
        if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC=UTGWSPEC_YEARLY
        if ivars(ii) eq 'U' then U=U_YEARLY
        if ivars(ii) eq 'V' then V=V_YEARLY
        if ivars(ii) eq 'Z3' then Z3=Z3_YEARLY
        if ivars(ii) eq 'Vstar' then VSTAR=VSTAR_YEARLY
        if ivars(ii) eq 'Wstar' then WSTAR=WSTAR_YEARLY
    endfor
;
; save yearly file
;
    save,filename=dir+'ALLVARS_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav',LAT,LEV,ILEV,LON,SDATE_ALL,$
         CLONO2,CO,DTCORE,DTV,EKGWSPEC,NO2,NO,NOx,NOy,O3,OMEGA,PSL,PTTEND,P,QCP,QJOULE,QNO,QRLNLTE,QRL,$
         QRS_AUR,QRS_CO2NIR,QRS,QRS_EUV,QRS_LO3A,QRS_LO3B,QRS_LO3,QRS_SO2A,QRS_SO2B,QRS_SO3A,$
         QRS_SO3B,QSUM,QTHERMAL,TAUX,TAUY,TS,TTEND,TTGW,T,UTGWSPEC,U,V,Z3,VSTAR,WSTAR
;erase
;!type=2^2+2^3
;plot,u(80,36,*),ytitle='Ubar at 60N and 10hPa',yrange=[-50,100],color=0,charsize=2,title='FR-WACCM CO2x1SmidEmin_yBWCN_vE set2'
;oplot,u(80,36,*),thick=3,color=col1(iyr-1)
endfor		; loop over years
;
; second 150 years
;
for iyr=152,300 do begin               ; all years from set2 (do Year 151 separately)
    syr=string(format='(i3.3)',iyr)
    ifiles=dir+ivars+'_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav'          ; filenames for all species each year
    nfiles=n_elements(ifiles)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles(ii)
        restore,ifiles(ii)
        if ivars(ii) eq 'CLONO2' then CLONO2=CLONO2_YEARLY
        if ivars(ii) eq 'CO' then CO=CO_YEARLY
        if ivars(ii) eq 'DTCORE' then DTCORE=DTCORE_YEARLY
        if ivars(ii) eq 'DTV' then DTV=DTV_YEARLY
        if ivars(ii) eq 'EKGWSPEC' then EKGWSPEC=EKGWSPEC_YEARLY
        if ivars(ii) eq 'NO2' then NO2=NO2_YEARLY
        if ivars(ii) eq 'NO' then NO=NO_YEARLY
        if ivars(ii) eq 'NOx' then NOx=NOx_YEARLY
        if ivars(ii) eq 'NOy' then NOy=NOy_YEARLY
        if ivars(ii) eq 'O3' then O3=O3_YEARLY
        if ivars(ii) eq 'OMEGA' then OMEGA=OMEGA_YEARLY
        if ivars(ii) eq 'PSL' then PSL=PSL_YEARLY
        if ivars(ii) eq 'PTTEND' then PTTEND=PTTEND_YEARLY
        if ivars(ii) eq 'P' then P=P_YEARLY
        if ivars(ii) eq 'QCP' then QCP=QCP_YEARLY
        if ivars(ii) eq 'QJOULE' then QJOULE=QJOULE_YEARLY
        if ivars(ii) eq 'QNO' then QNO=QNO_YEARLY
        if ivars(ii) eq 'QRLNLTE' then QRLNLTE=QRLNLTE_YEARLY
        if ivars(ii) eq 'QRL' then QRL=QRL_YEARLY
        if ivars(ii) eq 'QRS_AUR' then QRS_AUR=QRS_AUR_YEARLY
        if ivars(ii) eq 'QRS_CO2NIR' then QRS_CO2NIR=QRS_CO2NIR_YEARLY
        if ivars(ii) eq 'QRS' then QRS=QRS_YEARLY
        if ivars(ii) eq 'QRS_EUV' then QRS_EUV=QRS_EUV_YEARLY
        if ivars(ii) eq 'QRS_LO3A' then QRS_LO3A=QRS_LO3A_YEARLY
        if ivars(ii) eq 'QRS_LO3B' then QRS_LO3B=QRS_LO3B_YEARLY
        if ivars(ii) eq 'QRS_LO3' then QRS_LO3=QRS_LO3_YEARLY
        if ivars(ii) eq 'QRS_SO2A' then QRS_SO2A=QRS_SO2A_YEARLY
        if ivars(ii) eq 'QRS_SO2B' then QRS_SO2B=QRS_SO2B_YEARLY
        if ivars(ii) eq 'QRS_SO3A' then QRS_SO3A=QRS_SO3A_YEARLY
        if ivars(ii) eq 'QRS_SO3B' then QRS_SO3B=QRS_SO3B_YEARLY
        if ivars(ii) eq 'QSUM' then QSUM=QSUM_YEARLY
        if ivars(ii) eq 'QTHERMAL' then QTHERMAL=QTHERMAL_YEARLY
        if ivars(ii) eq 'TAUX' then TAUX=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS=TS_YEARLY
        if ivars(ii) eq 'TTEND' then TTEND=TTEND_YEARLY
        if ivars(ii) eq 'TTGW' then TTGW=TTGW_YEARLY
        if ivars(ii) eq 'T' then T=T_YEARLY
        if ivars(ii) eq 'UTGWSPEC' then UTGWSPEC=UTGWSPEC_YEARLY
        if ivars(ii) eq 'U' then U=U_YEARLY
        if ivars(ii) eq 'V' then V=V_YEARLY
        if ivars(ii) eq 'Z3' then Z3=Z3_YEARLY
        if ivars(ii) eq 'Vstar' then VSTAR=VSTAR_YEARLY
        if ivars(ii) eq 'Wstar' then WSTAR=WSTAR_YEARLY
    endfor
;
; save yearly file
;
    save,filename=dir+'ALLVARS_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav',LAT,LEV,ILEV,LON,SDATE_ALL,$
         CLONO2,CO,DTCORE,DTV,EKGWSPEC,NO2,NO,NOx,NOy,O3,OMEGA,PSL,PTTEND,P,QCP,QJOULE,QNO,QRLNLTE,QRL,$
         QRS_AUR,QRS_CO2NIR,QRS,QRS_EUV,QRS_LO3A,QRS_LO3B,QRS_LO3,QRS_SO2A,QRS_SO2B,QRS_SO3A,$
         QRS_SO3B,QSUM,QTHERMAL,TAUX,TAUY,TS,TTEND,TTGW,T,UTGWSPEC,U,V,Z3,VSTAR,WSTAR
;erase
;!type=2^2+2^3
;plot,u(80,36,*),ytitle='Ubar at 60N and 10hPa',yrange=[-50,100],color=0,charsize=2,title='FR-WACCM CO2x1SmidEmin_yBWCN_vE set2'
;oplot,u(80,36,*),thick=3,color=col1(iyr-1)
endfor          ; loop over years

end
