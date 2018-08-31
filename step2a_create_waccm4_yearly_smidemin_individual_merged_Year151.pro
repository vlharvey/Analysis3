;
; save out individual species for year 151 from previously merged ALLVARS file
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
;for iyr=151,151 do begin		; all years from set1
iyr=151L
    syr=string(format='(i3.3)',iyr)

restore,dir+'ALLVARS_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav'	;,LAT,LEV,ILEV,LON,SDATE_ALL,$
;     CLONO2,CO,DTCORE,DTV,EKGWSPEC,NO2,NO,NOx,NOy,O3,OMEGA,PSL,PTTEND,P,QCP,QJOULE,QNO,QRLNLTE,QRL,$
;     QRS_AUR,QRS_CO2NIR,QRS,QRS_EUV,QRS_LO3A,QRS_LO3B,QRS_LO3,QRS_SO2A,QRS_SO2B,QRS_SO3A,$
;     QRS_SO3B,QSUM,QTHERMAL,TAUX,TAUY,TS,TTEND,TTGW,T,UTGWSPEC,U,V,Z3,VSTAR,WSTAR

    ifiles1=dir+ivars+'_CO2x1SmidEmin_yBWCN_vE_Year'+syr+'.sav'		; individual variable filenames
    nfiles=n_elements(ifiles1)
    for ii=0L,nfiles-1L do begin
;       print,'restoring '+ifiles1(ii)(ii)
;       restore,ifiles1(ii)(ii)
        if ivars(ii) eq 'CLONO2' then begin
           CLONO2_YEARLY=CLONO2
           save,file=ifiles1(ii),lat,lev,sdate_all,CLONO2_YEARLY
        endif
        if ivars(ii) eq 'CO' then begin
           CO_YEARLY=CO
           save,file=ifiles1(ii),lat,lev,sdate_all,CO_YEARLY
        endif
        if ivars(ii) eq 'DTCORE' then begin
           DTCORE_YEARLY=DTCORE
           save,file=ifiles1(ii),lat,lev,sdate_all,DTCORE_YEARLY
        endif
        if ivars(ii) eq 'DTV' then begin
           DTV_YEARLY=DTV
           save,file=ifiles1(ii),lat,lev,sdate_all,DTV_YEARLY
        endif
        if ivars(ii) eq 'EKGWSPEC' then begin
           EKGWSPEC_YEARLY=EKGWSPEC
           save,file=ifiles1(ii),lat,ilev,sdate_all,EKGWSPEC_YEARLY
        endif
        if ivars(ii) eq 'NO2' then begin
           NO2_YEARLY=NO2
           save,file=ifiles1(ii),lat,lev,sdate_all,NO2_YEARLY
        endif
        if ivars(ii) eq 'NO' then begin
           NO_YEARLY=NO
           save,file=ifiles1(ii),lat,lev,sdate_all,NO_YEARLY
        endif
        if ivars(ii) eq 'NOx' then begin
           NOx_YEARLY=NOx
           save,file=ifiles1(ii),lat,lev,sdate_all,NOx_YEARLY
        endif
        if ivars(ii) eq 'NOy' then begin
           NOy_YEARLY=NOy
           save,file=ifiles1(ii),lat,lev,sdate_all,NOy_YEARLY
        endif
        if ivars(ii) eq 'O3' then begin
           O3_YEARLY=O3
           save,file=ifiles1(ii),lat,lev,sdate_all,O3_YEARLY
        endif
        if ivars(ii) eq 'OMEGA' then begin
           OMEGA_YEARLY=OMEGA
           save,file=ifiles1(ii),lat,lev,sdate_all,OMEGA_YEARLY
        endif
        if ivars(ii) eq 'PSL' then begin
           PSL_YEARLY=PSL
           save,file=ifiles1(ii),lat,lon,sdate_all,PSL_YEARLY
        endif
        if ivars(ii) eq 'PTTEND' then begin
           PTTEND_YEARLY=PTTEND
           save,file=ifiles1(ii),lat,lev,sdate_all,PTTEND_YEARLY
        endif
        if ivars(ii) eq 'P' then begin
           P_YEARLY=P
           save,file=ifiles1(ii),lat,lev,sdate_all,P_YEARLY
        endif
        if ivars(ii) eq 'QCP' then begin
           QCP_YEARLY=QCP
           save,file=ifiles1(ii),lat,lev,sdate_all,QCP_YEARLY
        endif
        if ivars(ii) eq 'QJOULE' then begin
           QJOULE_YEARLY=QJOULE
           save,file=ifiles1(ii),lat,lev,sdate_all,QJOULE_YEARLY
        endif
        if ivars(ii) eq 'QNO' then begin
           QNO_YEARLY=QNO
           save,file=ifiles1(ii),lat,lev,sdate_all,QNO_YEARLY
        endif
        if ivars(ii) eq 'QRLNLTE' then begin
           QRLNLTE_YEARLY=QRLNLTE
           save,file=ifiles1(ii),lat,lev,sdate_all,QRLNLTE_YEARLY
        endif
        if ivars(ii) eq 'QRL' then begin
           QRL_YEARLY=QRL
           save,file=ifiles1(ii),lat,lev,sdate_all,QRL_YEARLY
        endif
        if ivars(ii) eq 'QRS_AUR' then begin
           QRS_AUR_YEARLY=QRS_AUR
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_AUR_YEARLY
        endif
        if ivars(ii) eq 'QRS_CO2NIR' then begin
           QRS_CO2NIR_YEARLY=QRS_CO2NIR
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_CO2NIR_YEARLY
        endif
        if ivars(ii) eq 'QRS' then begin
           QRS_YEARLY=QRS
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_YEARLY
        endif
        if ivars(ii) eq 'QRS_EUV' then begin
           QRS_EUV_YEARLY=QRS_EUV
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_EUV_YEARLY
        endif
        if ivars(ii) eq 'QRS_LO3A' then begin
           QRS_LO3A_YEARLY=QRS_LO3A
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_LO3A_YEARLY
        endif
        if ivars(ii) eq 'QRS_LO3B' then begin
           QRS_LO3B_YEARLY=QRS_LO3B
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_LO3B_YEARLY
        endif
        if ivars(ii) eq 'QRS_LO3' then begin
           QRS_LO3_YEARLY=QRS_LO3
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_LO3_YEARLY
        endif
        if ivars(ii) eq 'QRS_SO2A' then begin
           QRS_SO2A_YEARLY=QRS_SO2A
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_SO2A_YEARLY
        endif
        if ivars(ii) eq 'QRS_SO2B' then begin
           QRS_SO2B_YEARLY=QRS_SO2B
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_SO2B_YEARLY
        endif
        if ivars(ii) eq 'QRS_SO3A' then begin
           QRS_SO3A_YEARLY=QRS_SO3A
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_SO3A_YEARLY
        endif
        if ivars(ii) eq 'QRS_SO3B' then begin
           QRS_SO3B_YEARLY=QRS_SO3B
           save,file=ifiles1(ii),lat,lev,sdate_all,QRS_SO3B_YEARLY
        endif
        if ivars(ii) eq 'QSUM' then begin
           QSUM_YEARLY=QSUM
           save,file=ifiles1(ii),lat,lev,sdate_all,QSUM_YEARLY
        endif
        if ivars(ii) eq 'QTHERMAL' then begin
           QTHERMAL_YEARLY=QTHERMAL
           save,file=ifiles1(ii),lat,lev,sdate_all,QTHERMAL_YEARLY
        endif
        if ivars(ii) eq 'TAUX' then begin
           TAUX_YEARLY=TAUX
           save,file=ifiles1(ii),lat,lon,sdate_all,TAUX_YEARLY
        endif
        if ivars(ii) eq 'TAUY' then begin
           TAUY_YEARLY=TAUY
           save,file=ifiles1(ii),lat,lon,sdate_all,TAUY_YEARLY
        endif
        if ivars(ii) eq 'TS' then begin
           TS_YEARLY=TS
           save,file=ifiles1(ii),lat,lon,sdate_all,TS_YEARLY
        endif
        if ivars(ii) eq 'TTEND' then begin
           TTEND_YEARLY=TTEND
           save,file=ifiles1(ii),lat,lev,sdate_all,TTEND_YEARLY
        endif
        if ivars(ii) eq 'TTGW' then begin
           TTGW_YEARLY=TTGW
           save,file=ifiles1(ii),lat,lev,sdate_all,TTGW_YEARLY
        endif
        if ivars(ii) eq 'T' then begin
           T_YEARLY=T
           save,file=ifiles1(ii),lat,lev,sdate_all,T_YEARLY
        endif
        if ivars(ii) eq 'UTGWSPEC' then begin
           UTGWSPEC_YEARLY=UTGWSPEC
           save,file=ifiles1(ii),lat,lev,sdate_all,UTGWSPEC_YEARLY
        endif
        if ivars(ii) eq 'U' then begin
           U_YEARLY=U
           save,file=ifiles1(ii),lat,lev,sdate_all,U_YEARLY
        endif
        if ivars(ii) eq 'V' then begin
           V_YEARLY=V
           save,file=ifiles1(ii),lat,lev,sdate_all,V_YEARLY
        endif
        if ivars(ii) eq 'Z3' then begin
           Z3_YEARLY=Z3
           save,file=ifiles1(ii),lat,lev,sdate_all,Z3_YEARLY
        endif
        if ivars(ii) eq 'Vstar' then begin
           vstar_YEARLY=vstar
           save,file=ifiles1(ii),lat,ilev,sdate_all,vstar_YEARLY
        endif
        if ivars(ii) eq 'Wstar' then begin
           wstar_YEARLY=wstar
           save,file=ifiles1(ii),lat,ilev,sdate_all,wstar_YEARLY
        endif
help,ivars(ii),sdate_all
    endfor
;
;endfor		; loop over years
end
