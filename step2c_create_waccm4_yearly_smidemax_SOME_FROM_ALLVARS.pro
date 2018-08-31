;
; for Randall Figure 3
; save a subset of variables in yearly files from ALLVARS yearly files
;
loadct,39
device,decompose=0
mcolor=255
nlvls=300
col1=(findgen(nlvls)/float(nlvls))*mcolor
!p.background=mcolor
dir='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/'
;
; loop over years
;
for iyr=1,300 do begin		; all years from set1
    syr=string(format='(i3.3)',iyr)
    ifiles=dir+'ALLVARS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'
    nfiles=n_elements(ifiles)
;
; restore yearly file
;
    restore,dir+'ALLVARS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'	;,LAT,LEV,ILEV,LON,SDATE_ALL,$
;         CLONO2,CO,DTCORE,DTV,EKGWSPEC,NO2,NO,NOx,NOy,O3,OMEGA,PSL,PTTEND,P,QCP,QJOULE,QNO,QRLNLTE,QRL,$
;         QRS_AUR,QRS_CO2NIR,QRS,QRS_EUV,QRS_LO3A,QRS_LO3B,QRS_LO3,QRS_SO2A,QRS_SO2B,QRS_SO3A,$
;         QRS_SO3B,QSUM,QTHERMAL,TAUX,TAUY,TS,TTEND,TTGW,T,UTGWSPEC,U,V,Z3,VSTAR,WSTAR
;
; save yearly file
; NOx, O3, T, ClONO2, Z3, U, WSTAR, QRS_LO3A, QRS_LO3B, QRS_SO3A, QRS_SO3B
;
    save,filename=dir+'SOMEVARS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav',LAT,LEV,ILEV,SDATE_ALL,$
         CLONO2,NOx,NOy,O3,QRS_LO3A,QRS_LO3B,QRS_SO3A,QRS_SO3B,T,U,Z3,WSTAR
print,'saved '+'SOMEVARS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'
if iyr eq 1 then begin
   erase
   !type=2^2+2^3
   plot,NOx(0,26,*)*1.e9,ytitle='NOx (ppbv) at 90S and 1hPa',yrange=[0,50],xtitle='DOY',color=0,charsize=2,title='FR-WACCM CO2x1SmidEmax_yBWCN_vE'
endif
oplot,NOx(0,26,*)*1.e9,thick=3,color=col1(iyr-1)

endfor          ; loop over years
end
