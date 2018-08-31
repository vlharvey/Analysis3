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
'PSL',$
'TAUX',$
'TAUY',$
'TS',$
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
        if ivars(ii) eq 'PSL' then PSL_set1=PSL_YEARLY
        if ivars(ii) eq 'PSL' then sdate_set1=sdate_all
        if ivars(ii) eq 'TAUX' then TAUX_set1=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY_set1=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS_set1=TS_YEARLY
        if ivars(ii) eq 'Vstar' then vstar_set1=vstar_YEARLY
        if ivars(ii) eq 'Wstar' then wstar_set1=wstar_YEARLY
help,'set1 ',ivars(ii),sdate_all
    endfor

    ifiles=dir+ivars+'_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'.sav'          ; most variables have full Year 151 but some start on day 11
    nfiles=n_elements(ifiles)
    for ii=0L,nfiles-1L do begin
        print,'restoring '+ifiles(ii)
        restore,ifiles(ii)
        if ivars(ii) eq 'PSL' then PSL_set2=PSL_YEARLY
        if ivars(ii) eq 'PSL' then sdate_set2=sdate_all
        if ivars(ii) eq 'TAUX' then TAUX_set2=TAUX_YEARLY
        if ivars(ii) eq 'TAUY' then TAUY_set2=TAUY_YEARLY
        if ivars(ii) eq 'TS' then TS_set2=TS_YEARLY
        if ivars(ii) eq 'Vstar' then vstar_set2=vstar_YEARLY
        if ivars(ii) eq 'Wstar' then wstar_set2=wstar_YEARLY
help,'set2 ',ivars(ii),sdate_all
    endfor
;
; PSL set2 starts on January 11th
;
startset2=where(sdate_set2 eq '0111')
psl=[[[psl_set1[*,*,0:-1]]],[[psl_set2[*,*,startSet2:-1]]]]
sdate_all=[sdate_set1,sdate_set2]
psl_yearly=psl
save,filename=dir+'PSL_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',LON,LAT,PSL_YEARLY,sdate_all
;
; TAUX, TAUY, TS set2 starts on January 11th
;
TAUX=[[[TAUX_set1[*,*,0:-1]]],[[TAUX_set2[*,*,startSet2:-1]]]]
TAUY=[[[TAUY_set1[*,*,0:-1]]],[[TAUY_set2[*,*,startSet2:-1]]]]
TS=[[[TS_set1[*,*,0:-1]]],[[TS_set2[*,*,startSet2:-1]]]]
taux_yearly=taux
tauy_yearly=tauy
ts_yearly=ts
save,filename=dir+'TAUX_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',LON,LAT,TAUX_YEARLY,sdate_all
save,filename=dir+'TAUY_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',LON,LAT,TAUY_YEARLY,sdate_all
save,filename=dir+'TS_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',LON,LAT,TS_YEARLY,sdate_all
;
; vstar and wstar set2 starts on January 11th
;
VSTAR=[[[VSTAR_set1[*,*,0:-1]]],[[VSTAR_set2[*,*,startSet2:-1]]]]
WSTAR=[[[WSTAR_set1[*,*,0:-1]]],[[WSTAR_set2[*,*,startSet2:-1]]]]
VSTAR_yearly=VSTAR
WSTAR_yearly=WSTAR
save,filename=dir+'Vstar_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',ILEV,LAT,VSTAR_YEARLY,sdate_all
save,filename=dir+'Wstar_CO2x1SmidEmax_yBWCN_vE_Year'+syr+'-good.sav',ILEV,LAT,WSTAR_YEARLY,sdate_all

erase
!type=2^2+2^3
plot,100.*reform(mean(wstar_yearly(80:-1,20,*),dim=1)),ytitle='Wstar (cm/s) at 60N and 0.01 hPa',yrange=[-5,5],color=0,charsize=2,title='FR-WACCM CO2x1SmidEmax_yBWCN_vE Year 151',thick=3

;endfor		; loop over years
end
