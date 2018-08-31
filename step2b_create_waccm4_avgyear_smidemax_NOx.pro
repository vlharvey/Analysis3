;
; create WACCM4 smidemax average year. Include averages and sigmas
; based on mee00 40-year code that read in daily zonal means. here the data is stored in yearly files.
; save NOx and Z3
; for Randall Figure 2
; 
loadct,39
device,decompose=0
mcolor=255
nlvls=30
col1=(findgen(nlvls)/float(nlvls))*mcolor
!p.background=mcolor
dirh='/Volumes/Data/WACCM/WACCM4/CO2x1SmidEmax_yBWCN/'

kday=365L
icount=0L
spawn,'ls '+dirh+'NOx_CO2x1SmidEmax_yBWCN_vE_Year???.sav',ifiles
spawn,'ls '+dirh+'Z3_CO2x1SmidEmax_yBWCN_vE_Year???.sav',zfiles
nyears=n_elements(ifiles)
;
; loop over years
;
for ifile=0L,nyears-1L do begin
;
; restore yearly file of daily zonal means of all variables
;
    restore,filename=ifiles(ifile)	;,LAT,LEV,NOX_YEARLY
    restore,filename=zfiles(ifile)	;,LAT,LEV,Z3_YEARLY
    print,'restored '+ifiles(ifile)
    nr=n_elements(lat)
    nl=n_elements(lev)
;
; declare yearly average and sigma arrays
;
    if ifile eq 0L then begin
       NOXsmidemax_avg=fltarr(nr,nl,kday)
       Z3smidemax_avg=fltarr(nr,nl,kday)

       NOXsmidemax_sig=fltarr(nr,nl,kday)
       Z3smidemax_sig=fltarr(nr,nl,kday)
;
; declare new "all" arrays that will contain all zonal mean data for NOx and Z
;
       NOXsmidemax_all=fltarr(nr,nl,kday,nyears)
       Z3smidemax_all=fltarr(nr,nl,kday,nyears)
    endif
;
; loop over years and retain all data
;
    NOXsmidemax_all(*,*,*,ifile)=NOX_YEARLY
    Z3smidemax_all(*,*,*,ifile)=Z3_YEARLY
endfor
;
; fill yearly average and sigma arrays
;
for iday=0L,kday-1L do begin
    for j=0L,nr-1L do begin
        for k=0L,nl-1L do begin
            NOXsmidemax_avg(j,k,iday)=mean(NOXsmidemax_all(j,k,iday,*))
            NOXsmidemax_sig(j,k,iday)=stdev(NOXsmidemax_all(j,k,iday,*))
    
            Z3smidemax_avg(j,k,iday)=mean(Z3smidemax_all(j,k,iday,*))
            Z3smidemax_sig(j,k,iday)=stdev(Z3smidemax_all(j,k,iday,*))
        endfor
    endfor
;erase
;!type=2^2+2^3
;level=min(NOXsmidemax_avg(*,*,iday)) + (( max(NOXsmidemax_avg(*,*,iday))-min(NOXsmidemax_avg(*,*,iday)))/float(nlvls))*findgen(nlvls)
;contour,reform(NOXsmidemax_avg(*,*,iday)),lat,reform(Z3smidemax_avg(*,*,iday))/1000.,levels=level,c_color=col1,/foll,color=0,ytitle='Altitude (km)',thick=3,title='NOx DOY= '+strcompress(iday+1)
;level=min(NOXsmidemax_sig(*,*,iday)) + (( max(NOXsmidemax_sig(*,*,iday))-min(NOXsmidemax_sig(*,*,iday)))/float(nlvls))*findgen(nlvls)
;contour,reform(NOXsmidemax_sig(*,*,iday)),lat,reform(Z3smidemax_avg(*,*,iday))/1000.,levels=level,c_color=col1,/foll,c_linestyle=5,/overplot,thick=2
endfor
;
; save yearly average and sigma file
;
saveit:
lat=float(lat)
p=float(lev)
ofile=dirh+'NOx_CO2x1SmidEmax_yBWCN_vE_AverageYear_VLH.sav'
save,file=ofile,lat,p,NOXsmidemax_avg,Z3smidemax_avg,NOXsmidemax_sig,Z3smidemax_sig
end
