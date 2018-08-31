;
; create WACCM4 noaur yearly files
;
@stddat
@kgmt
@ckday
@kdate

dirh='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/noaurfpl_FW3.cam2.h3.'
spawn,'ls '+dirh+'*0101_Qvars.sav',ifiles
model_years=3+findgen(n_elements(ifiles))
model_years=string(FORMAT='(i2.2)',long(model_years))
nyears=n_elements(model_years)
;
; loop over model years
;
for iyear=0L,nyears-1L do begin

lstmn=1 & lstdy=1 & lstyr=1995 & lstday=0
ledmn=12 & leddy=31 & ledyr=1995 & ledday=0	; choose any non leap year
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
kday=ledday-lstday+1L
if kday ne 365L then stop,'check kday'
sdate_all=strarr(kday)
icount=0
;
; loop over days
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr
;
; test for end condition and close windows.
;
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if icount eq kday then goto,saveit
      sdy=string(FORMAT='(i2.2)',idy)
      smn=string(FORMAT='(i2.2)',imn)
      sdate=smn+sdy
      sdate_all(icount)=sdate
      if sdate eq '0229' then stop,'check leap year?'	; there is no leap day in WACCM
;
; read WACCM data
;
      dum=findfile(dirh+'Year'+model_years(iyear)+'_'+sdate+'_Qvars.sav')
      if dum(0) eq '' then goto,jumpday
      restore,dum(0)
      nr=n_elements(lat)
      nl=n_elements(lev)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         DTCORENAFPL_avg=fltarr(nr,nl,kday)
         QCPNAFPL_avg=fltarr(nr,nl,kday)
         QRLNAFPL_avg=fltarr(nr,nl,kday)
         QRLNLTENAFPL_avg=fltarr(nr,nl,kday)
         QRSNAFPL_avg=fltarr(nr,nl,kday)
         QRS_AURNAFPL_avg=fltarr(nr,nl,kday)
         QRS_CO2NIRNAFPL_avg=fltarr(nr,nl,kday)
         QRS_EUVNAFPL_avg=fltarr(nr,nl,kday)
         QTHERMALNAFPL_avg=fltarr(nr,nl,kday)
      endif
;
; loop over years and retain all data
;
      print,'restored '+dum(0)
      DTCORENAFPL_avg(*,*,icount)=DTCORENAFPL
      QCPNAFPL_avg(*,*,icount)=QCPNAFPL
      QRLNAFPL_avg(*,*,icount)=QRLNAFPL
      QRLNLTENAFPL_avg(*,*,icount)=QRLNLTENAFPL
      QRSNAFPL_avg(*,*,icount)=QRSNAFPL
      QRS_AURNAFPL_avg(*,*,icount)=QRS_AURNAFPL
      QRS_CO2NIRNAFPL_avg(*,*,icount)=QRS_CO2NIRNAFPL
      QRS_EUVNAFPL_avg(*,*,icount)=QRS_EUVNAFPL
      QTHERMALNAFPL_avg(*,*,icount)=QTHERMALNAFPL
;
jumpday:
      icount=icount+1L
goto,jump
;
; save yearly file
;
saveit:
ofile=dirh+'Year'+model_years(iyear)+'_VLH_Qvars.sav'
save,file=ofile,sdate_all,lat,lev,DTCORENAFPL_avg,QCPNAFPL_avg,QRLNAFPL_avg,QRLNLTENAFPL_avg,$
     QRSNAFPL_avg,QRS_AURNAFPL_avg,QRS_CO2NIRNAFPL_avg,QRS_EUVNAFPL_avg,QTHERMALNAFPL_avg
endfor	; loop over years
end
