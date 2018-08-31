;
; create WACCM4 mee00 yearly files
;
@stddat
@kgmt
@ckday
@kdate

dirh='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/mee00fpl_FW2.cam2.h3.'
spawn,'ls '+dirh+'*0101.sav',ifiles
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
      dum=findfile(dirh+'Year'+model_years(iyear)+'_'+sdate+'.sav')
      if dum(0) eq '' then goto,jumpday
      restore,dum(0)
      nr=n_elements(lat)
      nl=n_elements(p)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         CLONO200FPL_avg=fltarr(nr,nl,kday)
         NOX00FPL_avg=fltarr(nr,nl,kday)
         NOY00FPL_avg=fltarr(nr,nl,kday)
         O300FPL_avg=fltarr(nr,nl,kday)
         OMEGA00FPL_avg=fltarr(nr,nl,kday)
         QSUM00FPL_avg=fltarr(nr,nl,kday)
         T00FPL_avg=fltarr(nr,nl,kday)
         U00FPL_avg=fltarr(nr,nl,kday)
         V00FPL_avg=fltarr(nr,nl,kday)
      endif
;
; loop over years and retain all data
;
      print,'restored '+dum(0)
      CLONO200FPL_avg(*,*,icount)=CLONO200FPL
      NOX00FPL_avg(*,*,icount)=NOX00FPL
      NOY00FPL_avg(*,*,icount)=NOY00FPL
      O300FPL_avg(*,*,icount)=O300FPL
      OMEGA00FPL_avg(*,*,icount)=OMEGA00FPL
      QSUM00FPL_avg(*,*,icount)=QSUM00FPL
      T00FPL_avg(*,*,icount)=T00FPL
      U00FPL_avg(*,*,icount)=U00FPL
      V00FPL_avg(*,*,icount)=V00FPL
;
jumpday:
      icount=icount+1L
goto,jump
;
; save yearly file
;
saveit:
ofile=dirh+'Year'+model_years(iyear)+'_VLH.sav'
save,file=ofile,sdate_all,lat,lev,p,alt,CLONO200FPL_avg,NOX00FPL_avg,NOY00FPL_avg,O300FPL_avg,OMEGA00FPL_avg,$
     QSUM00FPL_avg,T00FPL_avg,U00FPL_avg,V00FPL_avg

endfor	; loop over years
end
