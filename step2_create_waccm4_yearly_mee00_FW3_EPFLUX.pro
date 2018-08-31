;
; create WACCM4 mee00 yearly files
;
@stddat
@kgmt
@ckday
@kdate

dirh='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/mee00fpl_FW3.cam2.h3.'
spawn,'ls '+dirh+'*0101_EPFLUX.sav',ifiles
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
      dum=findfile(dirh+'Year'+model_years(iyear)+'_'+sdate+'_EPFLUX.sav')
      if dum(0) eq '' then goto,jumpday
      restore,dum(0)
      nr=n_elements(lat)
      nl=n_elements(lev)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         EPD00FPL_avg=fltarr(nr,nl,kday)
         FY00FPL_avg=fltarr(nr,nl,kday)
         FZ00FPL_avg=fltarr(nr,nl,kday)
         VRES00FPL_avg=fltarr(nr,nl,kday)
         WRES00FPL_avg=fltarr(nr,nl,kday)
      endif
;
; loop over years and retain all data
;
      print,'restored '+dum(0)
      EPD00FPL_avg(*,*,icount)=EPD00FPL
      FY00FPL_avg(*,*,icount)=FY00FPL
      FZ00FPL_avg(*,*,icount)=FZ00FPL
      VRES00FPL_avg(*,*,icount)=VRES00FPL
      WRES00FPL_avg(*,*,icount)=WRES00FPL
;
jumpday:
      icount=icount+1L
goto,jump
;
; save yearly file
;
saveit:
ofile=dirh+'Year'+model_years(iyear)+'_VLH_EPFLUX.sav'
save,file=ofile,sdate_all,lat,lev,EPD00FPL_avg,FY00FPL_avg,FZ00FPL_avg,VRES00FPL_avg,WRES00FPL_avg
endfor	; loop over years
end
