;
; read WACCM3 netcdf data from Ethan Peck
; check for missing days
; /Volumes/Data/WACCM/WACCM3/noaurfco/Fco_Press_vE.sav
;
@stddat
@kgmt
@ckday
@kdate

dir='/Volumes/Data/WACCM/WACCM3/mee00fco/'	; 20250101-20810101
restore,dir+'mee00_Press_vE.sav'

;dir='/Volumes/Data/WACCM/WACCM3/noaurfco/'	; 20250101-21140102
;restore,dir+'Fco_Press_vE.sav'
sdates=strcompress(long(DATES),/remove_all)
;
; loop over all days
;
lstmn=1 & lstdy=1 & lstyr=2025 & lstday=0
ledmn=12 & leddy=31 & ledyr=2080 & ledday=0     ; choose any non leap year
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
;
; --- Loop here --------
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr
;
; test for end condition and close windows.
;
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,'starting day outside range'
      if ndays gt ledday then stop,'normal termination condition'
      syr=string(FORMAT='(i4)',iyr)
      sdy=string(FORMAT='(i2.2)',idy)
      smn=string(FORMAT='(i2.2)',imn)
      sdate=syr+smn+sdy
      if smn+sdy eq '0229' then goto,jump
;
; find day
;
;     print,sdate
      today=where(sdates eq sdate)
      if today(0) eq -1L then print,'missing '+sdate
goto,jump
end
