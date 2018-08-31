;
; read WACCM4 netcdf data from Ethan Peck
; check for missing days
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
;
@stddat
@kgmt
@ckday
@kdate

dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW2/'
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
;restore,dir+'mee00fpl_FW2NoSpin_Press.sav'
;restore,dir+'cat_noaurfpl_FW2_vE.sav'
;restore,dir+'TS_noaurfpl_FW2_vE.sav'
restore,dir+'TS_mee00fpl_FW2_vE.sav'
;sdate_noaur=strcompress(long(DATENAFPL),/remove_all)
date00fpl=dates
sdate_noaur=strcompress(long(DATE00FPL),/remove_all)
index=where(long(DATE00FPL) lt 100101L)
if index(0) ne -1L then sdate_noaur(index)='0'+sdate_noaur(index)
sdate_noaur='20'+sdate_noaur
;
; loop over all days
;
lstmn=1 & lstdy=1 & lstyr=2001 & lstday=0
ledmn=12 & leddy=31 & ledyr=2042 & ledday=0     ; choose any non leap year
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
      today=where(sdate_noaur eq sdate)
      if today(0) eq -1L then print,'missing '+sdate
goto,jump
end
