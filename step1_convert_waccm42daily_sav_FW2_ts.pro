;
; Surface Temperature
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;IDL> restore,'TS_noaurfpl_FW2_vE.sav
; DATES           FLOAT     = Array[15331]
; LAT             DOUBLE    = Array[96]
; LON             DOUBLE    = Array[144]
; TS              FLOAT     = Array[144, 96, 15331]
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW2/'
restore,dir+'TS_noaurfpl_FW2_vE.sav'

;dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
;restore,dir+'TS_mee00fpl_FW2_vE.sav
dates_all=dates
ts_all=ts

dates=long(DATES_all)
nfiles=n_elements(DATES_all)
nc=n_elements(LON)
nr=n_elements(LAT)
sdate=strcompress(dates,/remove_all)
index=where(dates lt 100101)
if index(0) ne -1L then sdate(index)='0'+sdate(index)
syear=strmid(sdate,0,2)
smon=strmid(sdate,2,2)
sday=strmid(sdate,4,2)
for ifile=0L,nfiles-1L do begin
;
; strip out sday
;
    DATE=sdate(ifile)
    ts=reform(ts_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_TS.sav'
    print,ofile
    save,file=ofile,LAT,LON,DATE,TS
endfor			; loop over days
end
