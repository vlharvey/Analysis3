;
; Carbon Monoxide for Solar Min run (FW2)
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;IDL> restore,'cat_noaurfpl_FW2_vE.sav
;IDL> restore,'chem_noaurfpl_FW2_vE.sav
; ALT             FLOAT     = Array[96, 66, 15331]
; CH4             FLOAT     = Array[96, 66, 15331]
; CO              FLOAT     = Array[96, 66, 15331]
; DATES           FLOAT     = Array[15331]
; ILEV            DOUBLE    = Array[67]
; LAT             DOUBLE    = Array[96]
; LATDIM          LONG      =           96
; LEV             DOUBLE    = Array[66]
; LEVDIM          LONG      =           66
; NO2             FLOAT     = Array[96, 66, 15331]
; NOX             FLOAT     = Array[96, 66, 15331]
; NOY             FLOAT     = Array[96, 66, 15331]
; O3              FLOAT     = Array[96, 66, 15331]
; P               FLOAT     = Array[96, 66, 15331]
; PLEV            FLOAT     = Array[66]
;
;dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW2/'
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
;restore,dir+'cat_noaurfpl_FW2_vE.sav
restore,dir+'cat_mee00fpl_FW2_vE.sav
dates_all=dates
;restore,dir+'chem_noaurfpl_FW2_vE.sav
restore,dir+'chem_mee00fpl_FW2_vE.sav
co_all=co

dates=long(DATES_all)
nfiles=n_elements(DATES_all)
nl=n_elements(LEV)
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
    CO=reform(co_all(*,*,ifile))
;   ofile=dir+'noaurfpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_CO.sav'
    ofile=dir+'mee00fpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_CO.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE,CO
endfor			; loop over days
end
