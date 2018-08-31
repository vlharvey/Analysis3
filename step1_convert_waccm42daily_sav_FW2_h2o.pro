;
; Water Vapor for Solar Min run (FW2)
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
restore,dir+'mee00fpl_FW2NoSpin_Press.sav
dates_all=DATE00FPL
restore,dir+'H2O_mee00fpl_FW2_vE.sav
h2o_all=h2o
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
    h2o=reform(h2o_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_H2O.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE,H2O
endfor			; loop over days
end
