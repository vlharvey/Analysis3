;
; read WACCM4 netcdf data from Ethan Peck
; variables: QRS_LO3A QRS_LO3B QJOULE QRS_SO3B
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'
ifiles=dir+['noaurfpl_FW3NoSpin_paper1_Press_QJOULE.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
QJOULENAFPL_all=QJOULENAFPL
;
date=long(DATENAFPL_all)
nfiles=n_elements(DATENAFPL_all)
nl=n_elements(lev)
nr=n_elements(LAT)
for ifile=0L,nfiles-1L do begin
    sdate=strcompress(date,/remove_all)
    index=where(date lt 100101)
    if index(0) ne -1L then sdate(index)='0'+sdate(index)
    syear=strmid(sdate,0,2)
    smon=strmid(sdate,2,2)
    sday=strmid(sdate,4,2)
;
; strip out sday
;
    DATENAFPL=sdate(ifile)
    QJOULENAFPL=reform(QJOULENAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_QJOULE.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,QJOULENAFPL
endfor			; loop over days
end
