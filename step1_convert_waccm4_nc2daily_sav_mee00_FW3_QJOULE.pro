;
; read WACCM4 netcdf data from Ethan Peck
; variables: QRS_LO3A QRS_LO3B QJOULE QRS_SO3B
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/'
ifiles=dir+['mee00fpl_FW3NoSpin_paper1_Press_QJOULE.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATE00FPL_all=DATE00FPL
QJOULE00FPL_all=QJOULE00FPL
;
date=long(DATE00FPL_all)
nfiles=n_elements(DATE00FPL_all)
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
    DATE00FPL=sdate(ifile)
    QJOULE00FPL=reform(QJOULE00FPL_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_QJOULE.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE00FPL,QJOULE00FPL
endfor			; loop over days
end
