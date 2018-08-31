;
; read WACCM4 netcdf data from Ethan Peck
; variables: QRS_SO2A QRS_SO2B
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'
ifiles=dir+['noaurfpl_FW3NoSpin_paper1_Press_QRS_SO2A.sav','noaurfpl_FW3NoSpin_paper1_Press_QRS_SO2B.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
QRS_SO2ANAFPL_all=QRS_SO2ANAFPL
QRS_SO2BNAFPL_all=QRS_SO2BNAFPL
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
    QRS_SO2ANAFPL=reform(QRS_SO2ANAFPL_all(*,*,ifile))
    QRS_SO2BNAFPL=reform(QRS_SO2BNAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_QO2.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,QRS_SO2ANAFPL,QRS_SO2BNAFPL
endfor			; loop over days
end
