;
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/meeNAfpl_FW3
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3
;
; variable names for noaur run (meerun replace NA with NA) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'
spawn,'ls '+dir+'noaurfpl_FW3NoSpin_paper1_Press_*.sav',ifiles
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
DTCORENAFPL_all=DTCORENAFPL
QRSNAFPL_all=QRSNAFPL
QCPNAFPL_all=QCPNAFPL
QRS_EUVNAFPL_all=QRS_EUVNAFPL
QRS_CO2NIRNAFPL_all=QRS_CO2NIRNAFPL
QRS_AURNAFPL_all=QRS_AURNAFPL
QTHERMALNAFPL_all=QTHERMALNAFPL
QRLNAFPL_all=QRLNAFPL
QRLNLTENAFPL_all=QRLNLTENAFPL

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
    DTCORENAFPL=reform(DTCORENAFPL_all(*,*,ifile))
    QCPNAFPL=reform(QCPNAFPL_all(*,*,ifile))
    QRLNAFPL=reform(QRLNAFPL_all(*,*,ifile))
    QRLNLTENAFPL=reform(QRLNLTENAFPL_all(*,*,ifile))
    QRSNAFPL=reform(QRSNAFPL_all(*,*,ifile))
    QRS_AURNAFPL=reform(QRS_AURNAFPL_all(*,*,ifile))
    QRS_CO2NIRNAFPL=reform(QRS_CO2NIRNAFPL_all(*,*,ifile))
    QRS_EUVNAFPL=reform(QRS_EUVNAFPL_all(*,*,ifile))
    QTHERMALNAFPL=reform(QTHERMALNAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_Qvars.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,DTCORENAFPL,QCPNAFPL,QRLNAFPL,QRLNLTENAFPL,QRSNAFPL,$
         QRS_AURNAFPL,QRS_CO2NIRNAFPL,QRS_EUVNAFPL,QTHERMALNAFPL
endfor			; loop over days
end
