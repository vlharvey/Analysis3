;
; read WACCM4 netcdf data from Ethan Peck
; variables: QRS_LO3A QRS_LO3B QRS_SO3A QRS_SO3B
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'
ifiles=dir+['noaurfpl_FW3NoSpin_paper1_Press_QRS_LO3A.sav','noaurfpl_FW3NoSpin_paper1_Press_QRS_LO3B.sav',$
            'noaurfpl_FW3NoSpin_paper1_Press_QRS_SO3A.sav','noaurfpl_FW3NoSpin_paper1_Press_QRS_SO3B.sav',$
            'noaurfpl_FW3NoSpin_paper1_Press_QRS_LO3.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
QRS_SO3ANAFPL_all=QRS_SO3ANAFPL
QRS_SO3BNAFPL_all=QRS_SO3BNAFPL
QRS_LO3ANAFPL_all=QRS_LO3ANAFPL
QRS_LO3BNAFPL_all=QRS_LO3BNAFPL
QRS_LO3NAFPL_all=QRS_LO3NAFPL
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
    QRS_SO3ANAFPL=reform(QRS_SO3ANAFPL_all(*,*,ifile))
    QRS_SO3BNAFPL=reform(QRS_SO3BNAFPL_all(*,*,ifile))
    QRS_LO3ANAFPL=reform(QRS_LO3ANAFPL_all(*,*,ifile))
    QRS_LO3BNAFPL=reform(QRS_LO3BNAFPL_all(*,*,ifile))
    QRS_LO3NAFPL=reform(QRS_LO3NAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_QO3.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,QRS_SO3ANAFPL,QRS_SO3BNAFPL,QRS_LO3ANAFPL,QRS_LO3BNAFPL,QRS_LO3NAFPL
endfor			; loop over days
end
