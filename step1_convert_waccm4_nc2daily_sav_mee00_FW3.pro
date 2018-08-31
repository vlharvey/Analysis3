;
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW3
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/'
spawn,'ls '+dir+'mee00fpl_FW3NoSpin_paper1_Press_*.sav',ifiles
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATE00FPL_all=DATE00FPL
DTCORE00FPL_all=DTCORE00FPL
QRS00FPL_all=QRS00FPL
QCP00FPL_all=QCP00FPL
QRS_EUV00FPL_all=QRS_EUV00FPL
QRS_CO2NIR00FPL_all=QRS_CO2NIR00FPL
QRS_AUR00FPL_all=QRS_AUR00FPL
QTHERMAL00FPL_all=QTHERMAL00FPL
QRL00FPL_all=QRL00FPL
QRLNLTE00FPL_all=QRLNLTE00FPL

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
    DTCORE00FPL=reform(DTCORE00FPL_all(*,*,ifile))
    QCP00FPL=reform(QCP00FPL_all(*,*,ifile))
    QRL00FPL=reform(QRL00FPL_all(*,*,ifile))
    QRLNLTE00FPL=reform(QRLNLTE00FPL_all(*,*,ifile))
    QRS00FPL=reform(QRS00FPL_all(*,*,ifile))
    QRS_AUR00FPL=reform(QRS_AUR00FPL_all(*,*,ifile))
    QRS_CO2NIR00FPL=reform(QRS_CO2NIR00FPL_all(*,*,ifile))
    QRS_EUV00FPL=reform(QRS_EUV00FPL_all(*,*,ifile))
    QTHERMAL00FPL=reform(QTHERMAL00FPL_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_Qvars.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE00FPL,DTCORE00FPL,QCP00FPL,QRL00FPL,QRLNLTE00FPL,QRS00FPL,$
         QRS_AUR00FPL,QRS_CO2NIR00FPL,QRS_EUV00FPL,QTHERMAL00FPL
endfor			; loop over days
end
