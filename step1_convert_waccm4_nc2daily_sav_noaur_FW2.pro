;
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
; ALT             FLOAT     = Array[66]		; globally averaged Z3
; CLONO2NAFPL     FLOAT     = Array[96, 66]
; DATENAFPL       FLOAT     = Array[2050]	; first 1 or 2 numbers is the year, next two are month, next two are day
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[66]
; NOXNAFPL        FLOAT     = Array[96, 66]
; NOYNAFPL        FLOAT     = Array[96, 66]
; O3NAFPL         FLOAT     = Array[96, 66]
; OMEGANAFPL      FLOAT     = Array[96, 66]
; P               FLOAT     = Array[66]		; globally averaged pressure
; QSUMNAFPL       FLOAT     = Array[96, 66]
; TNAFPL          FLOAT     = Array[96, 66]
; UNAFPL          FLOAT     = Array[96, 66]
; VNAFPL          FLOAT     = Array[96, 66]
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW2/'
restore,dir+'noaurfpl_FW2NoSpin_Press.sav'
CLONO2NAFPL_all=CLONO2NAFPL
DATENAFPL_all=DATENAFPL
NOXNAFPL_all=NOXNAFPL
NOYNAFPL_all=NOYNAFPL
O3NAFPL_all=O3NAFPL
OMEGANAFPL_all=OMEGANAFPL
PRESSNAFPL_all=PRESSNAFPL
QSUMNAFPL_all=QSUMNAFPL
TNAFPL_all=TNAFPL
UNAFPL_all=UNAFPL
VNAFPL_all=VNAFPL
date=long(DATENAFPL_all)
nfiles=n_elements(DATENAFPL_all)
nl=n_elements(p)
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
    CLONO2NAFPL=reform(CLONO2NAFPL_all(*,*,ifile))
    NOXNAFPL=reform(NOXNAFPL_all(*,*,ifile))
    NOYNAFPL=reform(NOYNAFPL_all(*,*,ifile))
    O3NAFPL=reform(O3NAFPL_all(*,*,ifile))
    OMEGANAFPL=reform(OMEGANAFPL_all(*,*,ifile))
    PRESSNAFPL=reform(PRESSNAFPL_all(*,*,ifile))
    QSUMNAFPL=reform(QSUMNAFPL_all(*,*,ifile))
    TNAFPL=reform(TNAFPL_all(*,*,ifile))
    UNAFPL=reform(UNAFPL_all(*,*,ifile))
    VNAFPL=reform(VNAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'.sav'
    print,ofile
    save,file=ofile,LAT,P,LEV,ALT,DATENAFPL,CLONO2NAFPL,NOXNAFPL,NOYNAFPL,O3NAFPL,$
         OMEGANAFPL,QSUMNAFPL,TNAFPL,UNAFPL,VNAFPL
endfor			; loop over days
end
